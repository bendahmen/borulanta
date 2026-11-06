#!/bin/bash
#
# The static site's weekly run in one command: export, test, commit, push. Run it by
# hand from a terminal, or have an agent run it, once the week's attendance is in.
#
#   bash scripts/publish-site.sh            publish
#   bash scripts/publish-site.sh --dry-run  say what a publish would do, change nothing
#
# It runs in one of two places, and the branch says which. Until cutover that is
# borulanta-site on static-site, which merges main first because the CSVs are kept on
# main. After cutover it is the main folder on main, which has nothing to merge.
#
# It keeps no log and sends no notification, unlike sync-and-commit.sh. Somebody is
# watching this one run, and a publish leaves a commit behind as its record.

set -uo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo" || exit 1

# Not inherited when this is invoked from anywhere with a thin environment.
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:$PATH"

# An agent may be running this with nobody to answer, so git fails rather than asks.
export GIT_TERMINAL_PROMPT=0
export GIT_EDITOR=:

dry_run=false
for arg in "$@"; do
  case "$arg" in
    --dry-run) dry_run=true ;;
    *)
      echo "Usage: bash scripts/publish-site.sh [--dry-run]" >&2
      exit 2
      ;;
  esac
done

# The two folders the export owns, and the only things this script ever commits.
generated=(site/src/generated site/public/data/seasons)

# What the export and its tests read besides the CSVs.
export_code=(R scripts/build-site-data.R tests/testthat/test-site-export.R
             tests/testthat/helper-borulanta.R)

start_head="$(git rev-parse HEAD)" || exit 1
json_rewritten=false
merge_commit=""
pulling=false
preview=""
committed=false
merged=false

# A run that stops leaves the checkout as it found it, so the next run starts from the
# same place and history gets one merge per publish rather than one per attempt. Step 1
# proved the generated folders clean, so restoring them cannot lose anybody's work.
leave_as_found() {
  # A second Ctrl-C must not stop this halfway, for example with a CSV put back and
  # staged under a merge that stays.
  trap '' INT TERM
  if [ -n "$preview" ]; then
    rm -rf "$preview"
    preview=""
  fi
  if "$pulling" && [ -d "$(git rev-parse --git-path rebase-merge)" ]; then
    git rebase --abort || echo "WARNING: could not abort the pull's rebase. See git status."
  fi
  pulling=false
  if "$json_rewritten"; then
    git restore --source=HEAD --staged --worktree -- "${generated[@]}"
    git clean -fdq -- "${generated[@]}"
    json_rewritten=false
  fi
  if [ -n "$merge_commit" ]; then
    # Another session may have committed here while the tests ran. Its commit matters
    # more than a tidy history, so the merge then stays.
    if [ "$(git rev-parse HEAD)" = "$merge_commit" ]; then
      git reset -q --keep "$start_head" ||
        echo "WARNING: could not undo the merge of main. It is local and unpushed."
    else
      echo "WARNING: HEAD moved during this run, so the merge of main stays, local and unpushed."
    fi
    merge_commit=""
  fi
}
trap leave_as_found EXIT
trap 'exit 130' INT TERM

stop_at() {
  local step="$1"
  local made_merge="$merge_commit"
  shift
  leave_as_found
  echo
  echo "FAILED at $step."
  printf '%s\n' "$@"
  if [ -n "$made_merge" ] && [ "$(git rev-parse HEAD)" = "$start_head" ]; then
    echo "The merge of main from this run was undone, and the next run will merge it again."
  fi
  echo "Nothing was published."
  exit 1
}

# ---- Step 1: checks ----
echo "--- Step 1: checks"

# Before the branch check, because a rebase leaves HEAD detached and "wrong branch"
# would be the wrong thing to say.
for marker in MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD rebase-merge rebase-apply; do
  if [ -e "$(git rev-parse --git-path "$marker")" ]; then
    stop_at "step 1 (checks)" "This checkout is in the middle of a merge or rebase ($marker)." \
      "Finish or abort that first."
  fi
done

# A stash that failed to apply leaves these without any of the markers above.
unresolved="$(git diff --name-only --diff-filter=U)"
if [ -n "$unresolved" ]; then
  stop_at "step 1 (checks)" "These files have unresolved conflicts:" "$unresolved" \
    "Resolve them first."
fi

branch="$(git branch --show-current)"
case "$branch" in
  static-site) merge_first=true ;;
  main) merge_first=false ;;
  *)
    stop_at "step 1 (checks)" "This runs on static-site until cutover and on main after it." \
      "This checkout is on '${branch:-a detached HEAD}'."
    ;;
esac

# Untracked files count too, because git add below would sweep them in.
generated_dirty="$(git status --porcelain -uall -- "${generated[@]}")"
if [ -n "$generated_dirty" ]; then
  stop_at "step 1 (checks)" "The generated folders already have uncommitted changes:" \
    "$generated_dirty" \
    "The export would overwrite them or mix them into the commit. Commit or discard them first."
fi

# Other dirty files are left alone, except what the export reads, because it reads the
# working tree. JSON built from an uncommitted CSV has a data_hash that matches no commit,
# and the host's build would fail on it. Hidden files are skipped because data_hash
# skips them, such as the lock file of a spreadsheet left open.
csv_dirty="$(git status --porcelain -uall -- 'data/*.csv' | grep -v '/\.[^/]*$')"
if [ -n "$csv_dirty" ]; then
  stop_at "step 1 (checks)" "These CSVs have uncommitted changes:" \
    "$csv_dirty" \
    "The JSON would describe rows that are not in any commit, and the host's build would fail." \
    "Commit them on main in the main folder first."
fi

# Code is worse, because data_hash covers only the CSVs. JSON from an uncommitted fee rule
# would go out under a commit whose R/fees.R says something else, and nothing downstream
# could notice. The tests are here because they are what passes that JSON.
code_dirty="$(git status --porcelain -uno -- "${export_code[@]}")"
if [ -n "$code_dirty" ]; then
  stop_at "step 1 (checks)" "The export's code has uncommitted changes:" \
    "$code_dirty" \
    "The JSON would come from code that is in no commit. Commit or discard them first."
fi

# ---- Step 2: merge ----
echo "--- Step 2: merge"

unmerged=0
if "$merge_first"; then
  unmerged="$(git rev-list --count HEAD..main)" ||
    stop_at "step 2 (merge)" "Could not compare this branch with main."
  if [ "$unmerged" -eq 0 ]; then
    echo "Nothing on main to merge."
  else
    # git merge will not run over a staged change, so a publish stops here rather than
    # move it aside and risk not getting it back.
    staged="$(git diff --cached --name-only)"
    if [ -n "$staged" ]; then
      stop_at "step 2 (merge)" "main has $unmerged commit(s) to merge, and these files are staged:" \
        "$staged" \
        "git merge will not run over staged changes. Commit or unstage them first."
    fi
    if "$dry_run"; then
      # A dry run does not merge. Merging and undoing would show the true data, but a run
      # that died halfway would leave the branch moved, which a dry run must never do.
      # merge-tree says whether the merge would work without touching the checkout.
      echo "Not merging, because this is a dry run. main has $unmerged commit(s) this branch lacks:"
      git log --oneline HEAD..main
      if ! git merge-tree --write-tree --quiet HEAD main; then
        stop_at "step 2 (merge)" "Merging main would hit a conflict, so a publish would stop here." \
          "This was a dry run: nothing was merged."
      fi
      echo "Merging them would work. The export below uses this branch's CSVs, so it may be behind main."
    elif git merge --no-edit main; then
      merge_commit="$(git rev-parse HEAD)"
      merged=true
    elif [ -e "$(git rev-parse --git-path MERGE_HEAD)" ]; then
      conflicted="$(git diff --name-only --diff-filter=U | tr '\n' ' ')"
      conflicted="${conflicted% }"
      git merge --abort
      stop_at "step 2 (merge)" "git merge main hit a conflict in: $conflicted" \
        "The merge was aborted and the tree is as it was. Resolve it by merging main by hand."
    else
      stop_at "step 2 (merge)" "git merge main would not start: its message is above." \
        "The usual cause is a local change to a file main also changes."
    fi
  fi
else
  echo "On main, so there is nothing to merge."
fi

# ---- Step 3: attendance guard ----
echo "--- Step 3: attendance guard"

# Only lines that are dates count, because renv can write to stdout as it starts.
awaiting="$(Rscript scripts/build-site-data.R --matches-without-attendance)" ||
  stop_at "step 3 (attendance guard)" "R could not read the match and attendance files: see above."
awaiting="$(printf '%s\n' "$awaiting" | grep -E '^[0-9]{2}/[0-9]{2}/[0-9]{4}$' | tr '\n' ' ')"
awaiting="${awaiting% }"

if [ -n "$awaiting" ]; then
  stop_at "step 3 (attendance guard)" "No attendance on file for: $awaiting" \
    "Until it is in, the fees for those matches are not right, so the balances would be wrong." \
    "The pages for those matches would also say no lineup." \
    "Type attendance in data/attendance.csv in the main folder, commit it on main, and rerun."
fi
echo "Every match has attendance."
if [ "$unmerged" -gt 0 ] && "$dry_run"; then
  echo "That is this branch's matches. A match on main that is not merged here was not checked."
fi

# ---- Step 4: export ----
echo "--- Step 4: export"

# The export stops before writing anything on a repeated match date or attendance row.
# A dry run writes to a temporary folder, so that even a killed one leaves the committed
# JSON alone.
if "$dry_run"; then
  preview="$(mktemp -d)" || stop_at "step 4 (export)" "Could not make a temporary folder."
  Rscript -e 'source("scripts/build-site-data.R")
    built <- build_site_data(out_root = commandArgs(trailingOnly = TRUE)[[1]])
    message("Wrote ", length(built$files), " files to a temporary folder.")' "$preview/site"
else
  json_rewritten=true
  Rscript scripts/build-site-data.R
fi || stop_at "step 4 (export)" "The export stopped: R's message is above, naming any rows to fix." \
  "Fix them in the main folder, commit on main, and run this again."

# ---- Step 5: export tests ----
echo "--- Step 5: export tests"

# stop_on_failure is what carries a failure to the exit status. Without it test_file()
# reports the failure and Rscript still exits 0.
test_args='"tests/testthat/test-site-export.R", reporter = "summary", stop_on_failure = TRUE'
Rscript -e "testthat::test_file($test_args)" ||
  stop_at "step 5 (export tests)" "The export tests did not pass: see above." \
    "The committed JSON is unchanged. To see the new JSON, run Rscript scripts/build-site-data.R."

# ---- Step 6: commit ----
echo "--- Step 6: commit"

if "$dry_run"; then
  # Step 1 proved the generated folders match HEAD, so the files on disk stand for it.
  changes="$(
    {
      git ls-files -- "${generated[@]}"
      (cd "$preview" && find "${generated[@]}" -type f)
    } | sort -u | while read -r path; do
      if [ ! -f "$preview/$path" ]; then
        echo " D $path"
      elif [ ! -f "$path" ]; then
        echo "?? $path"
      elif ! cmp -s "$path" "$preview/$path"; then
        echo " M $path"
      fi
    done
  )"
  if [ -z "$changes" ]; then
    echo "The export changes no generated file."
  else
    echo "A publish would commit these generated files:"
    printf '%s\n' "$changes"
  fi
  if [ "$unmerged" -gt 0 ]; then
    echo "That is from this branch's CSVs. A publish merges main first, so it may change more."
  fi
  upstream="$(git rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null)" || upstream=""
  if [ -z "$upstream" ]; then
    echo "A publish would push $branch for the first time and set its upstream."
  else
    waiting="$(git rev-list --count "$upstream..HEAD")"
    echo "As of the last fetch, this branch has $waiting local commit(s) that $upstream lacks."
    git log --oneline "$upstream..HEAD"
  fi
  echo
  echo "Dry run: nothing committed or pushed, and the committed JSON was not touched."
  exit 0
fi

changes="$(git status --porcelain -uall -- "${generated[@]}")"
if [ -n "$changes" ]; then
  # Named paths throughout, as in sync-and-commit.sh: `git commit -- <paths>` commits
  # those paths and leaves the rest of the index alone. The add is scoped to the same two
  # folders, and is there because a new season's files are untracked and a commit by
  # path would not see them. A removed file needs nothing more than the commit.
  git add -A -- "${generated[@]}" &&
    git commit -q -m "Regenerate the site's JSON from the CSVs

Written by scripts/publish-site.sh." -- "${generated[@]}" ||
    stop_at "step 6 (commit)" "git could not commit the generated files: its message is above."
  committed=true
  echo "Committed $(git log -1 --format='%h "%s"'):"
  printf '%s\n' "$changes"
else
  echo "Nothing new to publish: the export changed no generated file."
fi
# From here on what has been done is kept, and a failure says what is local.
json_rewritten=false
merge_commit=""

# ---- Step 7: push ----
echo "--- Step 7: push"

push_failed() {
  echo
  echo "FAILED at step 7 (push): $1"
  if "$committed"; then
    echo "Committed locally but did not push, so the site has not changed."
  elif "$merged"; then
    echo "Merged main locally but did not push."
  fi
  echo "Run this again once that is fixed. A run with nothing new still pushes what is waiting."
  exit 1
}

upstream="$(git rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null)" || upstream=""

if [ -z "$upstream" ]; then
  # Until cutover static-site has never been pushed, so the first run sets its upstream,
  # whether or not it has anything new. Nothing can be deployed from it until then.
  echo "$branch has no upstream yet, so this is its first push."
  git push -u origin "$branch" || push_failed "git push -u origin $branch did not work."
else
  git fetch -q || push_failed "git fetch could not reach origin."

  if [ "$(git rev-list --count "HEAD..$upstream")" -gt 0 ]; then
    # Somebody has pushed from elsewhere since the last run, which can only matter after
    # cutover. A pull over uncommitted changes can leave them half-applied, so this one
    # stops and leaves the pull to a person.
    if [ -n "$(git status --porcelain -uno)" ]; then
      push_failed "$upstream has moved and this checkout has uncommitted changes.
Commit or discard them, run git pull --rebase=merges by hand, then run this again."
    fi
    # --rebase=merges because plain --rebase would flatten the merge of main into copies
    # of main's commits on this branch, and the next merge of main would bring them again.
    data_before_pull="$(git rev-parse HEAD:data)"
    pulling=true
    if ! git pull --rebase=merges; then
      leave_as_found
      push_failed "git pull could not apply this branch on top of $upstream.
Run git pull --rebase=merges by hand, then run this again."
    fi
    pulling=false
    # New CSVs mean the JSON committed above is out of date, and its data_hash would fail
    # the host's build. Running again rebuilds it from what was pulled.
    if [ "$(git rev-parse HEAD:data)" != "$data_before_pull" ]; then
      push_failed "That pull brought new CSVs, so the JSON here does not describe them."
    fi
  fi

  # What a publish owns: a merge, or a commit of the JSON, whether from this run or from
  # one whose push failed. Other local commits are not a reason to push. They do go along
  # when one of these goes, because a push sends the whole branch.
  to_publish="$(
    git rev-list --merges "$upstream..HEAD"
    git rev-list "$upstream..HEAD" -- "${generated[@]}"
  )"
  if [ -z "$to_publish" ]; then
    other_commits="$(git rev-list --count "$upstream..HEAD")"
    if [ "$other_commits" -gt 0 ]; then
      echo "Not pushing $other_commits local commit(s) that are neither a merge nor the JSON."
    fi
    echo "Nothing new to publish."
    exit 0
  fi

  echo "Pushing to $upstream:"
  git log --oneline "$upstream..HEAD"
  git push || push_failed "git push did not work."
fi

echo
echo "Published: $branch is on origin."
