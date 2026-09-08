#!/bin/bash
#
# The weekly run: sync, commit, push. Driven by the launchd agent beside this
# file, and safe to run by hand.
#
# This exists rather than a GitHub Action because the league site sits behind
# Cloudflare, which returns 403 to datacentre addresses — the identical request
# succeeds from a home connection and fails from a hosted runner. So the sync
# runs where a browser would.

set -uo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo" || exit 1

# launchd starts with almost no PATH, so git and R have to be findable.
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin"

log="$repo/raw/sync.log"
mkdir -p "$(dirname "$log")"

# A cron nobody watches fails silently, which is the same as not running.
notify() {
  osascript -e "display notification \"$2\" with title \"$1\"" >/dev/null 2>&1 || true
}

exec >>"$log" 2>&1
echo
echo "=== $(date '+%Y-%m-%d %H:%M:%S %Z') ==="

if ! Rscript scripts/sync.R --write; then
  echo "FAILED: the sync did not complete."
  notify "Borulanta sync failed" "See raw/sync.log"
  exit 1
fi

# Named paths throughout, and no `git add`: an unattended job must not sweep up
# whatever else happens to be staged or dirty and push it under this message.
# `git commit -- <paths>` commits those paths and leaves the index alone.
synced=(data/matches.csv data/match_events.csv)
if git diff --quiet HEAD -- "${synced[@]}"; then
  echo "Nothing new to commit."
  exit 0
fi

git commit -m "Sync results from Dream Leagues

Attendance for any new match still needs adding by hand." -- "${synced[@]}" || exit 1

# Somebody may have pushed from elsewhere since the last run.
if ! git pull --rebase --autostash || ! git push; then
  echo "FAILED: committed locally but could not push."
  notify "Borulanta sync" "Committed, but the push failed. See raw/sync.log"
  exit 1
fi

echo "Committed and pushed."
notify "Borulanta" "New results synced. Attendance still needs adding."
