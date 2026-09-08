#!/bin/bash
#
# The weekly run in one command: sync, commit, push. Run it by hand, from a
# terminal, when you sit down to add the week's attendance.
#
# It is not scheduled, and both of the obvious ways to schedule it are closed.
# A GitHub Action cannot fetch the page: the league site sits behind Cloudflare,
# which returns 403 to datacentre addresses while serving the identical request
# happily to a home connection. And a launchd agent on this Mac cannot reach the
# repository: it lives under ~/Library/CloudStorage, which macOS shields from
# scheduled jobs, so bash cannot so much as read this file from one.

set -uo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo" || exit 1

# Not inherited when this is invoked from anywhere with a thin environment.
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:$PATH"

log="$repo/raw/sync.log"
mkdir -p "$(dirname "$log")"

# Worth a nudge even when run by hand: the interesting output is in the log,
# and the run is quiet either way.
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
