#!/bin/bash
#
# Keep the static site's generated folders out of Dropbox, then install packages.
#
#   bash scripts/site-setup.sh           mark the folders, then run npm ci
#   bash scripts/site-setup.sh --mark    mark the folders only
#   bash scripts/site-setup.sh --check   fail if a folder is not marked
#
# The mark belongs to the folder, so deleting a folder loses it: npm ci keeps
# node_modules and keeps the mark, but rm -rf does not. Mark before installing,
# because Dropbox will not ignore a folder that already holds online-only files.
# Outside Dropbox, including on a build host, the script marks nothing.

set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
flag='com.apple.fileprovider.ignore#P'
folders=(node_modules dist .astro .wrangler)
mode="${1:-install}"

in_dropbox() {
  [[ "$root" == *"/Library/CloudStorage/Dropbox/"* ]]
}

mark() {
  in_dropbox || return 0
  for folder in "${folders[@]}"; do
    mkdir -p "$root/site/$folder"
    xattr -w "$flag" 1 "$root/site/$folder"
  done
}

check() {
  in_dropbox || return 0
  local missing=0
  for folder in "${folders[@]}"; do
    if [[ "$(xattr -p "$flag" "$root/site/$folder" 2>/dev/null)" != "1" ]]; then
      echo "Dropbox can sync site/$folder: run bash scripts/site-setup.sh --mark" >&2
      missing=1
    fi
  done
  return "$missing"
}

case "$mode" in
  --mark) mark ;;
  --check) check ;;
  install)
    mark
    if [[ -f "$root/site/package-lock.json" ]]; then
      (cd "$root/site" && npm ci)
    fi
    check
    ;;
  *)
    echo "Usage: bash scripts/site-setup.sh [--mark|--check]" >&2
    exit 2
    ;;
esac
