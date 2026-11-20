# The data is checked at build time

A static site keeps whatever numbers it was last built with, so two checks stop it publishing wrong ones, and a failed build or a stopped export leaves the last good version live. First, the export writes `data_hash`, a hash of every CSV in `data/` and `data/archive/`, and the Astro build recomputes it with Node alone and fails on a mismatch, so JSON exported from other CSVs cannot publish. Second, a match date on file twice, or a player down twice for one night, stops the export before it writes anything, because both make the charges wrong.

## Considered options

Shiny showed every data problem in a quiet banner, and the site still does for every other problem. We stopped on these two on 4 November 2026, after a test in which a repeated last match published £719.56 outstanding instead of £491.55 under that banner.

## Consequences

The stop matches the start of `validate_app_data()`'s wording for those two problems, so rewording them would silence it. The export tests guard against that.
