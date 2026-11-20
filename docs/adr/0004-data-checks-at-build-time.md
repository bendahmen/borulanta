# The data is checked at build time, and two problems stop the export

A static site keeps whatever numbers it was last built with, so two checks stop it publishing wrong ones.

First, the export writes `data_hash`, a hash of every CSV in `data/` and `data/archive/`, and the Astro build recomputes it and fails on a mismatch. A CSV committed without regenerated JSON would otherwise publish old numbers under new CSVs. The archive is included because the frozen ledgers feed the balances. The check uses only Node, in `site/src/lib/data-hash.js`, so it runs on Cloudflare, and a failed build leaves the previous version live.

Second, a match date on file twice, or a player down twice for one night, stops the export: it names the rows and the file to fix, and writes nothing. Every other problem `validate_app_data()` finds goes to the quiet banner on every page, as it did in Shiny. Ben decided on 4 November 2026 that correct fees come first. In the test that prompted it, a repeated last match published £719.56 outstanding instead of £491.55, under a banner nobody would act on. The old JSON then fails the hash check, so the last good version stays live.

## Consequences

The stop is matched on the start of `validate_app_data()`'s wording for those two problems, so that wording is data. Rewording it would silence the stop, and the export tests guard against that.
