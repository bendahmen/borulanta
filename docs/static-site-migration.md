# Static site migration plan

Written 4 November 2026. Read this whole file before doing anything, then read `README.md`.

## The goal

We are replacing the Shiny front end with a static site because design and usability matter most and nothing needs a backend. R stays the only source of numbers. Each week R recomputes the statistics on Ben's Mac and writes JSON, and an Astro site renders that JSON.

The prototype decides whether the rest happens. It covers Home and Players > Effects, and Ben judges it on a real phone at Stage 2. Do not start Stage 3 without his yes. If he says no, we delete the branch and Shiny has lost nothing.

## Decisions already made

Do not reopen these without new evidence.

| Topic | Decision |
|---|---|
| Framework | Astro, with Svelte only for interactive pieces |
| Charts | Observable Plot, drawn in the browser |
| Hosting | Cloudflare, connected at Stage 5. The prototype runs locally |
| Source of truth | CSVs in `data/`, edited by hand and validated by R |
| Statistics and fee rules | Stay in R. No regression or fee arithmetic in JS |
| Backend | None. No logins, database, functions or writes in this migration |
| The sync | Stays local and manual. The bot check on the league site blocks automation |
| Shiny | Stays live and untouched on `main` until cutover |

The debate behind these rejected Quarto, Django and Supabase. webR is also out: its smallest useful setup downloads about 29 MB.

## Repo logistics

### Folders and branches

There are two checkouts of one repository, both in Dropbox.

| Folder | Branch | Used for |
|---|---|---|
| `projects/borulanta` | `main` | Shiny, the weekly sync, attendance and payments |
| `projects/borulanta-site` | `static-site` | All migration work |

This separation is deliberate. `scripts/sync-and-commit.sh` commits to whichever branch is checked out and then pushes. On a shared checkout, a weekly sync run during migration work would put results on `static-site`, and they would never reach Shiny. Other sessions also add payments and attendance in the main folder, so that folder must stay on `main`.

Work only in `borulanta-site`. Never run the sync or add match data from there. To pick up new data, run `git merge main` on `static-site` and regenerate the JSON.

Pushing `main` redeploys Shiny. Pushing `static-site` does not.

### Permissions

Ben's standing rule is to ask before any commit or push. His kickoff prompt at the end of this file may grant commits on `static-site`. Pushing, or changing `main`, still needs a direct yes.

### Dropbox

Dropbox must not sync the installed packages. A test install of Astro, Svelte and Plot came to 10,886 files and 174 MB, against about 1,050 files for the repo today.

`scripts/site-setup.sh` handles this by marking `site/node_modules`, `site/dist`, `site/.astro` and `site/.wrangler` with Dropbox's ignore flag. I tested three cases: `npm ci` and `npm install` keep the flag, while deleting the folder loses it. So follow these rules.

1. Always install with `bash scripts/site-setup.sh`, which marks the folders before installing.
2. Add `predev` and `prebuild` entries to `site/package.json` that run `bash ../scripts/site-setup.sh --mark`. The script does nothing outside Dropbox, so the Cloudflare build is unaffected.
3. After the first `npm run build`, run `bash scripts/site-setup.sh --check`. I could not test whether Astro's build keeps the flag on `dist`, so this check settles it.
4. If you add another generated folder under `site/`, add it to `.gitignore` and to `folders` in the script.
5. At cutover, run the script again in the main folder, because the flags belong to folders and do not travel through git.

Do not symlink `node_modules` out of Dropbox.

### Other checkout notes

- `.gitignore` on this branch already covers those four folders.
- `.claude/launch.json` needs a `borulanta-site` entry for the dev server on port 4322, once `site/` exists. Keep the Shiny entry.
- This worktree stores absolute paths. Use it only on this Mac until it is merged.
- If R reports missing packages here, run `renv::restore()`. It links from the shared renv cache in seconds.
- `scripts/write-manifest.R` ships everything in `R/` except the sync files. Put export code in `scripts/`, not `R/`, so Shiny's bundle stays as it is if the manifest is regenerated.
- `jsonlite` is already in `renv.lock`. Do not add R packages without asking, because `renv.lock` and `manifest.json` must change together.
- Astro 7.4.5 needs Node 22.23.4 or later. Homebrew has Node 25.2.1 here, which is not a long-term support release. Pin one version in `site/.nvmrc` and `engines`, and use the same one on Cloudflare.

## Architecture

```
data/*.csv                         hand-edited truth, unchanged
scripts/build-site-data.R          sources R/, writes JSON, adds no statistics
site/src/generated/*.json          read at build time for pages with no season picker
site/public/data/seasons/*.json    one file per season combination, fetched when ticks change
site/src/                          Astro pages, Svelte components
```

Generated JSON is committed. That gives a readable diff of what changed each week, and the host can build without R.

Reuse the existing functions in `R/analysis.R`, `R/fees.R`, `R/data.R`, `R/seasons.R` and `R/format.R`. If the export needs something new, add a small function and a test rather than writing the logic twice.

### Things that will go wrong unless handled

**Today's date.** Shiny computes the next match, run-in, countdown and stale warning on each visit: see `upcoming_fixtures()`, `next_fixture()`, `run_in()` and `stale_fixture_note()`. A static build freezes those answers. So export every unplayed fixture with its standings columns, for example by calling `run_in()` with a date far in the past. Then filter in the browser on the Europe/London date. This small piece of date logic is the one permitted duplicate, and it needs a unit test.

**Season combinations.** Any combination of seasons can be ticked and every statistic follows it. Precompute every non-empty subset, named by sorted ids, such as `s1-s2.json`. Three seasons give 7 files. Earlier runs of 7 subsets x 2 estimators x 4 outcomes took about 2 seconds, at 2-5 KB gzipped per subset for regression output alone. The count doubles with each season. Around season 10, about 2030, we would limit it to consecutive seasons. Record that in a comment beside the loop. Do not build it now.

**Empty results.** OLS returns no rows for season 3 alone, and a window can have no matches. Keep the current empty states and wording from `app.R` rather than showing zeros.

**Numbers on the page.** Export the value and R's formatted string for money and estimates. `format_estimate()` and `format_fee_amount()` stay the only formatters of those. JS sorts on the value and shows the string.

**Reproducibility.** Output must be byte-identical across two runs on the same data. `RIDGE_SEED` handles the ridge folds. Sort every array, fix numeric precision, and write no build timestamps. `scraped_on` and the last match date provide the freshness note.

**Stale deploys.** A CSV commit without regenerated JSON would silently publish old numbers. Put a hash of `data/*.csv` in the export, and make the Astro build fail if it does not match the CSVs. The check needs only Node, so it runs on the host. A failed build keeps the previous version live.

**Data problems.** `validate_app_data()` returns problems that Shiny shows in a quiet banner. Export them and show the same banner. There is one exception: a repeated match date or a repeated attendance row makes the fees wrong, so the export stops, names the rows and writes nothing. The old JSON then fails the hash check, so the last good version stays live.

**Search engines.** Balances are currently public to anyone with the link. Add `noindex` and a disallowing `robots.txt` from the start, and leave the wider privacy question to Ben below.

## Design brief

Most visitors arrive on a phone from a WhatsApp link. They want to know who is next, what happened and what they owe. Build for a 375 px width first and treat desktop as the adaptation.

Do not port the cards one by one. The tab structure is a reasonable starting point, but the job is to answer those questions faster. Reorganise where that helps, and tell Ben what moved and why.

These are the requirements.

1. Tables must work on a phone without sideways scrolling for the main columns. Prefer fewer columns, stacked rows or a row that expands.
2. Keep the identity: the crest, the colours in `APP_COLOURS` and the Barlow and Barlow Condensed fonts, which should be self-hosted. Resize the crest, currently 597 KB, to under 30 KB.
3. Keep the chosen player and season ticks in `localStorage` wrapped in try/catch, and put the season ticks in the URL. A returning player should land on their own balance.
4. Keep the careful statistical wording in `R/cards.R`, such as the with-and-without note and the regression explanation. It says what the numbers do not show, and that is deliberate. Shorten it only with Ben's agreement.
5. Season-scoped pages show the picker. Home, Fees and League do not. `SEASON_SCOPED_TABS` in `app.R` records the rule.
6. Meet accessible contrast, label every control, and support light and dark.

Performance targets are below, measured with 4x CPU slowdown on slow 4G.

| Page | First view | JS, gzipped |
|---|---|---|
| Home | Under 1.5 s | Under 50 KB |
| Players > Effects | Under 2.5 s to the plot | Under 150 KB |

Keep dependencies few: Astro, Svelte, its Astro integration and Observable Plot. Write the sortable table rather than importing a table library. Self-host Plot through the build, never from a CDN.

## Stages

Finish each stage's checks before moving on. Show evidence rather than saying it works.

### Stage 0: confirm setup

Already prepared and tested on 4 November 2026: this worktree, the `.gitignore` entries, `scripts/site-setup.sh`, the Dropbox flags and `renv::restore()`. The existing test suite ran here with no failures.

- [x] `git branch --show-current` prints `static-site`, and `git worktree list` shows both folders.
- [x] `bash scripts/site-setup.sh --check` exits 0.
- [x] `Rscript -e 'renv::status()'` reports no issues. If not, run `renv::restore()`.
- [x] Commit this file, `.gitignore` and the setup script to `static-site`.

### Stage 1: data export

Write `scripts/build-site-data.R` and `tests/testthat/test-site-export.R`.

- [x] Every subset file exists, with both estimators and all four outcomes.
- [x] Exported balances equal `all_player_balances()` to the penny, and regression rows equal `player_regression_results()`.
- [x] Two runs produce byte-identical output.
- [x] Run the existing test suite. It must pass as it did before.
- [x] Report the file count and gzipped sizes. If a subset exceeds about 50 KB gzipped, split the player profiles out.

### Stage 2: prototype

Scaffold `site/`, then build the shared shell and navigation, Home and Players > Effects. Effects is included because it is the hardest page: the picker, the estimator switch, the outcome switch, plots and the table.

- [x] Run `bash scripts/site-setup.sh`, then `--check`, including after the first build.
- [x] Open both pages in the built-in browser at 375 px and on desktop, in light and dark, and check the console for errors.
- [x] Every number on the two pages matches the live Shiny app for at least three season combinations, including the empty OLS case.
- [x] Tell Ben how to open it on his phone with `npm run dev -- --host` over shared wifi.
- [x] Report the measured performance against the targets, and give Ben the list of decisions below that affect the prototype.

**Stop here.** Ben compares the prototype with the live site on his phone and decides. Iterate on design as he asks. Do not start Stage 3 without his yes.

### Stage 3: remaining pages

Build Fees (You, Everyone), League (table, run-in), Matches (Results, Trends) and Players (Roster and profile). Add shareable `/players/<slug>/` and `/matches/<date>/` pages. Each page gets its own title and description, which also sets up link previews.

- [x] Walk through `README.md` and every output in `app.R`, and list each feature as kept, changed or dropped. Ben approves anything dropped. The list is in `docs/static-site-features.md`.
- [x] All numbers match Shiny across all season combinations, by a script rather than by eye where practical.
- [x] Every balance and the outstanding and credit totals match to the penny.

### Stage 4: weekly routine

Write `scripts/publish-site.sh`. It runs the export, runs the export tests, commits the generated files by named path, and pushes. Follow the same pathspec rule as `sync-and-commit.sh`, so that nothing else gets swept up.

The routine has to cover what really happens: the sync comes first, attendance gets typed afterwards, and payments arrive on any day. Document that in `README.md`, including for agents adding a row, and test it once on a real week.

### Stage 5: hosting

Ben has to create the Cloudflare account and connect the GitHub app, because an agent should not. Check Cloudflare's current setup docs at that point. It recommends Workers static assets for new projects, and the details change.

- [x] Root `site/`, build `npm ci && npm run build`, output `dist`, Node version pinned.
- [x] Deploy from `static-site` for now, to a default Cloudflare address. Live at https://borulanta.bendahmen.workers.dev/.
- [x] The stale-deploy check fails a build when the JSON is out of date. Test it on purpose.
- [x] Ben tests the deployed site on his phone, including how a WhatsApp link previews.

The Worker is called `borulanta`, matching `site/wrangler.jsonc`, which serves `dist` as static assets with the site's own 404 page. Its build settings in the dashboard are: root directory `site`, build command `npm ci && npm run build`, deploy command `npx wrangler@4.158.0 deploy`, and the build variable `SKIP_DEPENDENCY_INSTALL=true` so that `npm ci` runs once. The production branch is set under Settings → Build → Branch control, because the first build used the default branch, `main`, and failed for want of `site/`. The dashboard has no button to start a build, so a push starts one.

### Stage 6: cutover

Only with Ben's go-ahead.

- [ ] Merge `static-site` into `main`, and switch Cloudflare's production branch to `main`.
- [ ] Run `bash scripts/site-setup.sh` in the main folder.
- [ ] Add a notice to the Shiny app linking to the new address. Connect Cloud offers no redirect we know of, so check this.
- [ ] Ben posts the new link in the WhatsApp group.
- [ ] Keep Shiny live for about four weeks and watch its credits, which stop the app if they exceed 20 a month.

### Stage 7: retire Shiny

Only after that overlap, and only with Ben's go-ahead.

Remove the Connect Cloud content, `app.R`, `manifest.json`, `scripts/write-manifest.R`, `www/` and whichever parts of `R/` and `tests/` only the Shiny UI used. Check what the export still needs before deleting anything, particularly in `R/tables.R` and `R/cards.R`. Prune `renv.lock`, rewrite `README.md`, and remove the worktree.

## Questions for Ben

Raise each one when it becomes relevant, and do not settle them yourself.

1. **Season picker, at Stage 2.** We keep any-combination ticks in the prototype. Nobody has evidence anyone uses combinations, so a dropdown with an all-time option may be better on a phone. Settled 5 November 2026: any-combination ticks stay.
2. **Privacy, before Stage 5.** Names and balances are readable in the public repo and history, and would sit in public JSON too. Ben can accept that, or add Cloudflare Access with a private repo. Whether Access covers static assets is unconfirmed, and the Connect Cloud free tier deploys only from public repos, so going private has to wait until Shiny is retired or be arranged separately. Settled 6 November 2026: the site stays public for now, and Ben revisits this at Stage 7, when the repo can go private.
3. **Address, at Stage 5.** A default Cloudflare address costs nothing, and a custom domain costs money. Settled 6 November 2026: start on the free workers.dev address, and decide on a domain before the link goes out at cutover. Settled 6 November 2026: Ben keeps https://borulanta.bendahmen.workers.dev/ for cutover and buys no domain.

## Out of scope until after cutover

These include writes of any kind, logins, push notifications, RSVP (WhatsApp Events is the first test of demand), title odds, Wrapped pages, `.ics` feeds, pay links and PWA installation. Most become easy afterwards. Note good ideas for later, and do not build them now.

## Change log

- 4 November 2026: a repeated match date or attendance row now stops the export instead of going to the banner. Ben decided this because correct fees come first. In the Stage 1 review's test, a repeated last match published £719.56 outstanding instead of £491.55 under a quiet banner. No regression estimate had any bearing on it. Shiny is unchanged: it carries on and shows them in its banner.
- 4 November 2026: `data_hash` covers `data/archive/*.csv` as well as `data/*.csv`, because the archived ledgers feed the balances and a stale one would otherwise pass the check. Decided while writing the export, with no bearing on any estimate.
- 5 November 2026: Ben gave the go-ahead for Stage 3. His phone could not reach the dev server, so the real-phone check falls to Stage 5. Any-combination ticks stay. Every other data problem stays in the banner, including a player missing from the active roster, attendance with no match, unknown players and anything dated before the first season. "To pay", "Nothing to pay" and "In credit" are approved. The ridge wording stays as written. Where one axis of the attack and defence plot would collapse into a line, the axes get separate scales and the plot says so. This last change came after seeing the estimates: under ridge each outcome has its own penalty, and season 2 alone shrinks goals scored to about 1e-37.
- 5 November 2026: Ben approved the Stage 3 feature list and the choices it raised. "Show all" buttons replace the pagers. The appearance strip uses plain squares rather than Observable Plot, because Plot costs about 93 KB on pages with no other chart, and it loses its date axis. The plots have no tooltips, and the attack and defence labels lose their white boxes. Roster keeps a picked player through tick changes and a second tap, and opens on the top row with no remembered name. Oleg's 12% tile and 13% row stay as Shiny has them. The new wording stands.
- 6 November 2026: Stage 4 found that a match with no attendance was charged as a squad of 0, putting the whole fee on the absent core until attendance was typed. With Ben's approval, `main` now charges nobody for such a match and lists it in the banner, and closing a season first checks every match has attendance. `scripts/publish-site.sh` stops on a match with no attendance, with no override, so the routine is sync, then attendance, then publish. Tested on the week of 4 November 2026: the sync showed the banner with outstanding unchanged at £491.55. After the attendance the publish merged `main`, committed the JSON and made the first push of `static-site`, at £567.56 owed.
- 6 November 2026: Ben chose to keep the site public for now. While the repo is public, Cloudflare Access would hide nothing the repo does not already show, and it would put a login in front of every WhatsApp link. Access does cover static assets, per Cloudflare's docs. Revisit at Stage 7, once Shiny is retired and the repo can go private.
- 6 November 2026: the site went live on Cloudflare at https://borulanta.bendahmen.workers.dev/, built from `static-site`. All 133 files match a local build of the same commit, apart from the random ids Astro gives islands. The data check passed on the host, which also shows the build sees `data/`. A commit with a wrong `data_hash` and a marker in the freshness note failed its build, and the marker never reached the live site. Its revert built and went live. On the live site at slow 4G and 4x CPU, Home's first view takes 0.73 s with no JS. Effects reaches the plot in 2.66 s with 126 KB of JS, over the 2.5 s target, because the plot code and the fonts share the bandwidth at the start. Ben accepted that. Hashed files under `/_astro/` are now cached for a year, and link previews carry the crest. A roster test written as "3 of 24" failed once the 4 November match made it 25. It now reads the string from the export.
- 6 November 2026: Ben checked the live site and its WhatsApp preview on his phone, and the Node version in the build log, and found all fine. Stage 5 is done.

## Kickoff prompt

Start a new session in `projects/borulanta-site`, and paste this.

> Read `docs/static-site-migration.md` in full and then `README.md`. Carry out Stages 0 to 2, working only in this folder on the `static-site` branch. You may commit on `static-site` as you go. Do not push, do not touch `main` and do not run the sync. Stop at the Stage 2 gate and report to me with evidence.
