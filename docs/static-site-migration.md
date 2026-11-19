# Static site migration plan

Written 4 November 2026. Stage 7 rewritten 19 November 2026. Read this whole file before doing anything, then read `README.md`.

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
| Shiny | Retired at Stage 7, below |

The debate behind these rejected Quarto, Django and Supabase. webR is also out: its smallest useful setup downloads about 29 MB.

## Repo logistics

**This section describes Stages 0 to 5.** Since cutover, `main` holds the site. Stage 7 has its own rules under "Where the sessions work", and they override anything here, including the Shiny entry in `launch.json`, the manifest and the kickoff prompt's permissions. The Dropbox rules still hold.

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

**Numbers on the page.** R formats values and the site writes the words, except dates, which the site formats. Decided 18 November 2026 in issue 6, replacing a rule that covered only money and estimates.

- R exports the value and a `*_display` string for money (`format_fee_amount()`), regression estimates (`format_estimate()`), the three rounded columns (goals per game, points per match by opponent, points per match played and missed), the with-and-without gap and the headline turnout. JS sorts on the value, shows the string and never rounds.
- R keeps two kinds of text that are data: the `validate_app_data()` banner messages, which the export's stop rule matches on, and the charge explanations, which closed seasons' ledgers already store.
- R exports dates as ISO strings, and the site formats them through `site/src/lib/dates.js`. It splits the string by hand and uses hand-written English names, with no `Intl`, because Node's en-GB gives `Wed, 09 Sept 2026` where R gives `Wed 09 Sep 2026`.
- Every other word on the page belongs to the site: labels, headings, empty states, small composed strings such as the ordinal and the scorer list, and the data-dependent sentences, which use R's display strings for the values inside them.

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

The Worker is called `borulanta`, matching `site/wrangler.jsonc`, which serves `dist` as static assets with the site's own 404 page. Its build settings in the dashboard are: root directory `site`, build command `npm ci && npm run build`, deploy command `npx wrangler@4.158.0 deploy`, and the build variable `SKIP_DEPENDENCY_INSTALL=true` so that `npm ci` runs once. The production branch is set under Settings → Build → Branch control, because the first build used the default branch, `main`, and failed for want of `site/`. The dashboard has no button to start a build, so a push starts one. After cutover the production branch is `main` and the build watch paths include only `site/*`, so only a push that touches `site/` starts a build, apart from a push of 20 or more commits, which always builds.

### Stage 6: cutover

Only with Ben's go-ahead.

- [x] Merge `static-site` into `main`, and switch Cloudflare's production branch to `main`.
- [x] Run `bash scripts/site-setup.sh` in the main folder.
- [x] Add a notice to the Shiny app linking to the new address. Connect Cloud offers no redirect we know of, so check this.
- [x] Ben posts the new link in the WhatsApp group. It is also in the Borulanta Core group description.
- [x] Keep Shiny live for about four weeks and watch its credits, which stop the app if they exceed 20 a month. Superseded 18 November 2026: Shiny may go before 4 December.

### Stage 7: retire Shiny

Stage 7 removes Shiny and leaves the live site exactly as it is. Ben settled it on 18 and 19 November 2026 through the map "Retire the Shiny app" (issue 2) and its tickets: the old address (issue 3), the checks (issue 4), the inventory of Shiny-side code (issue 5), formatting (issue 6) and this spec (issue 7). Issue 5's resolution comment lists every function, string, test file and package by name, so read it before starting any session.

The work comes in five steps, strictly in order. Ben closes the old address first. Then four agent sessions follow, each with its own GitHub issue labelled `ready-for-agent` and blocked by the one before it (issues 8 to 11):

| Step | Who | What | Changes `site/`? |
|---|---|---|---|
| 0 | Ben | Delete the Connect Cloud content and send the closing message | No |
| A1 | agent | Fixed wording moves into the site | Yes, so it builds |
| A2 | agent | Formatting and data-dependent wording move into the site | Yes, so it builds |
| B | agent | Delete Shiny and the code only it used | No |
| C | agent | Docs and clean-up | No |

The wording has to move before the deletion, because today the export builds its strings from `R/cards.R`, `R/tables.R` and `R/plots.R`. The repo stays public, and new site features stay out of scope.

#### Where the sessions work

Sessions A1 to C work in the `borulanta-site` worktree on a branch `stage-7`, cut from `main`. The main folder stays on `main` for the sync, attendance, payments and the weekly publish. Committing on `main` directly would be unsafe: `publish-site.sh` pushes `main`, so a Wednesday publish would push a session's half-done commits.

- A1 starts in the worktree with `git switch -c stage-7 main`, then `bash scripts/site-setup.sh` and `Rscript -e 'renv::status()'`. Later sessions continue on the same branch.
- Never run the sync, add match data or run `publish-site.sh` from the worktree.
- Each session may commit on `stage-7` as it goes. Pushing needs Ben's direct yes every time.
- To push, the session rebases `stage-7` onto `main` if `main` has moved, then fast-forwards `main` in the main folder with `git merge --ff-only stage-7` and pushes `main`. `stage-7` itself is never pushed.
- A rebase that clashes in `site/src/generated` or `site/public/data/seasons` is settled by rerunning the export on `main`'s CSVs, never by hand. A rebase that brings new CSVs means the session's checks run again against the new base.
- Any check that fails, and any `dist` difference outside what is allowed, stops the session and goes to Ben.
- Each session records its check results in the change log below and in its issue's resolution comment, then closes the issue.

#### Step 0: Ben closes the old address

Ben does this himself, because an agent should not, and chose on 18 November 2026 to do it before any code work.

1. Delete the Connect Cloud content in the dashboard. No stub, no frozen bundle, no grace period. This kills `bendahmen-borulanta.share.connect.posit.cloud` and the older `01a08745-…` address that redirects to it.
2. Remove `bendahmen/borulanta` from the Connect Cloud GitHub App under GitHub → Settings → Applications, or uninstall the app. Keep the Posit account.
3. Send the closing message to Borulanta Core: *"The old Borulanta page on posit.cloud is now closed. Everything is at https://borulanta.bendahmen.workers.dev/"*. It goes after the deletion so that it is true when read. No one-to-one messages go to the people who were sent the old link privately.
4. Tell an agent the content is gone. It rewrites the agent memory note `borulanta-live-url` to cover the site only, and records that both posit.cloud addresses are dead from that date.

Step 0 is a hard prerequisite for B's push. Auto-publish on push is on until the content is gone, so pushing the deletion while it exists would redeploy a broken bundle. A1 and A2 do not depend on it, because neither changes what Shiny runs.

#### Session A1: fixed wording moves into the site

A1 moves every string that does not depend on the data out of the export and into the site. That is all of `wording.json`, the strings copied from `app.R` (`APP_EMPTY_STATES`, `APP_PLAYER_NOTE_AFTER_NAME`), the table headings, the plot labels, the fixed empty states and fallbacks, and the Monzo link.

- Each string goes into the component that shows it. A string used in two or more places goes in `site/src/lib/labels.js`.
- The export stops writing `wording.json` and every fixed-string field. Delete the file and its readers, and keep the strings word for word.
- Reword the 25 comments in 19 files under `site/` that cite Shiny or `app.R`, so each stands alone: keep the reason, drop the reference. They change here because B must not touch `site/`.
- In `test-site-export.R`, delete the tests that compare fixed wording with `R/cards.R` or `app.R`, and take `wording.json` out of the file-list test. A fixed string gets no new test.
- Leave `R/cards.R`, `R/tables.R`, `R/plots.R` and `R/format.R` in place. A2 still reads some of them, and B deletes them.

Checks: part 1 below, then the live check after the push.

#### Session A2: formatting and data-dependent wording move into the site

A2 carries out the formatting rule under "Numbers on the page". Afterwards the export no longer sources `R/cards.R`, `R/tables.R` or `R/plots.R`, and it calls none of the `R/format.R` functions that B deletes.

R side:

- Export raw values: dates as ISO strings, `first_match` and `last_match` per season for the spans, the league position and the team count, scorers as `[{player, goals}]`, points, kickoff and pitch, and outcome ids (`points`, `goals_scored`, …) in place of labels.
- Key `player_regression_table()`'s display columns by outcome id instead of by label.
- Keep the `*_display` strings for money, estimates, the three rounded columns, the with-and-without gap (`format(round(abs(x), 2), nsmall = 2)`) and the headline turnout (`round(rate * 100)`). They live in `R/format.R` as small named functions, except `format_estimate()`, which stays in `R/analysis.R` beside `player_regression_table()`.
- Move `STALE_FIXTURES_DAYS` to `R/analysis.R` beside `run_in()`. `season_span()` returns dates only.
- The `validate_app_data()` messages and the charge explanations stay as R's text. The `has_regression` gate, which copies `app.R`'s `req()`, stays in the export.
- Rewrite the remaining mixed tests in `test-site-export.R`, about 25, so they keep the numbers and drop the wording. Rename `shiny_data` to `app_data`.

Site side:

- Write `site/src/lib/dates.js` with all six formats R uses: `%a %d %b %Y`, `%d %b %Y`, `%d %B %Y`, `%d %b`, `%b %Y` and `%b`. Its unit test checks every date on file against R's output.
- Season spans, with `season_span()`'s three cases.
- The small composed strings: the ordinal, the scorer list, the result badge, the kickoff line, fallbacks such as "Opponent to be confirmed", and "—" for a missing value. Also `OUTCOME_LABELS`.
- The data-dependent sentences: the freshness note, the scorer coverage note, the with-and-without note, the regression explanation and estimator note, the league position line, "Tonight / Tomorrow / In {days} days", the last-match details, the stale-fixture note and the banner title. Each keeps R's careful wording word for word.
- Data-dependent wording gets a site test: plurals, "3 of 24", which empty state shows, and "To pay" against "In credit".

Checks: part 1 below, then the live check after the push.

#### Session B: delete Shiny

B starts only after A2 is live, and pushes only once Ben confirms that Step 0 is done. Its push must change nothing under `site/`, so it starts no Cloudflare build.

Delete:

- `app.R`, `R/_disable_autoload.R`, `manifest.json`, `scripts/write-manifest.R` and `www/`.
- `R/cards.R`, `R/tables.R` and `R/plots.R`.
- From `R/format.R`: `APP_COLOURS`, `ordinal`, `format_match_date`, `result_badge`, `listed_scorers` and `estimate_label`.
- From `R/analysis.R`: `REGRESSION_TABLE_COLUMNS`, `next_fixture()` and `season_record()`, which is already dead.
- `tests/testthat/test-moved-notice.R`, `test-pickers.R` and `test-tables.R`.

Trim:

- `test-home.R`: the league-strip and ordinal tests, the four `next_fixture()` tests and the two `season_record()` tests.
- `helper-borulanta.R`: the `library()` calls for shiny, bslib, DT and ggplot2, and the deleted files in its source loop.
- `scripts/build-site-data.R`'s source list, its comment that cites `app.R`, and the comment in `tests/testthat.R` that names `app.R`.
- The `.gitignore` comment that cites `scripts/write-manifest.R`.
- `renv.lock`: remove the 72 packages that only Shiny-side code needs, which issue 5 lists. The diff must hold only removals, the 67 survivors keep their versions, and `renv::status()` must report no issues afterwards.

Stays: everything else in `R/`, `scripts/sync.R`, `scripts/sync-and-commit.sh`, `scripts/publish-site.sh`, `scripts/site-setup.sh`, `data/` and the remaining tests.

Checks: part 2 below. A `grep` for every deleted symbol and file name outside `docs/` must find nothing.

#### Session C: docs and clean-up

C describes what is left and removes the scaffolding of the migration.

- `README.md`: rewrite only the parts tied to Shiny. These are the opening line, Layout, the `R/_disable_autoload.R` note, Deploying (Connect Cloud and the manifest become Cloudflare and the publish), the Shiny paragraphs of The home page, Tables, and "runs at startup" in Data checks. Describe the export, the site and Cloudflare as they are, leave every other section alone, and match the README's own voice. Ben reviews the diff.
- `.claude/launch.json`: drop the Shiny `borulanta` entry and keep `borulanta-site`.
- Write four ADRs in `docs/adr/`, in the domain-modeling format. First, a static Astro site replaces Shiny, with no backend. Second, R computes every number, and the site does no statistics or fee arithmetic. Third, R formats values and the site writes the words, with dates formatted in JS. Finally, the data checks at build time: `data_hash` and the export's stop on a repeated match date or attendance row.
- Delete `docs/static-site-features.md` and this file, and repoint their references: `README.md` line 80 and the comment in `scripts/build-site-data.R`. Git keeps the history.
- Check that the memory note matches Step 0.

After C's push, from the main folder and with Ben's yes: `git worktree remove ../borulanta-site`, delete the local branches `stage-7`, `static-site` and `ui-refresh-sporty-mobile`, and delete the last two on GitHub with `git push origin --delete static-site ui-refresh-sporty-mobile`. Both are fully merged into `main`.

#### Checks

These come from issue 4. Each is a one-off check recorded in the change log and in the session's issue, not committed as a test.

**Part 1, for A1 and A2: moving the wording leaves the site unchanged.**

1. **The HTML diff.** Build `dist` at the session's base commit and again at its last commit, on the same CSVs. Compare every file after normalising Astro's random island ids and the hashed `/_astro/*` filenames. Anything under `data/` may differ, because the JSON changes shape. Every other byte must match.
2. **States that only JS renders.** Render each island's component in Node with `svelte/server`, as the site tests already do, for every season file, before and after, and compare the output. This covers the 6 non-default tick combinations and what clicks reveal. No headless browser is added.
3. **Wording the real data doesn't show.** Repeat both checks on one throwaway copy of `data/`, doctored to trigger the banner and a window with no matches. Keep the copy in the session's scratchpad and never commit it.
4. **The suites.** The R tests and the site tests pass, including the export tests that `publish-site.sh` runs as its step 4.
5. **The live site.** Once the push has built, compare the deployed site with a local build of the same commit. Every file matches apart from island ids.

**Part 2, for B: deleting Shiny breaks nothing.** Rerun the export after the deletion. Every JSON file must be byte-identical to its version before the deletion. Then run the R and site test suites. The push changes nothing under `site/`, so it needs no live check.

## Questions for Ben

Raise each one when it becomes relevant, and do not settle them yourself.

1. **Season picker, at Stage 2.** We keep any-combination ticks in the prototype. Nobody has evidence anyone uses combinations, so a dropdown with an all-time option may be better on a phone. Settled 5 November 2026: any-combination ticks stay.
2. **Privacy, before Stage 5.** Names and balances are readable in the public repo and history, and would sit in public JSON too. Ben can accept that, or add Cloudflare Access with a private repo. Whether Access covers static assets is unconfirmed, and the Connect Cloud free tier deploys only from public repos, so going private has to wait until Shiny is retired or be arranged separately. Settled 6 November 2026: the site stays public for now, and Ben revisits this at Stage 7, when the repo can go private. Settled 18 November 2026: the repo stays public, with no Cloudflare Access.
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
- 6 November 2026: Ben settled the open data questions on `main`. His bank details are off the Settle up card in Shiny and on the site, leaving the Monzo link, though they stay in the public repo's history. An own goal, which the league page lists as scorer "Og", counts in the score and everything built on it but for no player, and no longer shows in the banner. The 21 October goal by a guest also called Daniel is listed in `data/uncredited_goals.csv` and credited to nobody, which the sync cannot undo. The site keeps the workers.dev address.
- 6 November 2026: with Ben's go-ahead, `static-site` was fast-forwarded into `main`, so `main` now holds the site, and `scripts/site-setup.sh` ran in the main folder. `scripts/publish-site.sh` and `scripts/sync-and-commit.sh` now run on `main` only and refuse any other branch, and the publish no longer merges. Shiny shows a notice at the top of every tab: "Borulanta has moved. Use https://borulanta.bendahmen.workers.dev/ from now on. This page closes in early December." It is a link, not a redirect, because Connect Cloud's content settings offer none, only a custom name or, on paid plans, a custom domain. Ben switched Cloudflare's production branch to `main` and set its build watch paths to `site/*` only, so a push that changes only CSVs or R code builds nothing rather than failing the data check.
- 19 November 2026: Ben signed off the Stage 7 spec, built on the map "Retire the Shiny app" (issue 2). Shiny may go before 4 December, and Ben deletes the Connect Cloud content before any code work. The wording moves into the site in two sessions, before anything is deleted. "Numbers on the page" now says that R formats values and the site writes the words, with dates formatted in JS. The sessions work on a `stage-7` branch in the old worktree so that a weekly publish cannot push half-done work. Once Stage 7 is done, this file and the features list are deleted, and four ADRs keep the lasting decisions. The repo stays public.
- 19 November 2026: session A1 (issue 8) moved the fixed wording into the site. The export no longer writes `wording.json` or any fixed-string field. That includes four strings A2 lists, because they depend on no data: "Data check", the outcome labels, "Tonight / Tomorrow / In {days} days" and "Opponent to be confirmed". The estimator note stays in the season files for A2, because the Effects island reads it there. Ben chose to keep every island's props as they were, filled from `site/src/lib/labels.js` or the page that renders the island, so the built pages stay the same. The session first regenerated `meta.json`, whose `data_hash` had been stale since the roster's `active` column was dropped. Part 1, base `c7cb6dd` against the session's last commit, on the real data and on a doctored copy with a season 4 and no matches in it, and an attendance row on a night with no match: `_astro/` was byte-identical and the island renders were identical, 1,198 renders on the real data and 2,394 on the copy. Two differences fell outside `data/`, and Ben accepted both. Every page carries the reworded HTML comment in `Base.astro`, which no longer cites `www/`. On Fees > You and Players > Effects the inlined CSS rules come out in another order, with the same rules and nothing else changed. Those rules are scoped to StatTiles and CardHead, which nothing else styles. The R suite passed, 235 tests, and so did the site suite, 325 tests. 35 comment lines in 24 files under `site/` still cite `R/cards.R`, `R/tables.R`, `R/plots.R` or their functions, so A2 rewords them, since B must not touch `site/`. After the push of `a855414`, the Cloudflare build passed, and all 138 files it serves matched a local build of the same commit apart from island ids.
- 19 November 2026: session A2 (issue 9) moved the formatting and the data-dependent wording into the site. The export writes raw values and the `*_display` strings, from `format_rate()`, `format_gap()` and `format_turnout()` in `R/format.R`, and no longer sources `R/cards.R`, `R/tables.R` or `R/plots.R` or loads shiny, bslib, DT or ggplot2. `site/src/lib/dates.js` writes R's six date formats, and `site/src/lib/wording.js` writes every composed string and sentence word for word, "1 players" included. Pages write their words at build time and hand the islands the props they had. Each season file's regression blocks carry `min_appearances`, so that the Effects island can write its explanation without a new prop. The Shiny season picker keeps its own copy of the span caption until B deletes it. Part 1, base `ea06718` against `00f4676`, on the real data and on a doctored copy with an empty season 5, a season 4 of one match with no opponent, no events and a squad of one, an attendance row on a night with no match, and a fixture with no opponent, kickoff or pitch. Every JSON value outside the moved fields is unchanged, 358 checks on the real data and 1,445 on the copy, and each new display string equals the value it replaces. The island renders were identical, 4,575 on the real data and 18,562 on the copy, once Svelte's scoped class names are normalised, because the harness compiles each tree from its own folder. Outside `data/` three things differ, on both data sets, and need Ben's acceptance. The island bundles under `_astro/` change, as they must, since the islands now write the words; 8 of them. The shared wording code lands in the SeasonPicker chunk, so on the 22 pages with islands the same `modulepreload` links come out in another order, because `module-preload.js` sorts them by size. On Matches > Results and > Trends two scoped style blocks swap places, with the same lines and nothing else changed. Every other file is identical, 100 on the real data. The R suite passed, 234 tests, and the site suite, 352. The site's `coefficient_plot` card key and `OUTCOME_LABELS` in `R/analysis.R` share names with what B deletes or orphans, for B to weigh.

## Kickoff prompts

For Stage 7, start each session in `projects/borulanta-site` once the one before it has closed its issue, and paste its prompt. `<X>` is A1, A2, B or C, and `<N>` is that session's issue number: A1 is issue 8, A2 is issue 9, B is issue 10 and C is issue 11.

> Read `docs/static-site-migration.md` in full, then `README.md`, then the resolution comment on issue 5. Work issue `<N>`, which is Stage 7 session `<X>`, and nothing else. Follow "Where the sessions work": you may commit on `stage-7` as you go, but never run the sync or the publish, and ask me before any push. Run the session's checks, record them in the change log and as the issue's resolution comment, and stop with evidence if anything fails or differs.

The Stages 0 to 2 prompt, kept for the record: *Read `docs/static-site-migration.md` in full and then `README.md`. Carry out Stages 0 to 2, working only in this folder on the `static-site` branch. You may commit on `static-site` as you go. Do not push, do not touch `main` and do not run the sync. Stop at the Stage 2 gate and report to me with evidence.*
