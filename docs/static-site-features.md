# Shiny features on the static site

This is the Stage 3 walk through `README.md` and every output in `app.R`. Each feature is kept, changed or dropped. Nothing is dropped outright. The last section lists the small things that went inside a "changed" entry, for Ben to approve.

## Outputs in `app.R`

| Output | Status | Where it is now, and what changed |
|---|---|---|
| `next_match_card` | Changed | Home, in the header. Same words and logic. The date is worked out in the browser on the Europe/London date |
| `last_match_card` | Changed | Home, and the card links to the match's own page |
| `fees_owed`, `home_fees_owed` | Changed | Fees > You and Home. A first visit asks for a name instead of showing Anton's balance. "To pay", "Nothing to pay" or "In credit" sits beside the amount |
| `fee_overview_summary` | Kept | Fees > You |
| `payment_history` | Kept | Fees > You |
| `match_charges` | Changed | Fees > You. All seven columns. On a phone, four fold under the row. The pager became "Show all" |
| `balance_totals` | Kept | Fees > Everyone |
| `all_balances` | Changed | Fees > Everyone. On a phone, three columns fold under the row. "To pay" or "In credit" sits beside the name as well as the colour |
| `league_table_card`, `league_table` | Kept | League |
| `run_in` | Kept | League. Past fixtures drop on the Europe/London date |
| `season_form_plot` | Changed | Matches > Trends. Observable Plot rather than ggplot, buttons rather than a dropdown. Same columns, axis and reference line |
| `scorers`, `scorer_coverage` | Changed | Matches > Trends. The pager became "Show all". The first press on a figure sorts largest first. A chosen sort survives a tick change |
| `opponent_record`, `opponent_record_table` | Changed | Matches > Trends. On a phone, GF, GA and GD fold under the row |
| `matches` | Changed | Matches > Results. The pager became "Show all". Each row picks the match and links to its own page |
| `match_summary` | Kept | Matches > Results |
| `match_lineup` | Changed | Matches > Results. Names link to the player's page |
| `attendance_list` | Changed | Players > Roster. The pager became "Show all". "Top 3" sits beside the tint. Each row links to the player's page. On a phone, the goal columns fold under the row |
| `profile_player_note` | Kept | Players > Roster. Not on player pages, which have no table above |
| `player_headline` | Kept | Roster and player pages, with R's strings |
| `with_without` | Kept | Roster and player pages |
| `differential_note` | Kept | Roster and player pages, word for word |
| `appearance_timeline` | Changed | Roster and player pages. Plain squares rather than ggplot: see below |
| `attack_defence_plot` | Changed | Players > Effects. Observable Plot, labels with a halo rather than boxes. Separate axis scales where one axis would collapse into a line |
| `coefficient_plot` | Changed | Players > Effects. Observable Plot, buttons rather than a dropdown. The axis label wraps above the plot instead of being cut off |
| `player_regressions`, `regression_table_note` | Changed | Players > Effects. The pager and its count line are gone because every row shows. On a phone, the other outcomes fold under the row |
| `regression_explanation` | Kept | Players > Effects, in full |
| `season_has_matches`, `season_has_regression` | Changed | Same empty wording. The estimator switch stays visible when OLS has no rows, so season 3 alone can reach ridge |
| `season_picker_applies` | Kept | The picker shows only on Matches and Players, as `SEASON_SCOPED_TABS` says |

## Other features in `README.md` and the ui

| Feature | Status | What changed |
|---|---|---|
| Tabs: Home, Fees, League, Matches, Players | Changed | Bottom tab bar on phones, top bar from 768 px |
| Matches opens on Trends, Players on Effects | Kept | |
| Crest, colours, Barlow fonts | Kept | Fonts self-hosted, crest cut from 597 KB to 5 KB. Dark mode added |
| Strapline | Kept | On desktop Home only. The phone layout hid it already |
| Freshness note, full and short | Kept | |
| Stale fixture warning | Kept | Shows when the page is read, not when it was built |
| Data check banner | Changed | Every page, as before, except that a repeated match date or attendance row now stops the export |
| Fee check on Home and Fees, kept in step | Kept | Both read one remembered name |
| Settle up: Monzo link, bank details | Changed | Merged into the fee card. Bank details sit behind a tap |
| League position line and its link | Kept | Below the fee card on phones |
| Season tick boxes with captions | Changed | Compact chips. Ticks also go in the URL and are remembered |
| Roster table as the player picker | Kept | Opens on your own row where you turned out. Without a remembered name it opens on the top of the table, where Shiny opens on the first name on the fee list |
| Table sorting | Kept | Written by hand. Sorts on the value and shows R's string |
| Sync, scraping, fee rules, statistics | Kept | Unchanged in R. The site reads what R exports |
| Deployment to Connect Cloud | Stays until cutover | Stage 5 adds Cloudflare, Stage 7 retires this |

## New on the static site

Shareable `/matches/<date>/` and `/players/<slug>/` pages, a 404 page, one JSON file per player for the fee tables, and a stale-data check that fails the build.

## For Ben to approve

These small things went inside "changed" entries:

1. The DataTables pagers and their "Showing 1 to 15 of 16" lines became a "Show all" button.
2. The appearance strip loses its date axis and the gaps between dates. A first-to-last date line and a hover title on each square replace them.
3. The strip uses plain squares rather than Observable Plot, which departs from the plan's decision that charts use Plot. It saves about 93 KB on pages with no other chart.
4. The attack and defence labels lose their white boxes, and the plots have no tooltips: rolling values have no rounded string from R, and JS may not round them.
