# Borulanta

Shiny app tracking fees, results and attendance for the Wednesday football game.

## Layout

```
app.R                   UI, plot/table builders, server wiring
R/seasons.R             season definitions and filtering
R/fees.R                fee rule sets and the charge engine
R/analysis.R            match/player statistics (season-agnostic)
R/data.R                CSV loading, season tagging, validation
R/scrape.R              reading the league page
R/sync.R                reconciling a scrape with the files
scripts/sync.R          the sync runner
tests/testthat/         parser and sync tests, run against saved pages
data/                   the source of truth, all hand-editable CSVs
data/archive/           frozen ledgers for closed seasons
```

## Weekly routine

Results, opponents and goalscorers arrive on their own: a GitHub Action runs
every Friday morning, reads the league page and commits anything new. **All
that is left by hand is attendance** — one row per player who turned up in
`data/attendance.csv`. Record transfers in `data/payments.csv`.

Nothing else needs touching: the season is worked out from the date, and the
sync will not write a result it is not sure about.

## The sync

`scripts/sync.R` reads the Dream Leagues page for the league, keeps our
fixtures and writes them into `data/matches.csv` and `data/match_events.csv`.

```
Rscript scripts/sync.R          # dry run: say what would change
Rscript scripts/sync.R --write  # actually change it
Rscript scripts/sync.R --from saved-page.html --write
```

`.github/workflows/sync.yml` runs the same script on a Friday-morning cron,
after the tests, and commits the result. It can also be run from the Actions tab
at any time. Each run saves the page it parsed to `raw/` (gitignored locally,
uploaded as a run artifact in CI) so a broken parse can be reproduced offline,
and prints its report onto the run's summary page — worth a look, because a run
that writes nothing is green and quiet whether that is because there was nothing
new or because every fixture was refused.

Two things to know about the schedule. GitHub disables a cron workflow after 60
days with no activity in the repository, and a push made by the job itself does
not count — adding attendance each week does, so in normal use it stays awake.
And a run only ever writes what the page shows: if a new season has not been
added to `data/seasons.csv`, every fixture is refused as belonging to no season
and the run says so rather than inventing one.

The page is plain server-rendered HTML with no API behind it, so one request
returns the whole season: every fixture, and inside each one the goals with
scorer and minute, plus man of the match. What it does not carry is who turned
up, which is why attendance stays manual.

### What the sync refuses to do

Two things about the site can silently corrupt the record, and most of the sync
is about not being caught by them.

**The whole season is listed at `0 : 0` from day one.** An unplayed fixture, a
cancelled one and a genuine goalless draw are identical on the page. A match is
only written when its date has passed **and** either the score is not 0-0 or
goals are logged against it; anything else is reported as awaiting a scoreline
and left for you. A real 0-0 gets added by hand, once.

**The page is wiped when a league season rolls over,** and fixture ids start
again from scratch. So matches are keyed on their **date**, which is unique
across every season and is already what attendance joins on; `dl_match_id` is
kept for provenance only. A wiped page full of fresh fixtures reads as a page
full of unplayed matches and changes nothing.

On top of that the sync never deletes a match, never touches a season whose
`status` is `closed`, reports a score that disagrees with the one on file
instead of overwriting it, and writes nothing at all if the page fails to parse
or we do not appear in it. A scheduled run that hits one of those exits
non-zero, so it fails visibly rather than reporting success over an empty
scrape.

### What it writes

`data/matches.csv` is one row per match:

| column | meaning |
| --- | --- |
| `date` | the key everything else joins on, `dd/mm/yyyy` |
| `opponent` | blank for matches played before the sync existed |
| `goals_for` / `goals_against` | already oriented to us |
| `dl_match_id` | the site's fixture id, for provenance |

The score is two integers rather than a `4-5` string because that is what it is;
the hyphenated form is built for display in `with_result()` and stored nowhere.
Every match is played on neutral ground, so which side the league listed us on
is not recorded — it is only used to work out which end of the scoreline is ours.

`data/match_events.csv` is one row per thing that happened in a match:

| column | meaning |
| --- | --- |
| `date` | which match |
| `dl_match_id` | the site's fixture id, for provenance |
| `team` | `us` or `them` |
| `minute` | when, where the site records it |
| `event_type` | `goal` or `mom` today |
| `player` | our players by their roster name, theirs as the site has them |

It is long rather than wide so that recording something new later — assists,
cards, own goals — is a new `event_type`, not a schema change.

The sync owns `goal` and `mom` rows for any date it is looking at and rewrites
them on every run, which is what makes a correction on the site, or a newly
added name mapping, take effect on a re-run rather than appending a second copy.
Rows of any other kind are yours and are left alone.

`data/name_map.csv` maps the site's first names onto ours, since the league
records `Felix` where the roster says something else. Only our side is mapped;
the opposition's names are kept as they came, because they are not our players
and exist only so a goal tally reconciles with the score. An unmapped name is
left as it came and reported, so adding the mapping and re-running fixes it.

## Seasons

`data/seasons.csv` defines a season by its **start date** only. A season runs
until the day before the next one starts, and the newest is open-ended, so
there is no gap for an off-season settle-up payment to fall into.

Seasons here are defined by the fee rules, not the calendar: a season is a
stretch over which one set of payment rules applied, so it starts when the
rules change. They run a few months and a `2025/26` style label would imply a
schedule the game does not keep, so they are simply numbered. The picker shows the number with a small
caption saying when that season actually ran (`Sep 2025 – Aug 2026`), derived
from the matches played rather than from the nominal boundaries.

| column | meaning |
| --- | --- |
| `season_id` | short key used everywhere else (`s1`, `s2`) |
| `label` | what the season picker shows (`Season 1`) |
| `start_date` | first day of the season, `dd/mm/yyyy` |
| `fee_rules` | which rule set in `R/fees.R` prices it, or `archived` |
| `status` | `open` or `closed`, for reference |

Every match, attendance row and payment is tagged with its season at load time.
The tick boxes in the app header scope the **results and statistics** tabs, and
any combination can be in scope at once — tick one season, a few, or all of them
to pool them. Every season starts ticked, so the default view is all-time.
Ticking nothing puts nothing in scope, and each tab says so rather than showing
zeros.

The Fees tab deliberately ignores the picker and always covers all time. A
balance is a running total, not a per-season statistic: people settle up when
they settle up, not season by season, so scoping payments by date would show a
debt in one season and the mirror-image credit in the next even when everyone is
square. The charge table carries a season column instead.

Starting a new season is two steps: add a row to `data/seasons.csv`, and add
that season's players to `data/player_seasons.csv`.

## Rosters

`data/player_seasons.csv` holds one row per player per season:

| column | meaning |
| --- | --- |
| `season_id` | which season this entry describes |
| `player` | must exist in `data/players.csv` |
| `core` | subject to the core-player rules that season |
| `active` | on the roster; drives the fee picker and who gets charged |

Core status is per season rather than a column per rule change, so a player
moving in or out of the core is one cell, not a schema change.

`data/players.csv` is now just the list of names — the identity table.

## Fee rules

A rule set lives in `FEE_RULE_SETS` in `R/fees.R` and is a list of:

- `label` — human description
- `params` — the money and threshold constants
- `charge(squad, roster, params)` — returns `player`, `charge`, `explanation`,
  one row per rostered player, zero charges included

`squad` is the character vector of players present; `roster` is that season's
active players with their `core` flag. Because the function sees the whole
match at once, rules that split a remainder across the core are natural to
express, and the engine needs to know nothing about a rule's internals.

Two are defined:

- `even_split` — match fee divided by everyone present.
- `core_backstop` — even split at or above `min_players`; below that, guests
  pay a fixed reduced fee and core players cover the rest whether or not they
  played. This is what was in force at the end of 2025/26 and is what 2026/27
  currently points at.

To change the rules for a season, add a rule set and point that season's
`fee_rules` column at it. Nothing else changes.

## Closing a season

When a season is settled, freeze it so its charges survive the rules retiring:

1. Write its charges to `data/archive/charges_<season_id>.csv`
   (`season_id, date, player, played, squad_size, charge, explanation`);
   the filename must match the id, so season `s3` needs `charges_s3.csv`.
2. Set that season's `fee_rules` to `archived` and `status` to `closed`.

From then on the app reads the frozen ledger instead of repricing, so old
balances stay exactly as they were even after the rule code is deleted. Seasons
1 and 2 are already archived this way, in `data/archive/charges_s1.csv` and
`charges_s2.csv`. Write charges at full precision — the ledger is the record,
so it should not be pre-rounded.

## Statistics

Everything in `R/analysis.R` takes matches and attendance already filtered to
the window of interest and computes rates from what it is given, so the same
code serves one season, several, or all time. `filter_season()` takes a vector
of season ids, so those three cases are the same operation rather than three.

Player-effect regressions drop anyone below `MIN_REGRESSION_APPEARANCES`
(default 3) in the selected window — with only a handful of appearances a
player cannot be separated from the matches they happened to play in. They stay
in attendance and every other statistic.

## Data checks

`validate_app_data()` runs at startup and warns about attendance on a date with
no match, unknown players, rows dated before the first season, players who
appeared without being on that season's active roster, events on a date with no
match, and goals that do not add up to the scoreline they belong to. That last
one matters because a partial event list looks exactly like a complete one to
anything that counts it. Warnings appear in the console or the deployment log.
