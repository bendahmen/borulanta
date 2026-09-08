# Borulanta

Shiny app tracking fees, results and attendance for the Wednesday football game.

## Layout

```
app.R                   UI, plot/table builders, server wiring
R/seasons.R             season definitions and filtering
R/fees.R                fee rule sets and the charge engine
R/analysis.R            match/player statistics (season-agnostic)
R/data.R                CSV loading, season tagging, validation
data/                   the source of truth, all hand-editable CSVs
data/archive/           frozen ledgers for closed seasons
```

## Weekly routine

After a game, append one row to `data/matches.csv` and one row per player who
turned up to `data/attendance.csv`. Record transfers in `data/payments.csv`.
Nothing else needs touching — the season is worked out from the date.

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
no match, unknown players, rows dated before the first season, and players who
appeared without being on that season's active roster. Warnings appear in the
console or the shinyapps.io log.
