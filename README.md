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

| column | meaning |
| --- | --- |
| `season_id` | key used everywhere else (`2026-27`) |
| `label` | what the season picker shows (`2026/27`) |
| `start_date` | first day of the season, `dd/mm/yyyy` |
| `fee_rules` | which rule set in `R/fees.R` prices it, or `archived` |
| `status` | `open` or `closed`, for reference |

Every match, attendance row and payment is tagged with its season at load time.
The picker in the app header scopes **every** tab, and "All seasons" pools them.

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
   (`season_id, date, player, played, squad_size, charge, explanation`).
2. Set that season's `fee_rules` to `archived` and `status` to `closed`.

From then on the app reads the frozen ledger instead of repricing, so old
balances stay exactly as they were even after the rule code is deleted. 2025/26
is already archived this way.

## Statistics

Everything in `R/analysis.R` takes matches and attendance already filtered to
the window of interest and computes rates from what it is given, so the same
code serves one season, several, or all time.

Player-effect regressions drop anyone below `MIN_REGRESSION_APPEARANCES`
(default 3) in the selected window — with only a handful of appearances a
player cannot be separated from the matches they happened to play in. They stay
in attendance and every other statistic.

## Data checks

`validate_app_data()` runs at startup and warns about attendance on a date with
no match, unknown players, rows dated before the first season, and players who
appeared without being on that season's active roster. Warnings appear in the
console or the shinyapps.io log.
