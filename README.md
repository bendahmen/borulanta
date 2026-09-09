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
scripts/                the sync runner and its commit-and-push wrapper
tests/testthat/         parser and sync tests, run against saved pages
data/                   the source of truth, all hand-editable CSVs
data/fixtures.csv       ⤷ except these two, which the sync replaces wholesale
data/league_table.csv   ⤷ and which describe only the current league season
data/league_results.csv ⤷ and this one, every league result, which the sync accumulates
data/archive/           frozen ledgers for closed seasons
```

## Weekly routine

After a game, run the sync and add the week's attendance:

```
bash scripts/sync-and-commit.sh
```

That reads the league page and records the result, the opponent and the
goalscorers. **Attendance is the only thing left by hand** — one row per player
who turned up in `data/attendance.csv`. Record transfers in
`data/payments.csv`.

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

`scripts/sync-and-commit.sh` wraps that: it runs the sync, commits the two
files it owns — by pathspec, so it cannot sweep up anything else you have
staged — pushes, and appends to `raw/sync.log`.

Nothing is lost by running it late, or by skipping a week: the sync reads the
whole season every time and is idempotent, so it catches up on its own.

### Why it is not scheduled

Both of the obvious ways to schedule it are closed, and it is worth writing
down so neither gets attempted again.

**A GitHub Action cannot fetch the page.** The league site sits behind
Cloudflare, which returns 403 to datacentre addresses: the identical request
that succeeds from a home connection fails from a hosted runner. Its
`robots.txt` allows the default user agent, so this is an edge rule about where
a request comes from rather than a policy about the request — but the only ways
around it are to lie about the user agent or to proxy, and neither is worth
doing to a site that will happily serve the same page to the same person from
their own machine.

**A launchd agent on the Mac cannot reach the repository.** It lives under
`~/Library/CloudStorage`, which macOS shields from scheduled jobs, so a launchd
job cannot so much as read the script — `Operation not permitted` before it
runs a line. Granting Full Disk Access to `/bin/bash` would lift that, at the
cost of handing every script bash ever runs the same access; moving the
repository out of Dropbox would too.

So it is a command you run, which is no great imposition given attendance has
to be typed in the same sitting anyway.

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

It also replaces two **snapshot** files outright on every run:

| file | what it holds |
| --- | --- |
| `data/fixtures.csv` | every fixture of ours on the page, played or not: `date`, `opponent`, `dl_match_id` |
| `data/league_table.csv` | the league's own standings, plus the `scraped_on` date |

Neither is part of the permanent record, and the rules above deliberately do not
apply to them. They describe **the current league season only**, they are wiped
and rebuilt on the site when it rolls over, and a sync that replaces them
wholesale with next season's is doing the right thing. So they bypass the
reconciliation entirely — the one guarantee they keep is the important one, that
a page which fails to parse writes nothing at all. They are also rewritten when
no result changed, because the standings move whenever *any* team plays.

The standings are read off the site's own table rather than totted up from the
fixtures, because the two can legitimately disagree: a points deduction, a
forfeit or a tiebreak rule we do not model is visible in the table and invisible
in the scores.

`data/fixtures.csv` carries no score on purpose. `matches.csv` is the only
source of truth for a result; this is a list of dates and who we are down to
play on them.

And it **accumulates** one more file, `data/league_results.csv`: every played
fixture in the league, ours included, one row per match as the site lists it
(`date`, `home_team`, `away_team`, `home_goals`, `away_goals`, `dl_match_id`).
It exists so an opponent's strength can be read off its record against the rest
of the league — see below. It is history like `matches.csv`, because the page
is wiped at rollover, but nobody hand-edits it and nobody is going to arbitrate
a score between two other teams, so for any fixture the page lists the page
wins: a changed score is taken as a correction, and fixtures that have dropped
off the page are kept. A fixture is played by the same test as our own, so a
genuine 0-0 between two other teams is never recorded.

### Opponent strength in the player effects

The player-effect regressions compare matches with one another, and a match
against the league's best side is not the same test as one against its worst.
Opponent fixed effects are out of reach — eight other teams, each met about
twice a season — so the opponent enters as one number: its goal difference per
game that season, measured over its matches against everybody **except us**.
Leaving our own matches out stops our result from feeding back into the control
through the opponent's record. The whole season's record is used, games after
ours included, because the aim is to measure how good they were rather than to
forecast.

The index is centred within season so 0 is an average opponent. That is also
the value a match takes when its opponent is not on file — every match from
before the sync existed — together with an indicator saying so, which lets
those matches keep their own level. Seasons for this purpose are the fee
seasons in `seasons.csv`, which are the windows the app scopes a regression to.

`data/name_map.csv` maps the site's first names onto ours, since the league
records `Felix` where the roster says something else. Only our side is mapped;
the opposition's names are kept as they came, because they are not our players
and exist only so a goal tally reconciles with the score. An unmapped name is
left as it came and reported, so adding the mapping and re-running fixes it.

## The home page

The tab the app opens on, and the only one that answers a question in the
present tense: the last result with who was there and what they did, the next
fixture with a countdown, and the league table for context. Under those sit the
Fee check and Settle up cards, so the two things anyone actually opens the app
for are on the first screen.

The Fee check card is on both Home and Fees. Shiny needs a unique id per input,
so the card is built by a function rather than stored, and the two pickers get
different ids; the server keeps them on the same person, so switching tabs never
shows you somebody else's balance.

Like the Fees tab, and for the same reason, the home page **ignores the season
picker**. What just happened and what is next are not per-season questions.

Both cards degrade rather than break. Opponent, scorers and man of the match all
arrived with the sync, so nothing played before it existed has any of them —
each is dropped from the card rather than rendered blank, because a line reading
`Scorers: —` on every match in the archive is worse than no line at all.

**The live app is only as fresh as the last deploy.** The CSVs are bundled into
the deployment, so a fixture list synced and committed on the Mac does not reach
the deployed app until it is redeployed. The results tabs have always had this
property and it did not much matter; a countdown to the next match is the first
thing on the site that looks wrong when it is stale.

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

The Fees tab and the home page deliberately ignore the picker; Fees always
covers all time. A
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

Three are defined:

- `even_split` — match fee divided by everyone present.
- `core_backstop` — even split at or above `min_players`; below that, guests
  pay a fixed reduced fee and core players cover the rest whether or not they
  played. In force to the end of season 2.
- `min_denominator` — the fee (£76) is divided by the squad or by
  `min_players` (8), whichever is larger, so turning up never costs more than it
  would in a full squad. At 8 or more this is a plain even split and nobody
  absent pays anything; below 8 the split leaves a shortfall, which falls on the
  core players who missed the match. In force from season 3.

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
match, goals that do not add up to the scoreline they belong to, a fixture dated
on a match already recorded (a fixture list the sync has not caught up with),
and our own row going missing from the league table. That last
one matters because a partial event list looks exactly like a complete one to
anything that counts it. Warnings appear in the console or the deployment log.
