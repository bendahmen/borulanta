# R computes every number

The charges, balances, statistics and regressions were already written and tested in R, and a second copy in JavaScript would give two answers that can drift apart. So `scripts/build-site-data.R` calls the functions in `R/` and adds no statistics of its own, and the site does no statistics or fee arithmetic: it sorts, filters and displays what it is given.

## Consequences

There is one permitted duplicate. Which fixtures are still to come depends on today's date, and a static build would freeze it. So the export writes every unplayed fixture, and `site/src/lib/fixture-dates.js` filters them in the browser on the Europe/London date, as `upcoming_fixtures()` does in R. The next-match and run-in tests cover it.
