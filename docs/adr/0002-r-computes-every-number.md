# R computes every number, and the site does no statistics or fee arithmetic

The charges, balances, statistics and regressions were already written and tested in R, and a second copy in JavaScript would be two answers that can drift apart. So R is the only source of numbers: `scripts/build-site-data.R` calls the functions in `R/` and adds no statistics of its own, and the site sorts, filters and displays what it is given, with no regression or fee arithmetic.

There is one permitted duplicate. Which fixtures are still to come, for the next match, the countdown, the run-in and the stale-fixture warning, depends on today's date, and a static build would freeze the answer. So the export writes every unplayed fixture, and `site/src/lib/fixture-dates.js` filters them in the browser on the Europe/London date, as `upcoming_fixtures()` does in R. It has its own unit test.
