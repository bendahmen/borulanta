# Shiny sources every file in R/ automatically, before a line of app.R runs.
# This file's existence turns that off. It has to.
#
# Autoloading happens before app.R's library() calls, so anything in R/ that
# needs a package at load time fails there — R/cards.R builds its cards
# eagerly and cannot see bslib yet. It also ignores load order, which R/ has:
# R/seasons.R needs parse_match_date() from R/fees.R.
#
# app.R sources all of R/ itself, in that order, and always did — which is why
# autoloading was invisible until now: it was sourcing every file a second
# time, harmlessly, because they only defined functions.
