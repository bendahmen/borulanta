# Browser fallback tests never contact the site or launch Safari.
fetch_test_env <- function(status) {
  response <- structure(list(
    status_code = as.integer(status), url = DREAMLEAGUES_URL,
    headers = list(`content-type` = "text/html; charset=UTF-8"),
    content = charToRaw("<html>direct download</html>")
  ), class = "response")
  testthat::local_mocked_bindings(GET = function(...) response, .package = "httr",
                                  .env = parent.frame())
  env <- new.env(parent = globalenv())
  sys.source(here::here("R", "scrape.R"), envir = env)
  env$Sys.info <- function() c(sysname = "Darwin")
  env
}

test_that("403 captures Safari HTML and saves the successful snapshot", {
  env <- fetch_test_env(403)
  html <- fixture_html("shoreditch-unplayed.html")
  env$fetch_league_page_in_safari <- function(url) {
    expect_identical(url, DREAMLEAGUES_URL)
    html
  }
  snapshot <- tempfile()
  on.exit(unlink(snapshot))
  expect_identical(env$fetch_league_page(snapshot_path = snapshot), html)
  expect_identical(paste(readLines(snapshot), collapse = "\n"), html)
  expect_gt(nrow(parse_fixtures(html)), 0)
})

test_that("normal downloads and other HTTP errors do not open Safari", {
  for (status in c(200, 429, 500)) {
    env <- fetch_test_env(status)
    env$fetch_league_page_in_safari <- function(...) stop("Unexpected Safari call")
    if (status == 200) {
      expect_identical(env$fetch_league_page(), "<html>direct download</html>")
    } else {
      expect_error(env$fetch_league_page(), class = paste0("http_", status))
    }
  }
})

test_that("failed captures leave an existing snapshot untouched", {
  env <- fetch_test_env(403)
  env$system2 <- function(command, args, stdout, stderr) {
    writeLines("partial page", stdout)
    writeLines("Timed out waiting for league fixtures", stderr)
    1L
  }
  snapshot <- tempfile()
  on.exit(unlink(snapshot))
  writeLines("previous snapshot", snapshot)
  expect_error(env$fetch_league_page(snapshot_path = snapshot), "Timed out")
  expect_identical(readLines(snapshot), "previous snapshot")
})

test_that("empty captures fail and successful captures return UTF-8 HTML", {
  env <- fetch_test_env(403)
  captured <- ""
  env$system2 <- function(command, args, stdout, stderr) {
    writeLines(captured, stdout, useBytes = TRUE)
    0L
  }
  expect_error(env$fetch_league_page(), "empty page")
  captured <- "<html>é</html>"
  expect_identical(env$fetch_league_page(), captured)
})
