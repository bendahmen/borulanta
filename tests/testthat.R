library(testthat)

# No package here, just scripts, so point testthat at the project root and let
# helper-borulanta.R source R/ the same way app.R does.
setwd(here::here())
test_dir("tests/testthat")
