# The notice that sends Shiny's visitors to the static site until Shiny closes.

test_that("the moved notice links to the site's own address, in the same tab", {
  # The address as the site declares it, so that SITE_URL cannot drift from it and a
  # typo in either fails here.
  config <- readLines(here::here("site", "astro.config.mjs"))
  site <- na.omit(str_match(config, "^\\s*site:\\s*['\"]([^'\"]+)['\"]")[, 2])
  expect_length(site, 1)
  url <- paste0(site[[1]], "/")

  links <- htmltools::tagQuery(moved_notice)$find("a")$selectedTags()

  expect_length(links, 1)
  expect_equal(links[[1]]$attribs$href, url)
  expect_null(links[[1]]$attribs$target)

  # Squashed, because htmltools puts each child on a line of its own and the
  # browser reads any run of whitespace as one space. The <wbr> break points
  # show nothing, so without them the link text is the address itself.
  html <- gsub("<wbr>", "", str_squish(as.character(moved_notice)), fixed = TRUE)
  expect_match(
    html,
    paste0(
      "<strong>Borulanta has moved.</strong> Use <a href=\"", url, "\">",
      url, "</a> from now on. This page closes in early December."
    ),
    fixed = TRUE
  )
})
