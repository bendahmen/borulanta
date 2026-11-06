# The notice that sends Shiny's visitors to the static site until Shiny closes.

test_that("the moved notice links to the site, in the same tab", {
  links <- htmltools::tagQuery(moved_notice)$find("a")$selectedTags()

  expect_length(links, 1)
  expect_equal(links[[1]]$attribs$href, SITE_URL)
  expect_null(links[[1]]$attribs$target)

  # Squashed, because htmltools puts each child on a line of its own and the
  # browser reads any run of whitespace as one space. The <wbr> break points
  # show nothing, so without them the link text is the address itself.
  html <- gsub("<wbr>", "", str_squish(as.character(moved_notice)), fixed = TRUE)
  expect_match(
    html,
    paste0(
      "<strong>Borulanta has moved.</strong> Use <a href=\"", SITE_URL, "\">",
      SITE_URL, "</a> from now on. This page closes in early December."
    ),
    fixed = TRUE
  )
})
