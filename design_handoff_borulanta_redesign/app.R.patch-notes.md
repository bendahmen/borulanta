# Changes needed in app.R

Everything below is additive/small — no restructuring of the Shiny app is
required. The new `borulanta.css` (drop into `www/`) carries almost all of
the visual change on its own because it reuses every existing class name.

## 1. Theme colors (`app_theme <- bs_theme(...)`)

Replace the color arguments with the new palette so bslib's own generated
CSS (buttons, focus rings, DT chrome, etc.) matches:

```r
app_theme <- bs_theme(
    version = 5,
    bg = "#f7f9f6",
    fg = "#16241f",
    primary = "oklch(45% 0.16 258)",   # navy blue — was the lime green
    secondary = "oklch(52% 0.15 45)",  # amber
    success = "oklch(46% 0.15 148)",   # green
    info = "oklch(45% 0.16 258)",
    warning = "oklch(52% 0.15 45)",
    danger = "oklch(52% 0.15 45)",
    base_font = font_collection("Barlow", "Avenir Next", "Trebuchet MS", "sans-serif"),
    heading_font = font_collection(
        "Barlow Condensed",
        "Futura",
        "Avenir Next Condensed",
        "Arial Narrow",
        "sans-serif"
    ),
    code_font = font_collection("SFMono-Regular", "Menlo", "monospace")
)
```

## 2. Crest badge in the hero (optional but recommended)

1. Copy `design_reference/borulanta-crest.png` into `www/`.
2. In the `app-hero` div, add the crest as the first child:

```r
div(
    class = "app-hero",
    div(
        class = "hero-crest",
        tags$img(src = "borulanta-crest.png", alt = "Borulanta crest")
    ),
    div(
        class = "hero-kicker", "Wednesday Football"
    ),
    ...
)
```

Wrap the kicker/title/copy/meta block in its own `div` so the crest and
text sit side-by-side (flex row, already handled by the new `.app-hero`
CSS):

```r
div(
    class = "app-hero",
    div(class = "hero-crest", tags$img(src = "borulanta-crest.png", alt = "Borulanta crest")),
    div(
        div(class = "hero-kicker", "Wednesday Football"),
        h1(class = "hero-title", "Borulanta"),
        p(class = "hero-copy", "Track what you owe, review recent match results, and see who keeps the squad going each week."),
        div(class = "hero-meta", icon("futbol"), span("Fees, attendance, and results in one place"))
    )
)
```

## 3. Fee balance color (owed vs. settled/credit)

`fees_owed` currently always renders green via CSS. To make it switch to
amber when a balance is owed, add the `is-owed` class conditionally in the
server (`renderUI` for `output$fees_owed`):

```r
output$fees_owed <- renderUI({
    balance <- calculate_fees(input$fee_player, matches, attendance, payments, players)
    tags$div(
        class = paste("fee-amount", if (balance > 0) "is-owed" else ""),
        glue("£{balance}")
    )
})
```

(`.fee-amount.is-owed` is already defined in the new CSS — amber for
"payment due", green for settled/credit.)

## 4. Win / Draw / Loss badges on the Matches table (optional)

Currently the Matches tab is a plain DT of `date, result`. To show a
colored badge like the design reference, add a computed column before
rendering:

```r
match_table <- function(matches) {
    matches <- matches %>%
        mutate(
            scored = as.integer(str_extract(result, "^\\d+")),
            conceded = as.integer(str_extract(result, "\\d+$")),
            outcome = case_when(
                scored > conceded ~ "<span class='result-badge win'>WIN</span>",
                scored == conceded ~ "<span class='result-badge draw'>DRAW</span>",
                TRUE ~ "<span class='result-badge loss'>LOSS</span>"
            )
        ) %>%
        select(date, result, outcome)

    datatable(
        matches,
        rownames = FALSE,
        escape = FALSE, # required to render the badge span
        class = "nowrap",
        options = list(dom = "t", pageLength = nrow(matches), ordering = FALSE, autoWidth = TRUE, scrollX = TRUE)
    )
}
```

## 5. Column sorting on the Attendance table

No change needed — `attendance_table()` already omits `ordering = FALSE`,
so DT's default click-to-sort-by-column is already active. The new CSS
just recolors the sort caret and adds `cursor: pointer` to the header
cells so it reads as interactive.

Optional: highlight the top 3 rows the way the design reference does, by
adding a `rowCallback` in the `datatable(...)` options:

```r
options = list(
    ...,
    rowCallback = JS(
        "function(row, data, index) {",
        "  if (index < 3) { $(row).addClass('rank-top3'); }",
        "}"
    )
)
```
(Only correct once the table is sorted by attendance rate descending —
which is already its default order.)

## 6. Files to copy into the app

- `borulanta.css` → `www/borulanta.css` (overwrite)
- `design_reference/borulanta-crest.png` → `www/borulanta-crest.png`
