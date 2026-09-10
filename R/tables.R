# Tables ----
#
# Every DT the app draws. Each takes a frame and returns a datatable widget:
# the display naming, the column formatting and the DataTables options live
# here, and nowhere upstream of here.


#' Page length for a table that shows every row it holds.
#'
#' `dom = "t"` draws no pager, so a page length short of the data loses the
#' rows past it with nothing on screen to say so. Stated once here because the
#' number is not the point — showing everything is — and because DT's default
#' of 10 is a silent truncation wherever the pager is switched off.
#' Never zero: a page length is a count of rows to draw, and a table with
#' nothing in it still has a "nothing here" row to draw.
every_row <- function(data) max(nrow(data), 1L)


league_table_table <- function(league_table) {
    datatable(
        league_table %>%
            transmute(
                Pos = position,
                Team = team,
                P = played, W = won, D = drawn, L = lost,
                GD = goal_difference,
                Pts = points
            ),
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "t",
            pageLength = every_row(league_table),
            ordering = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE,
            rowCallback = JS(
                "function(row, data) {",
                sprintf("  if (data[1] === %s) { $(row).addClass('is-us'); }", jsonlite::toJSON(OUR_TEAM, auto_unbox = TRUE)),
                "}"
            )
        )
    )
}


match_table <- function(matches) {
    matches <- matches %>%
        mutate(
            outcome = case_when(
                goals_for > goals_against ~ "<span class='result-badge win'>WIN</span>",
                goals_for == goals_against ~ "<span class='result-badge draw'>DRAW</span>",
                TRUE ~ "<span class='result-badge loss'>LOSS</span>"
            )
        ) %>%
        transmute(
            Date = format(date, "%d %b %Y"),
            Opponent = coalesce(opponent, "\u2014"),
            Result = result,
            Outcome = outcome
        )

    datatable(
        matches,
        rownames = FALSE,
        escape = FALSE, # required to render the badge span
        class = "nowrap",
        # This table is the match picker: the detail card below it reads the
        # selected row. Rows stay in the order they arrive, newest first, so a
        # row index is a position in the data and nothing has to map it back.
        selection = list(mode = "single", selected = 1, target = "row"),
        options = list(
            dom = "t",
            pageLength = every_row(matches),
            ordering = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE
        )
    )
}


opponent_record_table <- function(record) {
    datatable(
        record %>%
            transmute(
                Opponent = opponent,
                P = played, W = won, D = drawn, L = lost,
                GF = goals_for, GA = goals_against, GD = goal_difference,
                `Pts/match` = round(points_per_match, 2)
            ),
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "t",
            pageLength = every_row(record),
            ordering = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE
        )
    )
}


run_in_table <- function(fixtures) {
    datatable(
        fixtures %>%
            transmute(
                Date = format(date, "%d %b %Y"),
                Opponent = opponent,
                Pos = position,
                P = played,
                GD = goal_difference,
                Pts = points
            ),
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "t",
            pageLength = every_row(fixtures),
            ordering = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE,
            language = list(emptyTable = "No fixtures left on the league page.")
        )
    )
}


match_lineup_table <- function(lineup) {
    datatable(
        lineup %>%
            transmute(
                Player = player,
                `Season appearances` = season_appearances,
                `Attendance rate` = attendance_rate
            ),
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "t",
            pageLength = every_row(lineup),
            ordering = FALSE,
            autoWidth = TRUE
        )
    ) %>%
        formatPercentage(columns = "Attendance rate", digits = 0)
}


scorer_display_table <- function(scorers) {
    datatable(
        scorers %>%
            transmute(
                Player = player,
                Goals = goals,
                MOM = mom,
                Appearances = appearances,
                `Goals per game` = round(goals_per_appearance, 2)
            ),
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "tip",
            pageLength = 15,
            order = list(list(1, "desc")),
            autoWidth = TRUE,
            scrollX = TRUE,
            language = list(
                emptyTable = "No goals on file yet \u2014 they arrive with the sync."
            )
        )
    )
}


#' The two records side by side, and the gap between them with its caveat.
#'
#' Presented as two rows rather than one difference because the difference is
#' the part that invites over-reading: it compares the nights they turned up
#' with the nights they did not, and nothing about that holds the rest of the
#' lineup, the opponent or the squad size fixed.
with_without_table <- function(profile) {
    rows <- bind_rows(
        profile$present %>% mutate(when = "Played"),
        profile$absent %>% mutate(when = "Missed")
    )

    datatable(
        rows %>%
            transmute(
                ` ` = when,
                P = played, W = won, D = drawn, L = lost,
                GF = goals_for, GA = goals_against,
                `Pts/match` = round(points_per_match, 2)
            ),
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "t", ordering = FALSE, autoWidth = TRUE, scrollX = TRUE
        )
    )
}


#' The roster, with one row already picked out.
#'
#' `selected_row` is seeded rather than sent afterwards through a proxy: a
#' proxy message can arrive before the table it is addressed to exists, and
#' then never again, because nothing has changed to make it fire twice. Given
#' here, the table and the page below it agree by construction.
attendance_table <- function(attendance_list, selected_row = 1L) {
    datatable(
        attendance_list %>%
            transmute(
                Player = player,
                Turnout = attendance_rate,
                `Avg points` = avg_points,
                `Avg goals scored` = avg_goals_scored,
                `Avg goals conceded` = avg_goals_conceded
            ),
        rownames = FALSE,
        class = "nowrap",
        # The roster is the player picker: the page below it reads the selected
        # row. DT reports the index into the data rather than the position on
        # screen, so re-sorting the table by any column keeps this honest.
        selection = list(mode = "single", selected = selected_row, target = "row"),
        options = list(
            dom = "tip",
            pageLength = 15,
            order = list(list(1, "desc")),
            autoWidth = TRUE,
            scrollX = TRUE,
            rowCallback = JS(
                "function(row, data, index) {",
                "  if (index < 3) { $(row).addClass('rank-top3'); }",
                "}"
            )
        )
    ) %>%
        formatPercentage(columns = "Turnout", digits = 0)
}


match_charge_table <- function(match_charges) {
    table_data <- match_charges %>%
        transmute(
            Date = format(date, "%d %b %Y"),
            Season = season,
            Result = result,
            `Played?` = if_else(played, "Yes", "No"),
            `Squad size` = squad_size,
            Charge = charge,
            Explanation = explanation
        )

    datatable(
        table_data,
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "tip",
            pageLength = 12,
            ordering = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE
        )
    ) %>%
        formatCurrency(columns = "Charge", currency = "£", digits = 2)
}


payment_history_table <- function(payment_history) {
    table_data <- payment_history %>%
        transmute(
            Date = format(date, "%d %b %Y"),
            Payment = amount
        )

    datatable(
        table_data,
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "t",
            pageLength = every_row(table_data),
            ordering = FALSE,
            autoWidth = TRUE,
            language = list(emptyTable = "No payments recorded yet.")
        )
    ) %>%
        formatCurrency(columns = "Payment", currency = "£", digits = 2)
}


balance_table <- function(balances) {
    table_data <- balances %>%
        transmute(
            Player = player,
            Charges = charges,
            Payments = payments,
            `Last payment` = if_else(
                is.na(last_payment), "\u2014", format(last_payment, "%d %b %Y")
            ),
            Balance = balance
        )

    datatable(
        table_data,
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "t",
            pageLength = every_row(table_data),
            ordering = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE,
            language = list(emptyTable = "Nobody has been charged anything yet."),
            # Picked out by row rather than by a badge in the cell: it is the
            # person that is owed or owing, not the number.
            rowCallback = JS(
                "function(row, data) {",
                "  var balance = parseFloat(data[4]);",
                "  if (balance > 0) { $(row).addClass('is-owed'); }",
                "  if (balance < 0) { $(row).addClass('is-credit'); }",
                "}"
            )
        )
    ) %>%
        formatCurrency(columns = c("Charges", "Payments", "Balance"), currency = "\u00a3", digits = 2)
}


regression_table <- function(regression_results) {
    # DT counts from zero, and only over the data columns because rownames are
    # off. Each visible outcome column is told to sort on its hidden partner.
    column_index <- function(name) match(name, names(regression_results)) - 1L
    # unname(), because a named columnDefs list is not valid DataTables options.
    visible <- unname(vapply(names(REGRESSION_TABLE_COLUMNS), column_index, integer(1)))
    hidden <- unname(vapply(unname(REGRESSION_TABLE_COLUMNS), column_index, integer(1)))

    datatable(
        regression_results,
        rownames = FALSE,
        class = "nowrap",
        options = list(
            dom = "tip",
            pageLength = 25,
            order = list(list(1, "desc")),
            autoWidth = TRUE,
            scrollX = TRUE,
            columnDefs = c(
                list(list(targets = hidden, visible = FALSE)),
                purrr::map2(
                    visible, hidden,
                    ~ list(targets = .x, orderData = .y, className = "dt-right")
                )
            )
        )
    )
}
