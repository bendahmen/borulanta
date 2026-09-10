# Plots ----
#
# Every ggplot the app draws. Each takes a frame already filtered to the window
# of interest and returns a plot object, so none of them knows about Shiny.


match_plot_theme <- function() {
    theme_minimal(base_size = 12) +
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
}


season_form_plot <- function(season_form, selected_metric) {
    if (selected_metric == "points") {
        return(
            ggplot(season_form, aes(x = date, y = rolling_points)) +
                geom_line(color = "#0850AB", linewidth = 1) +
                geom_point(color = "#0850AB", size = 2.4) +
                scale_y_continuous(limits = c(0, 3)) +
                labs(y = "Points per match", x = NULL) +
                match_plot_theme()
        )
    }

    if (selected_metric == "goals") {
        plot_data <- season_form %>%
            select(date, rolling_goals_scored, rolling_goals_conceded) %>%
            pivot_longer(
                cols = -date,
                names_to = "metric",
                values_to = "value"
            ) %>%
            mutate(
                metric = recode(
                    metric,
                    rolling_goals_scored = "Goals scored",
                    rolling_goals_conceded = "Goals conceded"
                )
            )

        return(
            ggplot(plot_data, aes(x = date, y = value, color = metric)) +
                geom_line(linewidth = 1) +
                geom_point(size = 2.2) +
                scale_color_manual(
                    values = c(
                        "Goals scored" = "#0850AB",
                        "Goals conceded" = "#AB4400"
                    ),
                    name = NULL
                ) +
                labs(y = "Goals per match", x = NULL) +
                match_plot_theme()
        )
    }

    if (selected_metric == "goal_difference") {
        return(
            ggplot(season_form, aes(x = date, y = rolling_goal_difference)) +
                geom_hline(yintercept = 0, color = "#8a938d", linewidth = 0.4) +
                geom_line(color = "#006D1E", linewidth = 1) +
                geom_point(color = "#006D1E", size = 2.4) +
                labs(y = "Goal difference per match", x = NULL) +
                match_plot_theme()
        )
    }

    ggplot(season_form, aes(x = date, y = rolling_squad_size)) +
        geom_line(color = "#006D1E", linewidth = 1) +
        geom_point(color = "#006D1E", size = 2.4) +
        labs(y = "Players per match", x = NULL) +
        match_plot_theme()
}


attack_defence_plot <- function(regression_results) {
    plot_data <- regression_results %>%
        filter(outcome %in% c("goals_scored", "goals_conceded", "goal_difference")) %>%
        select(player, appearances, outcome, estimate) %>%
        pivot_wider(names_from = outcome, values_from = estimate) %>%
        mutate(defensive_effect = -goals_conceded)

    ggplot(
        plot_data,
        aes(x = goals_scored, y = defensive_effect, size = appearances, color = goal_difference)
    ) +
        geom_vline(xintercept = 0, color = "#8a938d", linewidth = 0.4) +
        geom_hline(yintercept = 0, color = "#8a938d", linewidth = 0.4) +
        geom_point(alpha = 0.8) +
        ggrepel::geom_label_repel(
            aes(label = player),
            seed = 20260820,
            size = 3.2,
            fontface = "bold",
            fill = "white",
            color = "#16241f",
            label.size = 0.2,
            box.padding = 0.45,
            point.padding = 0.3,
            min.segment.length = 0,
            max.overlaps = Inf,
            show.legend = FALSE
        ) +
        scale_color_gradient2(
            low = "#AB4400",
            mid = "#16241f",
            high = "#006D1E",
            midpoint = 0,
            name = "Goal difference"
        ) +
        scale_size_area(max_size = 14, name = "Appearances") +
        scale_x_continuous(expand = expansion(mult = 0.2)) +
        scale_y_continuous(expand = expansion(mult = 0.2)) +
        labs(
            x = "Goals scored: coefficient",
            y = "Goals conceded: coefficient, sign flipped"
        ) +
        coord_equal(clip = "off") +
        theme_minimal(base_size = 12) +
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            plot.margin = margin(12, 32, 12, 32)
        )
}


coefficient_plot <- function(regression_results, selected_outcome) {
    # Ridge returns no interval, so the layer that draws one is left out rather
    # than handed a column of NA to warn about.
    has_interval <- !all(is.na(regression_results$conf_low))
    outcome_labels <- c(
        points = "Points",
        goals_scored = "Goals scored",
        goals_conceded = "Goals conceded",
        goal_difference = "Goal difference"
    )

    plot_data <- regression_results %>%
        filter(outcome == selected_outcome) %>%
        arrange(estimate) %>%
        mutate(player = factor(player, levels = player))

    ggplot(plot_data, aes(x = player, y = estimate)) +
        geom_hline(yintercept = 0, color = "#8a938d", linewidth = 0.4) +
        (if (has_interval) {
            geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0)
        }) +
        geom_point(color = "#0850AB", size = 2.6) +
        coord_flip() +
        labs(
            x = NULL,
            y = paste0(
                outcome_labels[[selected_outcome]],
                if (has_interval) {
                    " coefficient with 95% confidence interval"
                } else {
                    ": deviation from the average player, shrunk"
                }
            )
        ) +
        theme_minimal(base_size = 12) +
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank()
        )
}


appearance_timeline_plot <- function(profile) {
    ggplot(profile$timeline, aes(x = date, y = 1, fill = played)) +
        geom_tile(height = 0.6, width = 5) +
        scale_fill_manual(
            values = c(`TRUE` = "#0850AB", `FALSE` = "#dfe3e0"),
            labels = c(`TRUE` = "Played", `FALSE` = "Missed"),
            name = NULL
        ) +
        scale_y_continuous(breaks = NULL) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(
            panel.grid = element_blank(),
            legend.position = "bottom",
            axis.text.y = element_blank()
        )
}
