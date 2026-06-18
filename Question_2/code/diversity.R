# code/diversity.R
# ── Naming diversity & decade-level summaries ────────────────────────────────

#' Compute the number of unique names needed to cover 50% of births
#' for a given year and gender
compute_diversity_50 <- function(df, yr, gen) {
    ranked <- df %>%
        filter(Year == yr, Gender == gen) %>%
        arrange(desc(Total)) %>%
        mutate(CumPct = cumsum(Total) / sum(Total))
    sum(ranked$CumPct <= 0.50) + 1L
}

#' Build diversity data frame across all year × gender combos
build_diversity_df <- function(national_df) {
    crossing(
        Year   = sort(unique(national_df$Year)),
        Gender = c("F", "M")
    ) %>%
        mutate(
            Names_for_50pct = map2_int(Year, Gender,
                                       ~ compute_diversity_50(national_df, .x, .y))
        )
}

#' Get the top-N names per decade for a given gender
top_names_by_decade <- function(df, gen, n = 10) {
    df %>%
        add_decade() %>%
        filter(Gender == gen) %>%
        group_by(Decade, Name) %>%
        summarise(Total = sum(Total), .groups = "drop") %>%
        group_by(Decade) %>%
        slice_max(order_by = Total, n = n) %>%
        mutate(Rank = row_number()) %>%
        ungroup()
}

#' Get all-time top-N names for a gender (returns character vector)
get_alltime_top <- function(df, gen, n = 10) {
    df %>%
        filter(Gender == gen) %>%
        group_by(Name) %>%
        summarise(Grand_Total = sum(Total), .groups = "drop") %>%
        slice_max(order_by = Grand_Total, n = n) %>%
        pull(Name)
}
