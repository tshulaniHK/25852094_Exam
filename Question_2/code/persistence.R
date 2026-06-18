
#  Spearman rank correlation for naming persistence

#  Get top-N names for a given year and gender, ranked by Total
get_top_names <- function(df, yr, gen, n = 25) {
    df %>%
        filter(Year == yr, Gender == gen) %>%
        arrange(desc(Total)) %>%
        slice_head(n = n) %>%
        mutate(Rank = row_number()) %>%
        select(Name, Rank)
}

#  Rank ALL names for a given year and gender
rank_all_names <- function(df, yr, gen) {
    df %>%
        filter(Year == yr, Gender == gen) %>%
        arrange(desc(Total)) %>%
        mutate(Rank = row_number()) %>%
        select(Name, Rank)
}

#  Compute Spearman ρ between a base year s top-N and a target year s rankings
compute_persistence <- function(df, base_yr, target_yr, gen, n = 25) {
    base_top   <- get_top_names(df, base_yr, gen, n)
    target_all <- rank_all_names(df, target_yr, gen)

    joined <- base_top %>%
        left_join(target_all, by = "Name", suffix = c("_base", "_target")) %>%
        mutate(Rank_target = replace_na(Rank_target,
                                        n_distinct(target_all$Name) + 1L))

    if (nrow(joined) < 3) return(NA_real_)
    cor(joined$Rank_base, joined$Rank_target, method = "spearman")
}

#  Build full persistence grid: every year × lag × gender combination
#  Returns a tibble with Year, Lag, Gender, Target_Year, Correlation
build_persistence_grid <- function(df, lags = 1:3) {
    all_years <- sort(unique(df$Year))
    max_year  <- max(all_years)

    crossing(Year = all_years, Lag = lags, Gender = c("F", "M")) %>%
        filter(Year + Lag <= max_year) %>%
        mutate(
            Target_Year = Year + Lag,
            Correlation = pmap_dbl(
                list(Year, Target_Year, Gender),
                ~ compute_persistence(df, ..1, ..2, ..3, n = 25)
            )
        )
}
