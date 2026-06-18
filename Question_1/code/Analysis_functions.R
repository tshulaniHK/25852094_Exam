
# These functions analyse 7 key factors



# 3a. Top origins by average rating (min n coffees)
top_origins <- function(df, n = 12, min_count = 10) {
    df %>%
        group_by(origin_1) %>%
        summarise(
            avg_rating = mean(Rating),
            avg_cost   = mean(Cost_Per_100g),
            count      = n(),
            .groups = "drop"
        ) %>%
        filter(count >= min_count) %>%
        arrange(desc(avg_rating)) %>%
        slice_head(n = n)
}

# 3b. Rating by roast type
rating_by_roast <- function(df) {
    df %>%
        group_by(roast) %>%
        summarise(
            avg_rating    = mean(Rating),
            median_rating = median(Rating),
            count         = n(),
            .groups = "drop"
        ) %>%
        arrange(desc(avg_rating))
}

# 3c. Best value coffees (high rating per unit cost)
best_value <- function(df, n = 10) {
    df %>%
        mutate(value_score = Rating / (Cost_Per_100g + 0.5)) %>%
        arrange(desc(value_score)) %>%
        slice_head(n = n) %>%
        select(name, roaster, roast, origin_1, Rating,
               Cost_Per_100g, Cost_rands, value_score)
}

# 3d. Top roasters (min n coffees)
top_roasters <- function(df, n = 10, min_coffees = 5) {
    df %>%
        group_by(roaster) %>%
        summarise(
            avg_rating = mean(Rating),
            avg_cost   = mean(Cost_Per_100g),
            coffees    = n(),
            top_rating = max(Rating),
            .groups = "drop"
        ) %>%
        filter(coffees >= min_coffees) %>%
        arrange(desc(avg_rating)) %>%
        slice_head(n = n)
}

# 3e. Student-matched top picks
student_top_picks <- function(df, n = 10) {
    df %>%
        filter(student_match) %>%
        arrange(desc(kw_hits), desc(Rating)) %>%
        slice_head(n = n) %>%
        select(name, roaster, roast, origin_1, Rating,
               Cost_Per_100g, Cost_rands, kw_hits)
}

# 3f. Cost summary by roast
cost_summary <- function(df) {
    df %>%
        group_by(roast) %>%
        summarise(
            avg_cost_usd = mean(Cost_Per_100g),
            med_cost_usd = median(Cost_Per_100g),
            avg_cost_zar = mean(Cost_rands),
            .groups = "drop"
        ) %>%
        arrange(avg_cost_usd)
}

# 3g. Roaster locations
roaster_locations <- function(df, n = 8) {
    df %>%
        group_by(loc_country) %>%
        summarise(
            count      = n(),
            avg_rating = mean(Rating),
            .groups = "drop"
        ) %>%
        arrange(desc(count)) %>%
        slice_head(n = n)
}