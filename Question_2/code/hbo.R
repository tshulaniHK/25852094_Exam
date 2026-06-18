
#  HBO credits: character & actor name extraction

#  Extract first names from HBO character and actor names
#  Sanitises UTF-8 up front to avoid encoding errors
extract_hbo_names <- function(credits_df, titles_df) {

    enriched <- credits_df %>%
        mutate(
            name      = map_chr(name,      ~ sanitise_utf8(.x) %||% NA_character_),
            character = map_chr(character,  ~ sanitise_utf8(.x) %||% NA_character_)
        ) %>%
        inner_join(
            titles_df %>% select(id, title, release_year, tmdb_score, tmdb_popularity),
            by = "id"
        )

    # Character first names
    char_names <- enriched %>%
        filter(!is.na(character), character != "") %>%
        mutate(
            first_name = map_chr(character,
                                 ~ str_extract(.x, "^[A-Z][a-z]+") %||% NA_character_)
        ) %>%
        filter(!is.na(first_name), nchar(first_name) >= 3) %>%
        select(first_name, release_year, title, tmdb_score, tmdb_popularity, role) %>%
        mutate(source = "Character")

    # Actor first names
    actor_names <- enriched %>%
        filter(!is.na(name), name != "") %>%
        mutate(first_name = map_chr(name, extract_first_name)) %>%
        filter(!is.na(first_name), nchar(first_name) >= 3) %>%
        select(first_name, release_year, title, tmdb_score, tmdb_popularity, role) %>%
        mutate(source = "Actor")

    bind_rows(char_names, actor_names)
}

#  Summarise HBO names by frequency and average TMDB score
summarise_hbo_names <- function(hbo_names_df) {
    hbo_names_df %>%
        group_by(first_name, source) %>%
        summarise(
            n_appearances = n(),
            avg_tmdb      = mean(tmdb_score, na.rm = TRUE),
            first_year    = min(release_year, na.rm = TRUE),
            .groups       = "drop"
        ) %>%
        arrange(desc(n_appearances))
}

#  Build baby name trend overlaid with HBO appearance data for spotlight names
build_hbo_baby_overlay <- function(spotlight_names, national_df, hbo_names_df) {
    map_dfr(spotlight_names, function(nm) {
        baby_trend <- national_df %>%
            filter(Name == nm) %>%
            group_by(Year) %>%
            summarise(Total = sum(Total), .groups = "drop")

        baby_trend %>% mutate(Name = nm)
    })
}
