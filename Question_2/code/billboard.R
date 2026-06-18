# code/billboard.R
# ── Billboard Top 100 artist name extraction & matching ──────────────────────

#' Parse Billboard data: split compound artist fields, extract first names
parse_billboard_artists <- function(df) {
    df %>%
        mutate(
            chart_year  = year(date),
            artist_list = map(
                artist,
                ~ str_split(.x, " Featuring | & | feat\\.? | [Vv]s\\.? | [Xx] | [Ww]ith ") %>%
                    unlist() %>%
                    str_trim()
            )
        ) %>%
        unnest(artist_list) %>%
        mutate(first_name = map_chr(artist_list, extract_first_name)) %>%
        filter(!is.na(first_name), nchar(first_name) >= 3)
}

#' Summarise Billboard names: total weeks, peak year per artist first name
summarise_billboard <- function(parsed_billboard) {
    parsed_billboard %>%
        group_by(first_name, chart_year) %>%
        summarise(
            weeks_charting = n(),
            best_rank      = min(rank),
            .groups        = "drop"
        ) %>%
        rename(Name = first_name, Year = chart_year)
}

#' For a given artist name, build overlay data of baby counts vs chart weeks
overlay_artist_and_baby <- function(artist_name, national_df, billboard_summary, gen = "F") {
    baby <- national_df %>%
        filter(Name == artist_name, Gender == gen) %>%
        select(Year, Total)

    chart <- billboard_summary %>%
        filter(Name == artist_name) %>%
        group_by(Year) %>%
        summarise(Weeks = sum(weeks_charting), .groups = "drop")

    list(baby = baby, chart = chart)
}

#' Build combined overlay for a vector of artist names
build_music_overlay <- function(names_vec, national_df, billboard_summary) {
    map(names_vec, function(nm) {
        gen  <- if_else(nm %in% c("Elvis", "Kanye", "Drake", "Bruno"), "M", "F")
        data <- overlay_artist_and_baby(nm, national_df, billboard_summary, gen = gen)
        bind_rows(
            data$baby  %>% mutate(Series = "Baby Name Count", Value = Total, Name = nm),
            data$chart %>% mutate(Series = "Billboard Weeks",  Value = Weeks, Name = nm)
        )
    }) %>%
        list_rbind()
}
