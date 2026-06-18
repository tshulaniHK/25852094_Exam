# code/surges.R
# ── Year-over-year surge detection ───────────────────────────────────────────

#' Compute year-over-year change for every name × gender combination
#' min_prev filters out names with tiny base counts (avoids noisy %)
compute_yoy_change <- function(df, min_prev = 50) {
    df %>%
        arrange(Name, Gender, Year) %>%
        group_by(Name, Gender) %>%
        mutate(
            Prev   = lag(Total),
            Change = Total - Prev,
            Pct    = (Total - Prev) / Prev * 100
        ) %>%
        ungroup() %>%
        filter(!is.na(Pct), Prev >= min_prev)
}

#' Curated table of names with known cultural drivers
#' Used for the cultural spikes case-study plot
cultural_names <- tribble(
    ~Name,      ~Gender, ~Event,                                     ~Event_Year,
    "Katina",   "F",     "TV: 'Where the Heart Is'",                  1974,
    "Whitney",  "F",     "Whitney Houston debut",                     1985,
    "Ariel",    "F",     "Disney: The Little Mermaid",                 1989,
    "Miley",    "F",     "TV: Hannah Montana",                        2006,
    "Aaliyah",  "F",     "Singer Aaliyah peak fame",                   1995,
    "Barack",   "M",     "Obama presidential campaign",               2008,
    "Jayden",   "M",     "Britney Spears' son (pop culture ripple)",  2006,
    "Daenerys", "F",     "TV: Game of Thrones",                       2011,
    "Elsa",     "F",     "Disney: Frozen",                            2013,
    "Khaleesi", "F",     "TV: Game of Thrones",                       2012
)

#' Build trend data for the curated cultural names from national data
build_cultural_trends <- function(national_df, cultural_tbl = cultural_names) {
    cultural_tbl %>%
        pmap_dfr(function(Name, Gender, Event, Event_Year) {
            national_df %>%
                filter(Name == !!Name, Gender == !!Gender) %>%
                mutate(Event = Event, Event_Year = Event_Year)
        })
}
