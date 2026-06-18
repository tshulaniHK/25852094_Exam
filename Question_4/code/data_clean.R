
# functions to load in and clean the netflix data

library(tidyverse)

# the genres and production_countries cols are stored weirdly as python lists
# like "['drama', 'crime']" so need to strip the brackets and quotes out
clean_list_col <- function(x) {
    x %>%
        str_remove_all("\\[|\\]|'") %>%
        str_trim()
}

# just grabs the first genre/country from the cleaned string
# so "drama, crime, action" becomes just "drama"
get_primary <- function(cleaned_str) {
    cleaned_str %>%
        str_split(",") %>%
        map_chr(~str_trim(.x[1]))
}

# load titles and do the cleaning in one go
load_titles <- function(path = "data/netflix/titles.rds") {
    read_rds(path) %>%
        mutate(
            genres_clean = clean_list_col(genres),
            countries_clean = clean_list_col(production_countries),
            primary_genre = get_primary(genres_clean),
            primary_country = get_primary(countries_clean),
            # force numeric on a few cols just in case
            runtime = as.numeric(runtime),
            imdb_score = as.numeric(imdb_score),
            imdb_votes = as.numeric(imdb_votes),
            tmdb_popularity = as.numeric(tmdb_popularity),
            tmdb_score = as.numeric(tmdb_score),
            release_year = as.numeric(release_year)
        )
}

# load the movie info csv - duration comes in as "90 min" so we extract the number
load_movie_info <- function(path = "data/netflix/netflix_movies.csv") {
    read_csv(path, show_col_types = FALSE) %>%
        mutate(
            duration_mins = str_extract(duration, "\\d+") %>% as.numeric(),
            release_year = as.numeric(release_year),
            listed_in_clean = str_trim(listed_in)
        )
}

# credits is straightforward, just read it in
load_credits <- function(path = "data/netflix/credits.rds") {
    read_rds(path)
}

# quick join between titles and credits on the shared id col
join_titles_credits <- function(titles_df, credits_df) {
    titles_df %>%
        inner_join(credits_df, by = "id")
}
