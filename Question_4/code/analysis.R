
# some helper functions for the actual analysis part

library(tidyverse)

# counts a column and gives back the top n - use this a lot so made it a function
count_top_n <- function(df, col, n = 10) {
    df %>%
        count({{ col }}, sort = TRUE) %>%
        slice_head(n = n)
}

# same idea but filters first before counting
count_top_n_filtered <- function(df, filter_col, filter_val, count_col, n = 10) {
    df %>%
        filter({{ filter_col }} == filter_val) %>%
        count({{ count_col }}, sort = TRUE) %>%
        slice_head(n = n)
}

# grouped summary stats - mean, median, sd, count
summarise_grouped <- function(df, group_col, value_col) {
    df %>%
        group_by({{ group_col }}) %>%
        summarise(
            mean = mean({{ value_col }}, na.rm = TRUE),
            median = median({{ value_col }}, na.rm = TRUE),
            sd = sd({{ value_col }}, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        arrange(desc(n))
}

# --- text analysis stuff: call me Mcgyver

# basic tokeniser - makes everything lowercase, removes punctuation, splits on spaces
# only keeps words with 3+ characters to avoid junk
tokenise_text <- function(text_vector) {
    text_vector %>%
        na.omit() %>%
        str_to_lower() %>%
        str_remove_all("[^a-z\\s]") %>%
        str_split("\\s+") %>%
        unlist() %>%
        discard(~.x == "" | nchar(.x) < 3)
}

# list of common english stopwords to filter out
# probably not exhaustive but good enough for our purposes
get_stopwords <- function() {
    c("the", "and", "for", "that", "with", "this", "from", "are", "was",
      "were", "been", "have", "has", "had", "will", "would", "could",
      "should", "her", "his", "its", "our", "their", "your", "who",
      "whom", "which", "what", "when", "where", "why", "how", "all",
      "each", "every", "both", "few", "more", "most", "other", "some",
      "such", "than", "too", "very", "just", "about", "above", "after",
      "again", "against", "between", "into", "through", "during", "before",
      "once", "here", "there", "then", "she", "him", "they", "them",
      "not", "but", "also", "own", "same", "does", "did", "doing",
      "you", "your", "can", "man", "one", "two", "new", "get", "way",
      "now", "may", "day", "out", "back", "find", "take", "come",
      "make", "like", "time", "only", "know", "over", "year", "years",
      "even", "still", "first", "last", "long", "great", "little", "many")
}

# full pipeline - takes a vector of text and returns a tibble of word counts
# basically tokenise -> remove stopwords -> count
get_word_freq <- function(text_vector, n = 30) {
    stops <- get_stopwords()

    text_vector %>%
        tokenise_text() %>%
        discard(~.x %in% stops) %>%
        tibble(word = .) %>%
        count(word, sort = TRUE) %>%
        slice_head(n = n)
}

# --- genre/country exploding functions ---
# these split the comma-separated strings into individual rows
# so one title with 3 genres becomes 3 rows

explode_genres <- function(df) {
    df %>%
        separate_rows(genres_clean, sep = ",\\s*") %>%
        mutate(genre = str_trim(genres_clean)) %>%
        filter(genre != "")
}

# same thing but for the listed_in column in Movie_Info
explode_listed_in <- function(df) {
    df %>%
        separate_rows(listed_in_clean, sep = ",\\s*") %>%
        mutate(category = str_trim(listed_in_clean)) %>%
        filter(category != "")
}

# and for countries
explode_countries <- function(df) {
    df %>%
        separate_rows(countries_clean, sep = ",\\s*") %>%
        mutate(country = str_trim(countries_clean)) %>%
        filter(country != "")
}
