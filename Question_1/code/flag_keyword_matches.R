
#  count how many distinct keywords appear in a string using the keywords from the survey
count_keyword_hits <- function(text, keywords) {
    text_lower <- str_to_lower(text)
    map_int(keywords, ~ as.integer(str_detect(text_lower, fixed(.x)))) %>% sum()
}

# Vectorised wrapper using map
flag_keyword_matches <- function(df, keywords) {
    df %>%
        mutate(
            kw_hits       = map_int(desc_all, ~ count_keyword_hits(.x, keywords)),
            student_match = kw_hits >= median(kw_hits)
        )
}

