# code/utils.R
# ── General-purpose utility functions ────────────────────────────────────────

#' Sanitise a character vector to clean UTF-8
#' Drops invalid byte sequences and strips non-ASCII residuals
sanitise_utf8 <- function(x) {
    x %>%
        iconv(from = "UTF-8", to = "UTF-8", sub = "") %>%
        str_replace_all("[^\x01-\x7F]", "")
}

#' Extract the first capitalised name from a string
#' Handles leading "The" and trims whitespace
extract_first_name <- function(artist) {
    artist %>%
        sanitise_utf8() %>%
        str_trim() %>%
        str_remove_all("^The ") %>%
        str_extract("^[A-Z][a-z]+")
}

#' Add a Decade column (e.g. "1910s", "2000s") from a Year column
add_decade <- function(df) {
    df %>% mutate(Decade = paste0(floor(Year / 10) * 10, "s"))
}

#' Functional dataset summary — returns one-row tibble per dataset
summarise_dataset <- function(df, nm) {
    tibble(
        Dataset  = nm,
        Rows     = nrow(df),
        Columns  = ncol(df),
        ColNames = paste(names(df), collapse = ", ")
    )
}
