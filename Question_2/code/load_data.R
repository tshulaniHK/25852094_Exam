
# Data loading functions with purrr::safely() error handling

safe_read_rds <- safely(read_rds, otherwise = NULL)
safe_read_csv <- safely(read_csv, otherwise = NULL)

# Safely load an .rds file — returns empty tibble on failure
load_rds <- function(path) {
    result <- safe_read_rds(path)
    if (!is.null(result$error)) {
        warning(glue("Failed to load {path}: {result$error$message}"))
        return(tibble())
    }
    result$result
}

# Aggregate baby names to national level (sum across states)
aggregate_national <- function(df) {
    df %>%
        group_by(Name, Year, Gender) %>%
        summarise(Total = sum(Count), .groups = "drop")
}
