# Utility functions for Q3: Loans and Credit

# ---- Data loader that handles encoding issues
load_loan_data <- function(path) {
    df <- tryCatch(
        read_rds(path),
        error = function(e) {
            message("Trying base R...")
            readRDS(path)
        }
    )
    df
}

# ---- Clean interest rate
clean_int_rate <- function(x) {
    x %>% as.character() %>% str_remove_all("%") %>% str_trim() %>% as.numeric()
}

# ---- Clean employment length to numeric
clean_emp_length <- function(x) {
    x %>%
        as.character() %>%
        str_replace("< 1 year", "0") %>%
        str_replace("10\\+ years", "10") %>%
        str_extract("\\d+") %>%
        as.numeric()
}

# ---- Clean term to numeric months
clean_term <- function(x) {
    x %>% as.character() %>% str_extract("\\d+") %>% as.numeric()
}

# ---- Drop columns that are mostly empty
drop_mostly_empty <- function(df, cutoff = 0.80) {
    keep <- df %>%
        summarise(across(everything(), ~mean(is.na(.)))) %>%
        pivot_longer(everything()) %>%
        filter(value < cutoff) %>%
        pull(name)

    df %>% select(all_of(keep))
}

# ---- Loan outcome classifier (TIP from brief)
classify_loan <- function(status) {
    case_when(
        str_detect(status, regex("fully paid", ignore_case = TRUE)) ~ "Fully Paid",
        str_detect(status, regex("charged off|default", ignore_case = TRUE)) ~ "Default",
        str_detect(status, regex("current", ignore_case = TRUE)) ~ "Current",
        TRUE ~ "Other"
    )
}

# ---- Quick default rate by group
default_rate_by <- function(df, group_var) {
    gv <- sym(group_var)
    df %>%
        filter(loan_outcome %in% c("Fully Paid", "Default")) %>%
        group_by(!!gv) %>%
        summarise(
            n_loans = n(),
            defaults = sum(is_default),
            default_rate = mean(is_default),
            .groups = "drop"
        ) %>%
        arrange(desc(default_rate))
}
