#load coffee to hand all the utf decoding that comes from Excel that cannot be read in R
# Load coffee and clean encoding issues. I als fiter out all missing values.
load_coffee <- function(path = "data/Coffee/Coffee.csv") {

    df <- readr::read_csv(
        path,
        locale = readr::locale(encoding = "UTF-8"),
        show_col_types = FALSE
    )

    df <- df %>%
        mutate(
            across(
                where(is.character),
                ~ stringi::stri_trans_general(.x, "Latin-ASCII")
            )
        ) %>%
        filter(
            !is.na(roast),
            !is.na(desc_1),
            !is.na(desc_2),
            !is.na(desc_3)

        ) %>%
        mutate(
            Cost_rands = Cost_Per_100g * 16.17,
            desc_all = str_c(desc_1, " ", desc_2, " ", desc_3
            )
        )

    return(df)
}

# CALL IT PROPERLY

