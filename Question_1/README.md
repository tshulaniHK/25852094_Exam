# Purpose

This is the solution for the Coffee Hub analysis. The approach to the
question is as follows:

- Create the loader function to read the data from the csv file to fixe
  the encoding issues.

<!-- -->

    ##           used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells  562585 30.1    1255461 67.1   703848 37.6
    ## Vcells 1062219  8.2    8388608 64.0  1932073 14.8

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# 1. Loading the data

``` r
coffee <- load_coffee("data/Coffee/Coffee.csv")
# the load function firstly removes the encoding issues and then once its done i filter and remove any coffee item that has missing values or an NA in the observables.

# I also create a Cost variable in Rands using the exchange rate as of June 17th 2026 which was R16.17.
# lastly, i also merger the 3 reviewers feedback into 1 long review.
```

## KEYWORD MATCHING — Stellenbosch student word-cloud preferences

``` r
# This is the top words from the Stellenbosch survey that were given in the word bubble within the key words.
# The objective is to use the how many of the reviews hit or use the same words as in the ones that were top from the survey.


student_keywords <- c(
    "sweet", "chocolate", "aroma", "mouthfeel", "finish", "structure",
    "toned", "notes", "bright", "fruit", "rich", "floral", "balanced",
    "crisp", "spice", "dark", "roasted", "cocoa", "cup", "syrupy",
    "smooth", "honey", "berry", "citrus", "caramel", "vanilla", "nutty",
    "almond", "cherry"
)


coffee <- flag_keyword_matches(coffee, student_keywords) # this creates the variable kw_hits which how many of the keywords from the survey from stellenbosch were included in the reviews. 

head(coffee)
```

    ## # A tibble: 6 × 16
    ##   name          roaster roast loc_country origin_1 origin_2 Cost_Per_100g Rating
    ##   <chr>         <chr>   <chr> <chr>       <chr>    <chr>            <dbl>  <dbl>
    ## 1 "\"Sweety\" … A.R.C.  Medi… Hong Kong   Panama   Ethiopia         14.3      95
    ## 2 "Flora Blend… A.R.C.  Medi… Hong Kong   Africa   Asia Pa…          9.05     94
    ## 3 "Ethiopia Sh… Revel … Medi… United Sta… Guji Zo… Souther…          4.7      92
    ## 4 "Ethiopia Su… Roast … Medi… United Sta… Guji Zo… Oromia …          4.19     92
    ## 5 "Ethiopia Ge… Big Cr… Medi… United Sta… Gedeb D… Gedeo Z…          4.85     94
    ## 6 "Ethiopia Ka… Red Ro… Light United Sta… Odo Sha… Guji Zo…          5.14     93
    ## # ℹ 8 more variables: review_date <chr>, desc_1 <chr>, desc_2 <chr>,
    ## #   desc_3 <chr>, Cost_rands <dbl>, desc_all <chr>, kw_hits <int>,
    ## #   student_match <lgl>

# Analysis

Here i will analyse 7 key factors where i rank the coffees based of the
variabke of interest.

## Top origin

``` r
top_origins <- function(df, n = 12, min_count = 10) {
    df %>%
        group_by(origin_1) %>%
        summarise(
            avg_rating = mean(Rating),
            avg_cost   = mean(Cost_rands),
            count      = n(),
            .groups = "drop"
        ) %>%
        filter(count >= min_count) %>%
        arrange(desc(avg_rating)) %>%
        slice_head(n = n)
}

top_origins(coffee)
```

    ## # A tibble: 12 × 4
    ##    origin_1               avg_rating avg_cost count
    ##    <chr>                       <dbl>    <dbl> <int>
    ##  1 Boquete Growing Region       94.7    407.     34
    ##  2 Kiambu Growing Region        94.5     91.7    12
    ##  3 Sidama Growing Region        94.4    128.     14
    ##  4 Holualoa                     94.3    385.     41
    ##  5 Caicedonia                   94.3    217.     11
    ##  6 Bench-Maji Zone              94.1    254.     11
    ##  7 Nyeri Growing Region         94.0    112.     58
    ##  8 Kirinyaga District           94.0    120.     22
    ##  9 Aceh Province                93.8     85.7    12
    ## 10 Lintong Growing Region       93.7     78.6    19
    ## 11 Agaro Gera                   93.7     92.8    10
    ## 12 Sidamo Growing Region        93.6    518.     10
