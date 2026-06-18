# Purpose

This is the solution for the Coffee Hub analysis. The approach to the
question is as follows:

- Create the loader function to read the data from the csv file to fixe
  the encoding issues.

<!-- -->

    ##           used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells  562782 30.1    1256024 67.1   703848 37.6
    ## Vcells 1063185  8.2    8388608 64.0  1932073 14.8

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

``` r
# here i use the analyse functions for create variables that will be sueed for summary stats
origins        <- top_origins(coffee)
roast_stats    <- rating_by_roast(coffee)
value_coffees  <- best_value(coffee)
roasters       <- top_roasters(coffee)
student_picks  <- student_top_picks(coffee)
cost_stats     <- cost_summary(coffee)
locations      <- roaster_locations(coffee)
```

# visuals

``` r
# ── Plot 1: Top Origins ──────────────────────────────────────────────────
 origins %>%
    mutate(origin_1 = fct_reorder(origin_1, avg_rating)) %>%
    ggplot(aes(x = avg_rating, y = origin_1, fill = origin_1)) +
    geom_col(show.legend = FALSE, width = 0.65) +
    geom_text(aes(label = round(avg_rating, 1)),
              hjust = -0.2, size = 3.5, colour = "#3B2314") +
    scale_fill_manual(values = coffee_palette) +
    labs(
        title = "Top Coffee Origins by Average Rating",
        x = "Average Rating", y = NULL
    ) +
    coord_cartesian(xlim = c(min(origins$avg_rating) - 0.5, NA)) +
    theme_coffee()
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# ── Plot 2: Rating by Roast (boxplot) ────────────────────────────────────
roast_order <- roast_stats %>% pull(roast)
 
coffee %>%
    mutate(roast = factor(roast, levels = roast_order)) %>%
    ggplot(aes(x = roast, y = Rating, fill = roast)) +
    geom_boxplot(alpha = 0.8, show.legend = FALSE, width = 0.55) +
    scale_fill_manual(values = coffee_palette) +
    labs(
        title = "Rating Distribution by Roast Type",
        x = NULL, y = "Rating"
    ) +
    theme_coffee()
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
# ── Plot 3: Cost vs Rating (scatter, colour = keyword hits) ──────────────
 coffee %>%
    ggplot(aes(x = Cost_Per_100g, y = Rating, colour = kw_hits)) +
    geom_point(alpha = 0.55, size = 2) +
    scale_colour_gradient(low = "#F5DEB3", high = "#6F4E37",
                          name = "Student\nKeyword Hits") +
    labs(
        title = "Cost vs Rating (colour = student keyword match strength)",
        x = "Cost per 100g (USD)", y = "Rating"
    ) +
    theme_coffee()
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
 roasters %>%
    mutate(roaster = fct_reorder(roaster, avg_rating)) %>%
    ggplot(aes(x = avg_rating, y = roaster, fill = roaster)) +
    geom_col(show.legend = FALSE, width = 0.6) +
    geom_text(aes(label = round(avg_rating, 1)),
              hjust = -0.2, size = 3.5, colour = "#3B2314") +
    scale_fill_manual(values = coffee_palette) +
    labs(
        title = "Top 10 Roasters (>= 5 coffees reviewed)",
        x = "Average Rating", y = NULL
    ) +
    coord_cartesian(xlim = c(min(roasters$avg_rating) - 0.5, NA)) +
    theme_coffee()
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
med_cost <- median(coffee$Cost_Per_100g)
 
 coffee %>%
    ggplot(aes(x = Cost_Per_100g)) +
    geom_histogram(bins = 40, fill = "#6F4E37", colour = "white", alpha = 0.85) +
    geom_vline(xintercept = med_cost, linetype = "dashed",
               colour = "#D4A574", linewidth = 1) +
    annotate("text", x = med_cost + 3, y = Inf, vjust = 2,
             label = paste0("Median: $", round(med_cost, 2)),
             colour = "#3B2314", size = 4) +
    labs(
        title = "Cost Distribution of Reviewed Coffees",
        x = "Cost per 100g (USD)", y = "Number of Coffees"
    ) +
    theme_coffee()
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)
