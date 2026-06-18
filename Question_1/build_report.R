# =====================================================================
# build_report.R
# Builds a PowerPoint report of the Coffee Hub findings using officer.
#
# It re-uses the results already produced for README.md:
#   - the five figures knitted into README_files/figure-markdown_github/
#   - the interpretation text written up alongside each figure
#
# Run from the Question_1 directory (or open the Question_1.Rproj):
#   source("code/build_report.R")
# Output:  Coffee_Hub_Report.pptx
# =====================================================================

library(officer)

# ── Paths to the figures already produced by knitting README.rmd ──────
fig_dir <- "README_files/figure-markdown_github"
fig <- list(
    origins  = file.path(fig_dir, "unnamed-chunk-5-1.png"),  # Top origins
    roast    = file.path(fig_dir, "unnamed-chunk-6-1.png"),  # Rating by roast
    cost_rat = file.path(fig_dir, "unnamed-chunk-7-1.png"),  # Cost vs rating
    roasters = file.path(fig_dir, "unnamed-chunk-8-1.png"),  # Top roasters
    cost     = file.path(fig_dir, "unnamed-chunk-9-1.png")   # Cost distribution
)

stopifnot(all(file.exists(unlist(fig))))   # fail early if README not knitted yet

# ── Brand colours (match the coffee theme used in the plots) ──────────
brown_dark <- "#3B2314"
brown_mid  <- "#6F4E37"
cream      <- "#FFF8F0"
tan        <- "#D4A574"

# ── Text formatting helpers ───────────────────────────────────────────
fp_title  <- fp_text(font.size = 32, bold = TRUE,  color = brown_dark, font.family = "Calibri")
fp_sub    <- fp_text(font.size = 16, bold = FALSE, color = brown_mid,  font.family = "Calibri")
fp_head   <- fp_text(font.size = 26, bold = TRUE,  color = brown_dark, font.family = "Calibri")
fp_body   <- fp_text(font.size = 14, bold = FALSE, color = brown_dark, font.family = "Calibri")
fp_bullet <- fp_text(font.size = 15, bold = FALSE, color = brown_dark, font.family = "Calibri")

# Helper: turn a character vector of paragraphs into a block_list.
# An empty string "" produces a blank spacer line.
as_blocks <- function(paras, fp) {
    do.call(block_list, lapply(paras, function(p) fpar(ftext(p, fp))))
}

# Helper: add a content slide = title + image (left) + interpretation (right)
add_plot_slide <- function(doc, heading, img_path, interp) {
    doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
    # Heading
    doc <- ph_with(doc, fpar(ftext(heading, fp_head)),
                   location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.9))
    # Figure (left half)
    doc <- ph_with(doc, external_img(img_path, width = 5.2, height = 3.7),
                   location = ph_location(left = 0.4, top = 1.4, width = 5.2, height = 3.7))
    # Interpretation (right half) - one paragraph per element of interp
    doc <- ph_with(doc, as_blocks(interp, fp_body),
                   location = ph_location(left = 5.9, top = 1.4, width = 3.7, height = 5.2))
    doc
}

# ── Start the deck ────────────────────────────────────────────────────
doc <- read_pptx()

# 1. Title slide ------------------------------------------------------
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- ph_with(doc, fpar(ftext("Coffee Hub: Review Analysis", fp_title)),
               location = ph_location(left = 0.7, top = 2.2, width = 9, height = 1.2))
doc <- ph_with(doc, fpar(ftext("Profiling reviewed coffees across origin, roast, value, roaster, student taste-match and cost", fp_sub)),
               location = ph_location(left = 0.7, top = 3.4, width = 9, height = 1))

# 2. Data & method slide ----------------------------------------------
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- ph_with(doc, fpar(ftext("Data & Method", fp_head)),
               location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.9))
method_pts <- block_list(
    fpar(ftext("Loaded the raw CSV and fixed Excel/UTF-8 encoding (Latin-ASCII) so accented origin & roaster names are clean.", fp_bullet)),
    fpar(ftext("Dropped coffees missing a roast type or any of the three reviewer descriptions to avoid biased averages.", fp_bullet)),
    fpar(ftext("Engineered Cost_rands (USD x 16.17, 17 Jun 2026 rate) and desc_all (merged reviews for keyword scoring).", fp_bullet)),
    fpar(ftext("Scored each coffee against 29 top Stellenbosch student keywords (kw_hits) and flagged student_match at/above the median.", fp_bullet)),
    fpar(ftext("Summarised seven key factors; the five most decision-relevant are visualised next.", fp_bullet))
)
doc <- ph_with(doc, method_pts,
               location = ph_location(left = 0.7, top = 1.4, width = 9, height = 5))

# 3-7. Plot slides ----------------------------------------------------
doc <- add_plot_slide(
    doc, "Top Coffee Origins by Average Rating", fig$origins,
    c(
        "East African regions and a few premium single-origins lead: Boquete (Panama, ~94.6), Kiambu (Kenya, 94.5) and Sidama (Ethiopia, 94.4).",
        "",
        "All twelve top origins sit in a very narrow 93.6-94.6 band - barely one rating point apart.",
        "",
        "Origin signals quality (classic high-altitude specialty regions), but differences among the leaders are small, so origin alone weakly separates the best coffees."
    ))

doc <- add_plot_slide(
    doc, "Rating Distribution by Roast Type", fig$roast,
    c(
        "Lighter roasts score higher, almost monotonically.",
        "",
        "Light & Medium-Light have the highest medians (~93) with tight boxes; Medium / Medium-Dark sit near 92.",
        "",
        "Dark roast is the clear laggard - median ~87.5 and a much wider spread (lower AND less consistent).",
        "",
        "Takeaway: favour light / medium-light roasts for the highest, most reliable scores."
    ))

doc <- add_plot_slide(
    doc, "Cost vs Rating", fig$cost_rat,
    c(
        "Price is a poor predictor of quality.",
        "",
        "Points cluster below ~$20/100g and span almost the full rating range (88-97), so many cheap coffees score as well as expensive ones.",
        "",
        "Only a faint upward tilt with strong diminishing returns; $50-$130 coffees are not meaningfully better.",
        "",
        "Student keyword hits (colour) spread across all prices - good taste-fit is NOT confined to pricey coffees."
    ))

doc <- add_plot_slide(
    doc, "Top 10 Roasters (>= 5 coffees reviewed)", fig$roasters,
    c(
        "Hula Daddy Kona Coffee stands out at ~95.1 - the only roaster clearing 95 and visibly ahead of the field.",
        "",
        "Simon Hsieh Aroma Roast and Taster's Coffee (~94.6) and PT's Coffee Roasting Co. (94.5) lead a tight 94.0-94.3 chase pack.",
        "",
        "As with origins, leaders differ by ~one point: read roaster reputation as 'consistently excellent', with Hula Daddy the lone top outlier."
    ))

doc <- add_plot_slide(
    doc, "Cost Distribution of Reviewed Coffees", fig$cost,
    c(
        "The price distribution is heavily right-skewed.",
        "",
        "Most coffees cluster around the median of $5.86/100g, dropping off sharply after ~$15, with a long thin premium tail past $100.",
        "",
        "The mean is pulled up by those outliers, so the median is the fairer 'typical price'.",
        "",
        "The market is overwhelmingly affordable; luxury coffees sell on scarcity, not a rating premium."
    ))

# 8. Conclusion slide -------------------------------------------------
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- ph_with(doc, fpar(ftext("Overall Conclusion", fp_head)),
               location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.9))
concl_pts <- block_list(
    fpar(ftext("Quality is broad and shallow at the top: best origins (93.6-94.6) and roasters (94.0-95.1) are bunched within ~one rating point.", fp_bullet)),
    fpar(ftext("Roast level moves ratings most - lighter is better; dark roast underperforms and is erratic.", fp_bullet)),
    fpar(ftext("Origin character matters - East African and select premium single-origins lead.", fp_bullet)),
    fpar(ftext("Price barely tracks quality - most coffees ~$5.86/100g and the expensive tail buys little extra rating.", fp_bullet)),
    fpar(ftext("Recommendation for a Stellenbosch-student buyer: a light-to-medium, East-African or top-roaster coffee with high student keyword hits, at a low price - no trade-off required.", fp_text(font.size = 15, bold = TRUE, color = brown_dark)))
)
doc <- ph_with(doc, concl_pts,
               location = ph_location(left = 0.7, top = 1.4, width = 9, height = 5))

# ── Save ──────────────────────────────────────────────────────────────
out <- "Coffee_Hub_Report.pptx"
print(doc, target = out)
message("Saved report to: ", normalizePath(out))