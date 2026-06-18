# Plot helpers for Q3

# ---- Consistent theme ----
theme_q3 <- function() {
    theme_minimal() +
        theme(
            plot.title = element_text(face = "bold", size = 13),
            plot.subtitle = element_text(color = "grey40"),
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
}

# ---- Colours for loan outcomes ----
outcome_cols <- c("Fully Paid" = "#2C7A4B", "Default" = "#C0392B",
                  "Current" = "#2980B9", "Other" = "#95A5A6")
