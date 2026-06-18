# coffee palete theme: i tried to replicate a coffee based theme lol :)


coffee_palette <- c(
    "#6F4E37", "#C4A882", "#D4A574", "#A0522D", "#8B6914",
    "#D2691E", "#DEB887", "#F5DEB3", "#BC8F8F", "#CD853F",
    "#3B2314", "#F0E6D6"
)

theme_coffee <- function() {
    theme_minimal(base_size = 12) +
        theme(
            plot.background  = element_rect(fill = "#FFF8F0", colour = NA),
            panel.background = element_rect(fill = "#FFF8F0", colour = NA),
            panel.grid.major = element_line(colour = "#E8DDD0", linewidth = 0.3),
            panel.grid.minor = element_blank(),
            text             = element_text(colour = "#3B2314"),
            plot.title       = element_text(face = "bold", size = 14),
            axis.title       = element_text(size = 11),
            legend.position  = "bottom"
        )
}
