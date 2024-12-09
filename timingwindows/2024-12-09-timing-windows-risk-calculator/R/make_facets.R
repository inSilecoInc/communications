# Function to create faceted raster figures with AOI polygon
make_facets <- function(dat, name) {
  ggplot(dat, aes(x = x, y = y, fill = presence)) +
    geom_tile() +
    facet_grid(month ~ life_stage) +
    scale_fill_gradientn(
      colors = viridis::viridis(100), # Use viridis palette
      na.value = "grey",
      name = "Presence"
    ) +
    labs(
      x = "Longitude",
      y = "Latitude",
      title = name
    ) +
    coord_fixed() +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 8),
      panel.grid = element_blank()
    )
}
