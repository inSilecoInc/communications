# Function to plot cumulative risk with timing window polygons
plot_cumulative_risk <- function(risk_data, timing_data, title = "Cumulative Risk with Optimal Timing Windows") {
  # Load required packages
  library(ggplot2)
  library(dplyr)
  # Ensure timing_data has start and end dates for the month
  timing_data <- timing_data %>%
    mutate(
      start_date = as.Date(format(date, "%Y-%m-01")), # Start of the month
      end_date = as.Date(format(date + 31, "%Y-%m-01")) - 1 # End of the month
    )

  # Generate the plot
  ggplot(risk_data, aes(x = date, y = cum_risk, group = integrated)) +
    geom_line(color = "blue") +
    geom_point(color = "darkblue", size = 2) +
    facet_wrap(~integrated, labeller = labeller(integrated = c("TRUE" = "Integrated", "FALSE" = "Non-Integrated"))) +
    geom_rect(
      data = timing_data,
      aes(
        xmin = start_date,
        xmax = end_date,
        ymin = -Inf,
        ymax = Inf,
        fill = as.factor(integrated)
      ),
      inherit.aes = FALSE,
      alpha = 0.2
    ) +
    scale_fill_manual(
      values = c("TRUE" = "green", "FALSE" = "yellow"),
      labels = c("Integrated", "Non-Integrated"),
      name = "Timing Window"
    ) +
    labs(
      title = title,
      x = "Date",
      y = "Cumulative Risk"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 10, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
