# Function to calculate optimal timing window with wrapping
calculate_optimal_timing <- function(data, window_size, period_start = NULL, period_end = NULL, wrap = TRUE) {
  # Load required packages
  library(RcppRoll)
  library(dplyr)

  # Ensure the date column is in Date format
  data <- data %>%
    mutate(date = as.Date(date))

  # Wrap data if specified
  if (wrap) {
    wrapped_data <- bind_rows(data, head(data, window_size - 1))
  } else {
    wrapped_data <- data
  }

  # Filter the data if a period requirement is specified
  if (!is.null(period_start) & !is.null(period_end)) {
    period_start <- as.Date(period_start)
    period_end <- as.Date(period_end)
    period_data <- wrapped_data %>%
      filter(date >= period_start & date <= period_end)
  } else {
    period_data <- wrapped_data
  }

  # Calculate rolling sums
  wrapped_data <- wrapped_data %>%
    mutate(
      rolling_risk = roll_sum(cum_risk, n = window_size, fill = NA, align = "center")
    )

  # Find the optimal window
  if (nrow(period_data) > 0) {
    # Calculate rolling risk for the filtered period
    period_data <- period_data %>%
      mutate(
        rolling_risk = roll_sum(cum_risk, n = window_size, fill = NA, align = "center")
      )
    optimal_window <- period_data[which.min(period_data$rolling_risk), ]
  } else {
    optimal_window <- wrapped_data[which.min(wrapped_data$rolling_risk), ]
  }

  return(optimal_window)
}
