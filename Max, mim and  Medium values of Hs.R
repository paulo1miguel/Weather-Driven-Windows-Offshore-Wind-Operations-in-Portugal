rm(list = ls())

if (!requireNamespace("resourcecodedata", quietly = TRUE)) {
  install.packages("resourcecodedata")
}
if (!requireNamespace("resourcecode", quietly = TRUE)) {
  install.packages("resourcecode")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("tidy", quietly = TRUE)) {
  install.packages("tidy")
}


library(resourcecodedata)  #Provides access to the RESOURCECODE data sets
library(resourcecode)      #Analyze Resourcecode data.
library(ggplot2)           #Provides functions to create plots
library(lubridate)         #Work with dates.
library(dplyr)             #Data manipulation and transformation.
library(tidyr)             #For reshaping data


# Coordinates of interest
# Longitude and Latitude for the location of interest - Viana do Castelo
coords <- c(-2.786, 47.239)

# Find the closest grid point (node) to the specified coordinates
closest_node <- closest_point_field(coords)


# Download data for the entire time range (1994–2020) for significant wave height (hs) and peak period (tp)
parameters <- get_parameters(
  parameters = c("hs", "tp"),
  node = closest_node$point,  # Closest node ID extracted using the closest_point_field function
  start = "1994-01-01 00:00:00 UTC",  # Start date for the dataset
  end = "2020-12-31 23:00:00 UTC"    # End date for the dataset
)

# Add a column for the month and calculate monthly statistics
parameters <- parameters %>%
  mutate(mes = month(time, label = TRUE, abbr = TRUE))  # Extract month names (abbreviated) from the time column

# Calculate monthly statistics: mean, maximum, and minimum significant wave heights
stats_mensais <- parameters %>%
  group_by(mes) %>%  # Group data by month
  summarise(
    mean_hs = mean(hs, na.rm = TRUE),  # Calculate the mean significant wave height for each month
    max_hs = max(hs, na.rm = TRUE),    # Find the maximum significant wave height for each month
    min_hs = min(hs, na.rm = TRUE)     # Find the minimum significant wave height for each month
  )

# Transform the data into long format for easier plotting with ggplot2
stats_long <- stats_mensais %>%
  pivot_longer(
    cols = c(mean_hs, max_hs, min_hs),  # Columns to be transformed
    names_to = "metric",                # New column name to store variable names (e.g., mean_hs, max_hs, min_hs)
    values_to = "value"                 # New column name to store variable values
  )

# Create a line plot to visualize the monthly statistics
ggplot(stats_long, aes(x = mes, y = value, color = metric, group = metric)) +
  geom_line(linewidth = 1) +  # Plot lines for each metric (mean, max, min)
  geom_point(size = 2) +  # Add points to emphasize values at each month
  labs(
    title = "Monthly Statistics of Significant Wave Heights (Hs)",
    x = "Month",
    y = "Hs (m)",
    color = "Metric"
  ) +
  scale_color_manual(
    values = c(
      "mean_hs" = "blue",   # Blue for mean values
      "max_hs" = "red",     # Red for maximum values
      "min_hs" = "green"    # Green for minimum values
    ),
    labels = c(
      "mean_hs" = "Mean",
      "max_hs" = "Maximum",
      "min_hs" = "Minimum"
    )
  ) +
  theme_minimal()  # Use a clean and minimal theme for the plot

