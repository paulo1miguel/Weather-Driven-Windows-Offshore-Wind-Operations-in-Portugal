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
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  install.packages("RColorBrewer")
}
if (!requireNamespace("scales", quietly = TRUE)) {
  install.packages("scales")
}

library(resourcecodedata)  #Provides access to the RESOURCECODE data sets
library(resourcecode)      #Analyze Resourcecode data.
library(ggplot2)           #Provides functions to create plots
library(lubridate)         #Work with dates.
library(scales)            #Format plot scales.
library(RColorBrewer)      #Use color palettes.
library(dplyr)             #Data manipulation and transformation.
library(patchwork)         #Combine plots.


# Coordinates of interest
# Longitude and Latitude for the specific location - Viana do Castelo.

coords <- c(-9.80, 41.48)

# Find the closest grid point (node) to the coordinates
closest_node <- closest_point_field(coords)


# Download data for the entire time range (1994–2020) for significant wave height (hs) and peak period (tp)
parameters <- get_parameters(
  parameters = c("hs", "tp"),  # Variables: significant wave height and peak period
  node = closest_node$point,  # Node ID retrieved from the closest_point_field function
  start = "1994-01-01 00:00:00 UTC",  # Start date of the dataset
  end = "2020-12-31 23:00:00 UTC"    # End date of the dataset
)


# Add a column for the month and categorize data into seasons
parameters$mes <- month(parameters$time, label = TRUE, abbr = TRUE)  # Extract the month name (abbreviated) from the date.
parameters$estacao <- case_when(
  parameters$mes %in% c("dez", "jan", "fev") ~ "Winter",  # Winter: December, January, February
  parameters$mes %in% c("mar", "abr", "mai") ~ "Spring",  # Spring: March, April, May
  parameters$mes %in% c("jun", "jul", "ago") ~ "Summer",  # Summer: June, July, August
  parameters$mes %in% c("set", "out", "nov") ~ "Autumn"   # Autumn: September, October, November
)

# Filter data for each season
winter_data <- filter(parameters, estacao == "Winter")
spring_data <- filter(parameters, estacao == "Spring")
summer_data <- filter(parameters, estacao == "Summer")
autumn_data <- filter(parameters, estacao == "Autumn")

# Define a palette of 12 distinct colors for visualization
distinct_colors <- brewer.pal(12, "Set3")

# Custom color palettes optimized for each season to improve contrast
winter_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                   "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
                   "#FC8D62", "#8DA0CB")
spring_colors <- c("#D73027", "#FC8D59", "#FEE08B", "#D9EF8B", "#91CF60",
                   "#1A9850", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8",
                   "#313695", "#A6D96A")
summer_colors <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
                   "#E6AB02", "#A6761D", "#666666", "#8C510A", "#BF812D",
                   "#DFC27D", "#F6E8C3")
autumn_colors <- c("#000000", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                   "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999",
                   "#1B9E77", "#D95F02")

# Plot for Winter
winter_plot <- ggplot(winter_data, aes(x = hs, color = mes)) +
  geom_density(linewidth = 1) +  # Density curve for significant wave height
  scale_color_manual(values = winter_colors) +  r
  labs(
    x = "Hs (m)",
    y = "Density",
    title = "Density Distribution of Hs - Winter",
    color = "Month"
  ) +
  theme_minimal()

# Plot for Spring
spring_plot <- ggplot(spring_data, aes(x = hs, color = mes)) +
  geom_density(linewidth = 1) +
  scale_color_manual(values = spring_colors) +
  labs(
    x = "Hs (m)",
    y = "Density",
    title = "Density Distribution of Hs - Spring",
    color = "Month"
  ) +
  theme_minimal()

# Plot for Summer
summer_plot <- ggplot(summer_data, aes(x = hs, color = mes)) +
  geom_density(linewidth = 1) +
  scale_color_manual(values = summer_colors) +
  labs(
    x = "Hs (m)",
    y = "Density",
    title = "Density Distribution of Hs - Summer",
    color = "Month"
  ) +
  theme_minimal()

# Plot for Autumn
autumn_plot <- ggplot(autumn_data, aes(x = hs, color = mes)) +
  geom_density(linewidth = 1) +
  scale_color_manual(values = autumn_colors) +
  labs(
    x = "Hs (m)",
    y = "Density",
    title = "Density Distribution of Hs - Autumn",
    color = "Month"
  ) +
  theme_minimal()

# Combine all four plots into a 2x2 grid
(winter_plot | spring_plot) /  # Winter and Spring on the first row
  (summer_plot | autumn_plot) +  # Summer and Autumn on the second row
  plot_annotation(
    title = "Density Distribution of Significant Wave Heights (Hs)",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

