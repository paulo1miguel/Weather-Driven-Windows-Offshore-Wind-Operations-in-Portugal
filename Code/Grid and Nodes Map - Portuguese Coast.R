
rm(list = ls())

if (!requireNamespace("resourcecodedata", quietly = TRUE)) {
  install.packages("data.table")
}
if (!requireNamespace("resourcecode", quietly = TRUE)) {
  install.packages("data.table")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!requireNamespace("maps", quietly = TRUE)) install.packages("maps")

library(resourcecodedata)
library(resourcecode)
library(ggplot2)
library(lubridate)  #trabalhar com data
library(maps)
###################################################################################################

## Map of Portuguese Coast

#Load required libraries for the analysis and visualization

library(resourcecodedata) #Provides access to the RESOURCECODE data sets

library(ggplot2) #Provides functions to create plots

library(maps) #Provides geographical data for borders and base maps


# Load information about nodes (latitude, longitude, depth and D50)

data("rscd_field")

# Plotting map of nodes -  Portuguese Coast
# Geographical Limits for Portugal Coast: Longitude (-10,-6), Latitude (37,42)

ggplot() +
  borders("world", xlim = c(-10, -6), ylim = c(37, 42), colour = "gray85", fill = "gray80") +
  geom_point(data = rscd_field, aes(x = longitude, y = latitude), color = "blue", size = 0.1) +
  coord_cartesian(xlim = c(-10, -6), ylim = c(37, 42)) +  #Limits the visible area of the plot
  labs(
    title = "Grid and Nodes Map - Portuguese Coast",
    x = "Longitude (°)",
    y = "Latitude (°)"
  ) +
  theme_minimal() +  #minimalistic theme
  theme(aspect.ratio = 1)  #Ensures the x and y axes have a 1:1 ratio

