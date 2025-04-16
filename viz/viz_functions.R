# Load required packages
library(ggplot2)
library(dplyr)
library(hdatools)
library(scales)
library(ggtext)
library(air)

# 1) Bar chart showing a value across several years
hfv_year_bar_chart <- function(data, 
                               x_col, 
                               y_col, 
                               title = "Value by Year",
                               subtitle = "Subtitle",
                               color = "#334a66") {
  
  # Convert x column to character if it's not already
  if(!is.character(data[[x_col]])) {
    data[[x_col]] <- as.character(data[[x_col]])
  }
  
  ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_col(fill = color) +
    theme_minimal() +
    labs(
      title = title,
      subtitle = subtitle
    ) + 
    theme_hfv() +
    theme(
      panel.grid.minor = element_blank(),
      text = element_text(size = 12)
    )
}


# 2) Bar chart ordered by value
hfv_ordered_bar_chart <- function(data, 
                                  x_col, 
                                  y_col, 
                                  title = "Values by Category",
                                  subtitle = "subtitle", 
                                  color_strategy = "single", # "single", "multiple", or "gradient"
                                  base_color = "#4e79a7",
                                  highlight_categories = NULL) {
  
  # Reorder the x variable based on y values
  data <- data %>%
    arrange(.data[[y_col]]) %>%
    mutate(ordered_x = factor(.data[[x_col]], levels = .data[[x_col]]))
  
  # Base plot
  p <- ggplot(data, aes(x = ordered_x, y = .data[[y_col]]))
  
  # Apply color strategy
  if (color_strategy == "single") {
    # Single color for all bars
    p <- p + geom_bar(stat = "identity", fill = base_color)
    
    # Apply highlighting if specified
    if (!is.null(highlight_categories)) {
      data$highlight <- data[[x_col]] %in% highlight_categories
      p <- ggplot(data, aes(x = ordered_x, y = .data[[y_col]], 
                            fill = highlight)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("FALSE" = base_color, 
                                     "TRUE" = "#e15759"), 
                          guide = "none")
    }
  } else if (color_strategy == "multiple") {
    # Different color for each bar
    p <- p + geom_bar(stat = "identity", aes(fill = ordered_x)) +
      scale_fill_hfv() # or another color palette
  } else if (color_strategy == "gradient") {
    # Color gradient based on values
    p <- p + geom_bar(stat = "identity", 
                      aes(fill = .data[[y_col]])) +
      scale_fill_gradient(low = lighten(base_color, 0.5), 
                          high = base_color, 
                          guide = "none")
  }
  
  # Complete the plot
  p + 
    labs(
      title = title,
      subtitle = subtitle
    ) +
    theme(
      panel.grid.minor = element_blank(),
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Helper function to lighten colors
lighten <- function(color, factor = 0.5) {
  rgb <- col2rgb(color)
  rgb <- 255 - (255 - rgb) * factor
  rgb <- rgb/255
  return(rgb(rgb[1], rgb[2], rgb[3]))
}

# 3) Line graph showing a value across time from two different factors
hfv_comparison_line_graph <- function(data, 
                                      x_col, 
                                      y_col, 
                                      group_col,
                                      title = "Values Over Time by Group",
                                      subtitle = "Subtitle") {
  
  ggplot(data, aes(x = .data[[x_col]], 
                   y = .data[[y_col]], 
                   color = .data[[group_col]], 
                   group = .data[[group_col]])) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(
      title = title,
      color = "Group"
    ) +
    theme(
      panel.grid.minor = element_blank(),
      text = element_text(size = 12),
      legend.position = "bottom"
    )
}

# 4) Bar chart showing different values across different factors, ordered by factors
create_grouped_bar_chart <- function(data, 
                                     x_col, 
                                     y_col, 
                                     group_col,
                                     title = "Values by Category and Group",
                                     y_label = "Value",
                                     custom_order = NULL) {
  
  # If a custom order is provided, use it for ordering the x-axis
  if (!is.null(custom_order)) {
    data <- data %>%
      mutate(ordered_x = factor(.data[[x_col]], levels = custom_order))
  } else {
    # Otherwise, use the levels as they appear in the data
    data <- data %>%
      mutate(ordered_x = factor(.data[[x_col]], levels = unique(.data[[x_col]])))
  }
  
  ggplot(data, aes(x = ordered_x, 
                   y = .data[[y_col]], 
                   fill = .data[[group_col]])) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    labs(
      title = title,
      x = "Category",
      y = y_label,
      fill = "Group"
    ) +
    theme(
      panel.grid.minor = element_blank(),
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

# Create dummy datasets for testing visualization functions

# Load required packages
library(dplyr)
library(ggplot2)

# ----- 1. Data for Year Bar Chart -----
# Housing units built per year
year_data <- data.frame(
  year = 2015:2024,
  units_built = c(320, 450, 380, 510, 490, 420, 630, 580, 720, 650)
)

# ----- 2. Data for Ordered Bar Chart -----
# Affordable housing by county
county_data <- data.frame(
  county = c("Fairfax", "Arlington", "Henrico", "Chesterfield", 
             "Loudoun", "Richmond City", "Alexandria", "Virginia Beach"),
  affordable_units = c(8500, 4200, 3800, 5100, 2900, 7200, 3200, 6400)
)

# ----- 3. Data for Comparison Line Graph -----
# Housing costs over time for two different housing types
cost_data <- data.frame(
  year = rep(2015:2024, 2),
  cost = c(
    # Market rate housing costs
    750, 780, 820, 860, 910, 950, 990, 1050, 1100, 1180,
    # Affordable housing costs
    580, 590, 610, 630, 650, 670, 680, 700, 720, 740
  ),
  housing_type = rep(c("Market Rate", "Affordable"), each = 10)
)

# ----- 4. Data for Grouped Bar Chart -----
# Housing vacancy rates by region and housing type
vacancy_data <- data.frame(
  region = rep(c("Northern VA", "Central VA", "Tidewater", "Southwest VA"), 3),
  vacancy_rate = c(
    # Market rate housing
    2.1, 3.4, 2.8, 4.2,
    # Affordable housing
    0.8, 1.2, 1.5, 2.3,
    # Senior housing
    1.3, 1.8, 2.2, 3.1
  ),
  housing_type = rep(c("Market Rate", "Affordable", "Senior"), each = 4)
)

# ----- Examples of using the functions -----

# Example 1: Bar chart showing units built by year
year_chart <- hfv_year_bar_chart(
  year_data, 
  x_col = "year",
  y_col = "units_built",
  title = "Affordable Housing Units Built in Virginia (2015-2024)",
  subtitle = "Number of Units"
)
print(year_chart)

# Example 2: Bar chart ordered by affordable units
ordered_chart <- hfv_ordered_bar_chart(
  county_data,
  x_col = "county",
  y_col = "affordable_units",
  title = "Affordable Housing Units by County in Virginia",
  subtitle = "subtitle",
  color_strategy = "gradient",
  base_color = "#6a9fb5"
)

print(ordered_chart)

# Example 3: Line graph comparing housing costs over time
comparison_chart <- hfv_comparison_line_graph(
  cost_data,
  x_col = "year",
  y_col = "cost",
  group_col = "housing_type",
  title = "Housing Costs in Virginia (2015-2024)",
  subtitle = "subtitle"
) + 
  theme_hfv()

print(comparison_chart)

# Example 4: Grouped bar chart showing vacancy rates
# Set a custom order for regions
region_order <- c("Northern VA", "Central VA", "Tidewater", "Southwest VA")

grouped_chart <- create_grouped_bar_chart(
  vacancy_data,
  x_col = "region",
  y_col = "vacancy_rate",
  group_col = "housing_type",
  title = "Housing Vacancy Rates by Region and Type",
  y_label = "Vacancy Rate (%)",
  custom_order = region_order
)
print(grouped_chart)