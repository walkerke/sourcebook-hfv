# ---- Library Loading ----
library(tidyverse)
library(ggtext)
library(hdatools)  # Assuming this contains theme_hfv() and scale_fill_hfv()
library(ggiraph)
library(shiny)

# ---- Color and Theme Settings ----

# Central color palette definition
hfv_colors <- c(
  "primary" = "#011E41",     # Shadow - dark blue
  "secondary" = "#40C0C0",   # Sky
  "accent1" = "#259591",     # Grass
  "accent2" = "#E0592A",     # HousingX Orange
  "accent3" = "#FFC658",     # Desert
  "accent4" = "#FF7276",     # HousingX Red
  "accent5" = "#8B85CA",     # Lilac
  "accent6" = "#B1005F"      # Berry
)

# Get color palettes for different variable types
get_color_palette <- function(variable_type, variable_names) {
  if (variable_type == "tenure") {
    return(c("Homeowner" = hfv_colors["primary"], "Renter" = hfv_colors["secondary"], "All" = "#777777"))
  } else if (variable_type == "race") {
    # Create a palette with as many colors as race categories
    colors_to_use <- hfv_colors[1:min(length(variable_names), length(hfv_colors))]
    palette <- setNames(colors_to_use, variable_names)
    return(palette)
  } else if (variable_type == "age") {
    return(c(
      "17 years and under" = hfv_colors["accent3"],
      "18 to 24 years" = hfv_colors["accent2"],
      "25 to 34 years" = hfv_colors["accent1"],
      "35 to 44 years" = hfv_colors["secondary"],
      "45 to 54 years" = hfv_colors["accent5"],
      "55 to 64 years" = hfv_colors["accent6"],
      "65 years and over" = hfv_colors["accent4"]
    ))
  }
  # Default to use the full palette
  return(hfv_colors)
}

# Enhanced theme function - UPDATED to use element_markdown
theme_hfv_enhanced <- function(base_size = 12, base_family = "") {
  theme_hfv() %+replace%
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        lineheight = 0.8,
        margin = margin(t = 5)
      ),
      strip.text = element_text(size = 12, face = "bold"),
      strip.background = element_blank(),
      panel.spacing = unit(1, "lines"),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
      # Use element_markdown instead of element_text for plot title to render HTML
      plot.title = element_markdown(size = 14, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 12, hjust = 0),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
}

# ---- Data Loading Functions ----

# Function to load all datasets at once
load_all_datasets <- function() {
  datasets <- list()
  
  # Household type data
  datasets$hh_type <- read_rds("data/hh_type.rds")
  
  # Living arrangements data
  datasets$lvng_arr <- read_rds("data/lvng_arr.rds")
  
  # Household size data
  datasets$hh_size <- read_rds("data/hh_size.rds") %>%
    mutate(tenure = case_when(
      tenure == "Owner" ~ "Homeowner",
      TRUE ~ tenure
    ))
  
  # Average household size
  datasets$avg_size <- read_rds("data/avg_hh_size.rds") %>% 
    mutate(tenure = case_when(
      tenure == "Owner" ~ "Homeowner",
      TRUE ~ tenure
    ))
  
  # Median household income 
  datasets$state_minc <- read_rds("data/b25119_state.rds")
  datasets$cbsa_minc <- read_rds("data/b25119_cbsa.rds")
  datasets$local_minc <- read_rds("data/b25119_local.rds")
  
  # Income distribution
  datasets$inc_dist <- read_rds("data/b25118_data.rds")
  
  # Median income by age
  datasets$state_inc_age <- read_rds("data/b19049_state.rds")
  datasets$cbsa_inc_age <- read_rds("data/b19049_cbsa.rds")
  datasets$local_inc_age <- read_rds("data/b19049_locality.rds")
  
  # Income by race/ethnicity
  datasets$locality_rinc <- read_rds("data/b19013_locality.rds") %>% 
    mutate(locality = str_remove(locality, ", Virginia"))
  datasets$cbsa_rinc <- read_rds("data/b19013_cbsa.rds")
  datasets$state_rinc <- read_rds("data/b19013_state.rds")
  
  # Poverty by race
  datasets$poverty_race <- read_rds("data/poverty_race.rds")
  
  # Poverty by age
  datasets$poverty_age <- read_rds("data/poverty_age.rds")
  
  # Housing type by tenure
  datasets$b25032 <- read_rds("data/b25032.rds")
  
  # Housing by year built and structure
  datasets$b25127 <- read_rds("data/b25127.rds")
  
  # Tenure by bedrooms
  datasets$b25042 <- read_rds("data/b25042.rds")
  
  # Overcrowding
  datasets$b25014 <- read_rds("data/b25014.rds")
  
  # Homeownership rate
  datasets$b25003_state <- read_rds("data/b25003_state.rds") %>% 
    mutate(ho_rate = est_owner/est_all)
  
  # VA lookup table
  datasets$va_lookup <- read_csv("data/va-cbsa-locality-lookup.csv") %>% 
    mutate(fips = as.character(fips_full))
  
  # Create lookup lists for UI
  datasets$cbsa_list <- sort(unique(datasets$hh_type$cbsa_title))
  datasets$locality_list <- sort(unique(datasets$hh_type$name_long))
  datasets$year_list <- sort(unique(datasets$hh_type$year))
  
  return(datasets)
}

# ---- Data Processing Helper Functions ----

# Filter data by geography and year
filter_data <- function(data, geo_level, geo_name = NULL, year = NULL) {
  filtered <- data
  
  if (!is.null(year)) {
    filtered <- filtered %>% filter(year == year)
  }
  
  if (geo_level == "state") {
    return(filtered)
  } else if (geo_level == "cbsa" && !is.null(geo_name)) {
    return(filtered %>% filter(cbsa_title == geo_name))
  } else if (geo_level == "locality" && !is.null(geo_name)) {
    return(filtered %>% filter(name_long == geo_name))
  }
  
  return(filtered)
}

# Calculate percentages within groups
calculate_percentages <- function(data, group_vars, value_var = "estimate") {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(percent = !!sym(value_var) / sum(!!sym(value_var))) %>%
    ungroup()
}

# Calculate percent changes between years
calculate_percent_change <- function(data, group_vars, ref_year, comp_year, value_var = "estimate") {
  data %>%
    filter(year %in% c(ref_year, comp_year)) %>%
    mutate(year = as.character(year)) %>%
    arrange(across(c(all_of(group_vars), year))) %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(pct_change = case_when(
      year == as.character(comp_year) ~ ((!!sym(value_var) - first(!!sym(value_var))) / first(!!sym(value_var))),
      TRUE ~ NA_real_
    )) %>%
    ungroup()
}

# ---- Visualization Helper Functions ----

# Function for bar charts with percentage labels - UPDATED to use element_markdown
create_percentage_bar_chart <- function(data, x_var, y_var = "percent", fill_var, title_text, 
                                        facet_var = NULL, coord_flip = FALSE, label_accuracy = 1,
                                        subtitle_text = NULL) {
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var))) +
    geom_col() +
    geom_text(aes(label = scales::percent(!!sym(y_var), accuracy = label_accuracy),
                  color = !!sym(fill_var)),
              position = position_dodge(width = 0.9),
              vjust = -0.5,
              size = 3.5) +
    scale_color_hfv() +
    scale_fill_hfv() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = title_text, subtitle = subtitle_text) +
    theme_hfv() +
    guides(color = "none") +
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        lineheight = 0.8,
        margin = margin(t = 5)
      ),
      plot.title = element_markdown() # Added this to render HTML in titles
    )
  
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!sym(facet_var)), scales = "free_x", space = "free")
  }
  
  if (coord_flip) {
    p <- p + coord_flip()
  }
  
  return(p)
}

# Function for comparison bar charts (e.g., different years) - UPDATED to use element_markdown
create_comparison_bar_chart <- function(data, x_var, y_var = "estimate", fill_var, 
                                        facet_var, title_text, show_pct_change = TRUE) {
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var))) + 
    geom_col() +
    facet_wrap(vars(!!sym(facet_var)), nrow = 1) +
    theme_hfv() +
    scale_fill_hfv() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.spacing = unit(1, "lines"),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
      strip.background = element_blank(),
      plot.title = element_markdown() # Added this to render HTML in titles
    ) +
    scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    labs(title = title_text)
  
  # Add percent change labels if requested
  if (show_pct_change && "pct_change" %in% names(data)) {
    latest_year <- max(as.numeric(as.character(data[[x_var]])))
    p <- p + geom_text(
      data = filter(data, as.character(!!sym(x_var)) == as.character(latest_year)),
      aes(label = scales::percent(pct_change, accuracy = 0.1)),
      position = position_stack(),
      vjust = -0.5,
      size = 3.5
    )
  }
  
  return(p)
}

# Function for line charts - UPDATED to use element_markdown
create_line_chart <- function(data, x_var, y_var, color_var = NULL, group_var = NULL,
                              title_text, subtitle_text = NULL, use_smooth = TRUE, 
                              y_format = scales::number_format(accuracy = 0.01),
                              facet_var = NULL) {
  
  # Set up basic aes mapping
  if (is.null(color_var) && is.null(group_var)) {
    p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var)))
  } else if (is.null(group_var)) {
    p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(color_var)))
  } else {
    p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), 
                          color = !!sym(color_var), group = !!sym(group_var)))
  }
  
  # Add layers
  p <- p + 
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(title = title_text, subtitle = subtitle_text) +
    scale_y_continuous(labels = y_format) +
    theme_hfv() +
    theme(plot.title = element_markdown()) # Added this to render HTML in titles
  
  # Add smooth if requested
  if (use_smooth) {
    p <- p + geom_smooth(method = "loess", se = TRUE, color = "#40C0C0", fill = "#40C0C0", alpha = 0.2)
  }
  
  # Add facet if provided
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!sym(facet_var)), nrow = 1)
  }
  
  return(p)
}

# ---- Data Processing Functions for Specific Visualizations ----

# Household Type Data Processing
process_household_type_data <- function(hh_type) {
  # Locality level
  locality_hh <- hh_type %>% 
    group_by(year, name_long) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # CBSA level
  cbsa_hh <- hh_type %>% 
    group_by(year, cbsa_title, type, subtype) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    group_by(year, cbsa_title) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # State level
  state_hh <- hh_type %>% 
    group_by(year, type, subtype) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    group_by(year) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # Return processed data
  return(list(
    locality = locality_hh,
    cbsa = cbsa_hh,
    state = state_hh
  ))
}

# Living Arrangements Data Processing
process_living_arrangements_data <- function(lvng_arr) {
  # Locality level
  locality_la <- lvng_arr %>% 
    group_by(year, name_long) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # CBSA level
  cbsa_la <- lvng_arr %>% 
    group_by(year, cbsa_title, age, type) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    group_by(year, cbsa_title) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # State level
  state_la <- lvng_arr %>% 
    group_by(year, age, type) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    group_by(year) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # Return processed data
  return(list(
    locality = locality_la,
    cbsa = cbsa_la,
    state = state_la
  ))
}

# Household Size Data Processing
process_household_size_data <- function(hh_size) {
  # Locality level
  locality_size <- hh_size %>% 
    pivot_wider(
      id_cols = c(year, name_long, hhsize),
      names_from = tenure,
      values_from = estimate
    ) %>% 
    mutate(All = Renter + Homeowner) %>% 
    pivot_longer(
      cols = c(Renter, Homeowner, All),
      names_to = "tenure",
      values_to = "estimate"
    ) %>%
    arrange(name_long, tenure, year) %>% 
    group_by(name_long, tenure, hhsize) %>% 
    mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) %>% 
    group_by(year, name_long, tenure) %>% 
    mutate(percent = estimate/sum(estimate)) %>% 
    ungroup()
  
  # CBSA level
  cbsa_size <- hh_size %>% 
    group_by(year, cbsa_title, tenure, hhsize) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    pivot_wider(
      id_cols = c(year, cbsa_title, hhsize),
      names_from = tenure,
      values_from = estimate
    ) %>% 
    mutate(All = Renter + Homeowner) %>% 
    pivot_longer(
      cols = c(Renter, Homeowner, All),
      names_to = "tenure",
      values_to = "estimate"
    ) %>% 
    arrange(cbsa_title, tenure, year) %>% 
    group_by(cbsa_title, tenure, hhsize) %>% 
    mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) %>% 
    group_by(year, cbsa_title, tenure) %>% 
    mutate(percent = estimate/sum(estimate)) %>%
    ungroup()
  
  # State level
  state_size <- hh_size %>%
    group_by(year, tenure, hhsize) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>%  
    pivot_wider(
      id_cols = c(year, hhsize),
      names_from = tenure,
      values_from = estimate
    ) %>% 
    mutate(All = Renter + Homeowner) %>% 
    pivot_longer(
      cols = c(Renter, Homeowner, All),
      names_to = "tenure",
      values_to = "estimate"
    ) %>%
    group_by(year, tenure, hhsize) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    arrange(tenure, year) %>% 
    group_by(tenure, hhsize) %>% 
    mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) %>% 
    group_by(year, tenure) %>% 
    mutate(percent = estimate/sum(estimate)) %>% 
    ungroup()
  
  # Return processed data
  return(list(
    locality = locality_size,
    cbsa = cbsa_size,
    state = state_size
  ))
}

# Average Household Size Data Processing
process_avg_household_size_data <- function(avg_size) {
  # Separate by geography
  locality <- avg_size %>% filter(geography == "locality")
  cbsa <- avg_size %>% filter(geography == "cbsa")
  state <- avg_size %>% filter(geography == "state")
  
  # Return processed data
  return(list(
    locality = locality,
    cbsa = cbsa,
    state = state
  ))
}

# Income Distribution Data Processing
process_income_distribution_data <- function(inc_dist) {
  # Define income order
  income_order <- c("Less than $15,000", "$15,000 to $24,999", "$25,000 to $49,999",
                    "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $149,999",
                    "$150,000 or more")
  
  # State level
  state <- inc_dist %>% 
    group_by(year, tenure, income) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    mutate(income = factor(income, levels = income_order))
  
  # CBSA level
  cbsa <- inc_dist %>% 
    group_by(year, cbsa_title, tenure, income) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    mutate(income = factor(income, levels = income_order))
  
  # Local level
  local <- inc_dist %>% 
    mutate(income = factor(income, levels = income_order))
  
  # Return processed data
  return(list(
    state = state,
    cbsa = cbsa,
    locality = local
  ))
}

# Housing Type Data Processing
process_housing_type_data <- function(b25032) {
  # State level
  state_housing <- b25032 %>% 
    group_by(year, tenure, type) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    group_by(year, tenure) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # CBSA level
  cbsa_housing <- b25032 %>% 
    group_by(year, cbsa_title, tenure, type) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    group_by(year, cbsa_title, tenure) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # Local level
  local_housing <- b25032 %>% 
    group_by(year, name_long, tenure) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # Return processed data
  return(list(
    state = state_housing,
    cbsa = cbsa_housing,
    locality = local_housing
  ))
}

# Housing Type by Year Built Data Processing
process_housing_by_year_built <- function(b25127) {
  # Define the desired order for structure types
  structure_order <- c("1, detached or attached", 
                       "2 to 4", 
                       "5 to 19", 
                       "20 to 49", 
                       "50 or more", 
                       "Mobile home, boat, RV, van, etc.") 
  
  # State level
  state_housing_built <- b25127 %>% 
    group_by(year, tenure, yrbuilt, structure) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    ungroup() %>% 
    mutate(structure = factor(structure, levels = structure_order))
  
  # CBSA level
  cbsa_housing_built <- b25127 %>% 
    group_by(year, cbsa_title, tenure, yrbuilt, structure) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    ungroup() %>% 
    mutate(structure = factor(structure, levels = structure_order))
  
  # Local level
  local_housing_built <- b25127 %>% 
    mutate(structure = factor(structure, levels = structure_order))
  
  # Return processed data
  return(list(
    state = state_housing_built,
    cbsa = cbsa_housing_built,
    locality = local_housing_built
  ))
}

# Bedrooms by Tenure Data Processing
process_bedrooms_by_tenure <- function(b25042) {
  # Define the desired order for bedrooms
  bedroom_order <- c("No bedroom", "1 bedroom", "2 bedrooms", "3 bedrooms", 
                     "4 bedrooms", "5 or more bedrooms")
  
  # State level
  state_bed <- b25042 %>% 
    group_by(year, tenure, br) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    mutate(br = factor(br, levels = bedroom_order))
  
  # CBSA level
  cbsa_bed <- b25042 %>% 
    group_by(year, cbsa_title, tenure, br) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    mutate(br = factor(br, levels = bedroom_order))
  
  # Local level
  local_bed <- b25042 %>% 
    group_by(year, name_long, tenure, br) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    mutate(br = factor(br, levels = bedroom_order))
  
  # Return processed data
  return(list(
    state = state_bed,
    cbsa = cbsa_bed,
    locality = local_bed
  ))
}

# Overcrowding Data Processing
process_overcrowding_data <- function(b25014) {
  # State level
  state_crowd <- b25014 %>% 
    group_by(year, tenure, overcrowded) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    ungroup() %>% 
    group_by(year, tenure) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # CBSA level
  cbsa_crowd <- b25014 %>% 
    group_by(year, cbsa_title, tenure, overcrowded) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    ungroup() %>% 
    group_by(year, cbsa_title, tenure) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # Local level
  local_crowd <- b25014 %>% 
    group_by(year, name_long, tenure, opr, overcrowded) %>% 
    summarise(estimate = sum(estimate), .groups = "drop") %>% 
    ungroup() %>% 
    group_by(year, name_long, tenure) %>% 
    mutate(percent = estimate/sum(estimate))
  
  # Return processed data
  return(list(
    state = state_crowd,
    cbsa = cbsa_crowd,
    locality = local_crowd
  ))
}

# ---- Visualization Functions for Specific Data Types ----

# Household Type Visualization - UPDATED to use element_markdown
plot_household_type <- function(data, geo_level, geo_name = NULL, selected_year = 2023) {
  # Filter data based on parameters
  if (geo_level == "locality") {
    plot_data <- filter_data(data$locality, "locality", geo_name, selected_year) %>%
      group_by(type) %>%
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
      ungroup()
    subtitle <- plot_data$name_long[1]
  } else if (geo_level == "cbsa") {
    plot_data <- filter_data(data$cbsa, "cbsa", geo_name, selected_year) %>%
      group_by(type) %>%
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
      ungroup()
    subtitle <- plot_data$cbsa_title[1]
  } else { # state
    plot_data <- filter_data(data$state, "state", NULL, selected_year) %>%
      group_by(type) %>%
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
      ungroup()
    subtitle <- "Virginia"
  }
  
  title_text <- "<b><span style='color:#011E41'>Householder with no partner</span></b> and 
                 <b><span style='color:#40C0C0'>Married or cohabitating couple</span></b>"
  
  # Create visualization
  ggplot(plot_data,
         aes(x = reorder(subtype, rank_within_type),
             y = percent,
             fill = type)) + 
    geom_col() +
    geom_text(aes(label = scales::percent(percent, accuracy = 1),
                  color = type),
              position = position_dodge(width = 0.9),
              vjust = -0.5,
              size = 3.5) +
    scale_color_hfv() +
    labs(title = title_text,
         subtitle = subtitle) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_hfv() +
    scale_fill_hfv() +
    guides(color = "none") +
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        lineheight = 0.8,
        margin = margin(t = 5)
      ),
      strip.text = element_blank(),
      plot.title = element_markdown() # Added this to render HTML in titles
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    facet_grid(cols = vars(type), scales = "free_x", space = "free")
}

# Living Arrangements Visualization - UPDATED to use element_markdown
plot_living_arrangements <- function(data, geo_level, geo_name = NULL, selected_year = 2023, 
                                     selected_age = "All ages") {
  # Filter data based on parameters
  if (geo_level == "locality") {
    plot_data <- filter_data(data$locality, "locality", geo_name, selected_year) %>%
      filter(age == selected_age)
    subtitle <- plot_data$name_long[1]
  } else if (geo_level == "cbsa") {
    plot_data <- filter_data(data$cbsa, "cbsa", geo_name, selected_year) %>%
      filter(age == selected_age)
    subtitle <- plot_data$cbsa_title[1]
  } else { # state
    plot_data <- filter_data(data$state, "state", NULL, selected_year) %>%
      filter(age == selected_age)
    subtitle <- "Virginia"
  }
  
  title_text <- "<b><span style='color:#011E41'>Living arrangement of adults</span></b>"
  
  # Create visualization
  ggplot(plot_data,
         aes(x = reorder(type, percent),
             y = percent,
             fill = type)) + 
    geom_col() +
    geom_text(aes(label = scales::percent(percent, accuracy = 1),
                  color = type),
              position = position_dodge(width = 0.9),
              vjust = -0.5,
              size = 3.5) +
    scale_color_hfv() +
    labs(title = title_text,
         subtitle = subtitle) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_hfv() +
    scale_fill_hfv() +
    guides(color = "none") +
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        lineheight = 0.8,
        margin = margin(t = 5)
      ),
      strip.text = element_blank(),
      plot.title = element_markdown() # Added this to render HTML in titles
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
}

# Household Size Visualization - UPDATED to use element_markdown
plot_household_size <- function(data, geo_level, geo_name = NULL, 
                                start_year = 2010, end_year = 2023, 
                                selected_tenure = "All") {
  
  # Filter data based on parameters
  if (geo_level == "locality") {
    plot_data <- filter_data(data$locality, "locality", geo_name) %>%
      filter(year == start_year | year == end_year,
             tenure == selected_tenure) %>%
      mutate(year = as.character(year))
    title <- paste(selected_tenure, "households by size in", geo_name)
  } else if (geo_level == "cbsa") {
    plot_data <- filter_data(data$cbsa, "cbsa", geo_name) %>%
      filter(year == start_year | year == end_year,
             tenure == selected_tenure) %>%
      mutate(year = as.character(year))
    title <- paste(selected_tenure, "households by size in", geo_name)
  } else { # state
    plot_data <- filter_data(data$state, "state") %>%
      filter(year == start_year | year == end_year,
             tenure == selected_tenure) %>%
      mutate(year = as.character(year))
    title <- paste(selected_tenure, "households by size in Virginia")
  }
  
  # Recalculate percent change for the filtered data
  plot_data <- plot_data %>%
    arrange(year, hhsize) %>%
    group_by(hhsize) %>%
    mutate(pct_change = case_when(
      year == as.character(end_year) ~ ((estimate - first(estimate)) / first(estimate)),
      TRUE ~ NA_real_
    )) %>%
    ungroup()
  
  # Create visualization
  ggplot(plot_data,
         aes(x = year,
             y = estimate,
             fill = year)) + 
    geom_col() +
    facet_wrap(~hhsize, nrow = 1) +
    geom_text(
      data = filter(plot_data, year == as.character(end_year)),
      aes(label = scales::percent(pct_change, accuracy = 0.1)),
      position = position_stack(),
      vjust = -0.5,
      size = 3.5
    ) +
    theme_hfv() +
    scale_fill_hfv() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.spacing = unit(1, "lines"),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
      strip.background = element_blank(),
      plot.title = element_markdown() # Added this to render HTML in titles
    ) +
    scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    labs(title = title)
}

# Average Household Size Visualization - UPDATED to use element_markdown
plot_avg_household_size <- function(data, geo_level, geo_name = NULL, selected_tenure = "All") {
  # Filter data based on parameters
  if (geo_level == "locality") {
    plot_data <- filter_data(data$locality, "locality") %>%
      filter(name == geo_name, tenure == selected_tenure)
    title <- paste(selected_tenure, "Average Household Size Over Time")
    subtitle <- geo_name
  } else if (geo_level == "cbsa") {
    plot_data <- filter_data(data$cbsa, "cbsa") %>%
      filter(name == geo_name, tenure == selected_tenure)
    title <- paste(selected_tenure, "Average Household Size Over Time")
    subtitle <- geo_name
  } else { # state
    plot_data <- filter_data(data$state, "state") %>%
      filter(tenure == selected_tenure)
    title <- paste(selected_tenure, "Average Household Size in Virginia")
    subtitle <- "2010-2023"
  }
  
  # For state level, add labels for min and max points
  if (geo_level == "state") {
    plot_data <- plot_data %>%
      mutate(label_point = year == min(year) | year == max(year) | 
               estimate == max(estimate) | estimate == min(estimate))
    
    p <- ggplot(plot_data,
                aes(x = year,
                    y = estimate)) +
      geom_line(linewidth = 1, color = "#011E41") +
      geom_point(size = 3, color = "#011E41") +
      geom_smooth(method = "loess", se = TRUE, color = "#40C0C0", fill = "#40C0C0", alpha = 0.2) +
      geom_text(data = filter(plot_data, label_point),
                aes(label = scales::number(estimate, accuracy = 0.01)),
                vjust = -0.8, hjust = 0.5, size = 3.5)
  } else {
    p <- ggplot(plot_data,
                aes(x = year,
                    y = estimate)) +
      geom_line(linewidth = 1, color = "#011E41") +
      geom_point(size = 3, color = "#011E41") +
      geom_smooth(method = "loess", se = TRUE, color = "#40C0C0", fill = "#40C0C0", alpha = 0.2)
  }
  
  # Add common elements
  p <- p +
    labs(title = title,
         subtitle = subtitle,
         x = "Year",
         y = "Average Household Size") +
    scale_y_continuous(limits = c(min(plot_data$estimate) * 0.95, max(plot_data$estimate) * 1.05),
                       labels = scales::number_format(accuracy = 0.01)) +
    theme_hfv() +
    theme(plot.title = element_markdown()) # Added this to render HTML in titles
  
  return(p)
}

# Income Distribution Visualization - UPDATED to use element_markdown
plot_income_distribution <- function(data, geo_level, geo_name = NULL, selected_year = 2023) {
  # Filter data based on parameters
  if (geo_level == "locality") {
    plot_data <- filter_data(data$locality, "locality", geo_name, selected_year)
    title <- paste("Income Distribution by Tenure in", geo_name)
  } else if (geo_level == "cbsa") {
    plot_data <- filter_data(data$cbsa, "cbsa", geo_name, selected_year)
    title <- paste("Income Distribution by Tenure in", geo_name)
  } else { # state
    plot_data <- filter_data(data$state, "state", NULL, selected_year)
    title <- "Income Distribution by Tenure in Virginia"
  }
  
  # Create visualization
  ggplot(plot_data,
         aes(
           x = income,
           y = estimate,
           fill = tenure)) +
    geom_col() +
    facet_wrap(~tenure, ncol = 1) +
    scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    theme_hfv() +
    scale_fill_hfv() +
    theme(
      strip.text = element_blank(),
      axis.text.x = element_text(angle = 45,
                                 hjust = 1,
                                 vjust = 1,
                                 size = 10,
                                 lineheight = 0.9),
      plot.title = element_markdown() # Added this to render HTML in titles
    ) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
    labs(title = title)
}

# Housing Type by Tenure Visualization - UPDATED to use element_markdown
plot_housing_type <- function(data, geo_level, geo_name = NULL, selected_year = 2023) {
  # Filter data based on parameters
  if (geo_level == "locality") {
    plot_data <- filter_data(data$locality, "locality", geo_name, selected_year)
    title <- paste("Housing Type by Tenure in", geo_name)
  } else if (geo_level == "cbsa") {
    plot_data <- filter_data(data$cbsa, "cbsa", geo_name, selected_year)
    title <- paste("Housing Type by Tenure in", geo_name)
  } else { # state
    plot_data <- filter_data(data$state, "state", NULL, selected_year)
    title <- "Housing Type by Tenure in Virginia"
  }
  
  title_text <- "<b><span style='color:#011E41'>Homeowner</span></b> and 
                <b><span style='color:#40C0C0'>renter</span></b> households by housing type"
  
  # Create visualization
  ggplot(plot_data, 
         aes(x = reorder(type, -percent), 
             y = percent, 
             fill = tenure)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = scales::percent(percent, accuracy = 1), 
                  color = tenure),
              position = position_dodge(width = 0.9),
              hjust = -0.2) +
    facet_wrap(~tenure) +
    scale_fill_hfv() + 
    scale_color_hfv() +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.2))) +
    labs(title = title_text) +
    theme_hfv() +
    theme(legend.position = "none",
          strip.text = element_blank(),
          plot.title = element_markdown()) # Added this to render HTML in titles
}

# Housing Type by Year Built Visualization - UPDATED to use element_markdown
plot_housing_by_year_built <- function(data, geo_level, geo_name = NULL, selected_year = 2023, 
                                       selected_tenure = "All") {
  # Filter data based on parameters
  if (geo_level == "locality") {
    plot_data <- filter_data(data$locality, "locality", geo_name, selected_year) %>%
      filter(tenure == selected_tenure)
    title <- paste("Distribution of", selected_tenure, "housing by year built in", geo_name)
  } else if (geo_level == "cbsa") {
    plot_data <- filter_data(data$cbsa, "cbsa", geo_name, selected_year) %>%
      filter(tenure == selected_tenure)
    title <- paste("Distribution of", selected_tenure, "housing by year built in", geo_name)
  } else { # state
    plot_data <- filter_data(data$state, "state", NULL, selected_year) %>%
      filter(tenure == selected_tenure)
    title <- paste("Distribution of", selected_tenure, "housing by year built in Virginia")
  }
  
  # Define a color palette for structure types
  housing_palette <- c(
    "1, detached or attached" = "#011E41",
    "2 to 4" = "#259591",
    "5 to 19" = "#8B85CA",
    "20 to 49" = "#B1005F",
    "50 or more" = "#E0592A",
    "Mobile home, boat, RV, van, etc." = "#FFC658"
  )
  
  # Create visualization
  ggplot(plot_data,
         aes(x = yrbuilt,
             y = estimate,
             fill = structure)) +
    geom_col(position = "stack") +
    facet_wrap(~tenure) +
    coord_flip() +
    scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    labs(title = title) +
    theme_minimal() +
    scale_fill_manual(values = housing_palette) +
    theme(legend.title = element_blank(),
          axis.title = element_blank(),
          plot.title.position = "plot",
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_markdown() # Added this to render HTML in titles
    )
}

# Bedrooms by Tenure Visualization - UPDATED to use element_markdown
plot_bedrooms_by_tenure <- function(data, geo_level, geo_name = NULL, selected_year = 2023) {
  # Filter data based on parameters
  if (geo_level == "locality") {
    plot_data <- filter_data(data$locality, "locality", geo_name, selected_year)
    title <- paste("Distribution of housing by bedroom count and tenure in", geo_name)
  } else if (geo_level == "cbsa") {
    plot_data <- filter_data(data$cbsa, "cbsa", geo_name, selected_year)
    title <- paste("Distribution of housing by bedroom count and tenure in", geo_name)
  } else { # state
    plot_data <- filter_data(data$state, "state", NULL, selected_year)
    title <- "Distribution of housing by bedroom count and tenure in Virginia"
  }
  
  # Create visualization
  ggplot(plot_data,
         aes(
           x = br,
           y = estimate,
           fill = tenure)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~tenure) +
    scale_fill_hfv() +
    scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_markdown(),
      plot.title.position = "plot",
      legend.position = "none",
      axis.title = element_blank()
    )
}

# Overcrowding Visualization - UPDATED to use element_markdown
plot_overcrowding <- function(data, geo_level, geo_name = NULL, selected_year = 2023) {
  # Filter data based on parameters
  if (geo_level == "locality") {
    plot_data <- filter_data(data$locality, "locality", geo_name, selected_year) %>%
      filter(overcrowded != "Not overcrowded")
    title <- paste("Housing Overcrowding Rates by Tenure in", geo_name)
  } else if (geo_level == "cbsa") {
    plot_data <- filter_data(data$cbsa, "cbsa", geo_name, selected_year) %>%
      filter(overcrowded != "Not overcrowded")
    title <- paste("Housing Overcrowding Rates by Tenure in", geo_name)
  } else { # state
    plot_data <- filter_data(data$state, "state", NULL, selected_year) %>%
      filter(overcrowded != "Not overcrowded")
    title <- "Housing Overcrowding Rates by Tenure in Virginia"
  }
  
  # Create visualization
  ggplot(plot_data,
         aes(
           x = overcrowded,
           y = percent,
           fill = tenure)) +
    geom_col(position = "dodge") +
    scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, 0.03),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_fill_hfv() +
    labs(
      title = title,
      subtitle = "Renters experience higher overcrowding rates than homeowners",
      x = "Overcrowding Category",
      y = "Percent of Households",
      fill = "Housing Tenure"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_markdown() # Added this to render HTML in titles
    )
}

# ---- Shiny Modules for Each Visualization ----

# Module for Household Type
householdTypeUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$hh_type$year), decreasing = TRUE),
                         selected = max(data$hh_type$year)),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

householdTypeServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Process household type data
    processed_data <- reactive({
      process_household_type_data(all_data$hh_type)
    })
    
    # Render plots
    output$state_plot <- renderPlot({
      plot_household_type(processed_data(), "state", selected_year = input$year)
    })
    
    output$cbsa_plot <- renderPlot({
      plot_household_type(processed_data(), "cbsa", input$cbsa, input$year)
    })
    
    output$locality_plot <- renderPlot({
      plot_household_type(processed_data(), "locality", input$locality, input$year)
    })
  })
}

# Module for Living Arrangements
livingArrangementsUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$lvng_arr$year), decreasing = TRUE),
                         selected = max(data$lvng_arr$year)),
             selectInput(ns("age"), "Select Age Group:", 
                         choices = sort(unique(data$lvng_arr$age)),
                         selected = "All ages"),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

livingArrangementsServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Process living arrangements data
    processed_data <- reactive({
      process_living_arrangements_data(all_data$lvng_arr)
    })
    
    # Render plots
    output$state_plot <- renderPlot({
      plot_living_arrangements(processed_data(), "state", selected_year = input$year, 
                               selected_age = input$age)
    })
    
    output$cbsa_plot <- renderPlot({
      plot_living_arrangements(processed_data(), "cbsa", input$cbsa, input$year, input$age)
    })
    
    output$locality_plot <- renderPlot({
      plot_living_arrangements(processed_data(), "locality", input$locality, input$year, input$age)
    })
  })
}

# Module for Household Size
householdSizeUI <- function(id, data) {
  ns <- NS(id)
  
  min_year <- min(data$hh_size$year)
  max_year <- max(data$hh_size$year)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("start_year"), "Start Year:", 
                         choices = sort(unique(data$hh_size$year)),
                         selected = min_year),
             selectInput(ns("end_year"), "End Year:", 
                         choices = sort(unique(data$hh_size$year), decreasing = TRUE),
                         selected = max_year),
             selectInput(ns("tenure"), "Select Tenure:", 
                         choices = c("All", "Homeowner", "Renter"),
                         selected = "All"),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

householdSizeServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Process household size data
    processed_data <- reactive({
      process_household_size_data(all_data$hh_size)
    })
    
    # Render plots
    output$state_plot <- renderPlot({
      plot_household_size(processed_data(), "state", 
                          start_year = input$start_year, 
                          end_year = input$end_year,
                          selected_tenure = input$tenure)
    })
    
    output$cbsa_plot <- renderPlot({
      plot_household_size(processed_data(), "cbsa", input$cbsa, 
                          input$start_year, input$end_year, input$tenure)
    })
    
    output$locality_plot <- renderPlot({
      plot_household_size(processed_data(), "locality", input$locality, 
                          input$start_year, input$end_year, input$tenure)
    })
  })
}

# Module for Average Household Size
avgHouseholdSizeUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("tenure"), "Select Tenure:", 
                         choices = c("All", "Homeowner", "Renter"),
                         selected = "All"),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = unique(data$avg_size$name[data$avg_size$geography == "cbsa"]), 
                           selected = unique(data$avg_size$name[data$avg_size$geography == "cbsa"])[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = unique(data$avg_size$name[data$avg_size$geography == "locality"]), 
                           selected = unique(data$avg_size$name[data$avg_size$geography == "locality"])[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

avgHouseholdSizeServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Process average household size data
    processed_data <- reactive({
      process_avg_household_size_data(all_data$avg_size)
    })
    
    # Render plots
    output$state_plot <- renderPlot({
      plot_avg_household_size(processed_data(), "state", selected_tenure = input$tenure)
    })
    
    output$cbsa_plot <- renderPlot({
      plot_avg_household_size(processed_data(), "cbsa", input$cbsa, input$tenure)
    })
    
    output$locality_plot <- renderPlot({
      plot_avg_household_size(processed_data(), "locality", input$locality, input$tenure)
    })
  })
}

# NEW MODULE: Income Distribution
incomeDistributionUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$inc_dist$year), decreasing = TRUE),
                         selected = max(data$inc_dist$year)),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

incomeDistributionServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Process income distribution data
    processed_data <- reactive({
      process_income_distribution_data(all_data$inc_dist)
    })
    
    # Render plots
    output$state_plot <- renderPlot({
      plot_income_distribution(processed_data(), "state", selected_year = input$year)
    })
    
    output$cbsa_plot <- renderPlot({
      plot_income_distribution(processed_data(), "cbsa", input$cbsa, input$year)
    })
    
    output$locality_plot <- renderPlot({
      plot_income_distribution(processed_data(), "locality", input$locality, input$year)
    })
  })
}

# NEW MODULE: Median Income
medianIncomeUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$state_minc$year), decreasing = TRUE),
                         selected = max(data$state_minc$year)),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

medianIncomeServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Render median income plots
    output$state_plot <- renderPlot({
      plot_data <- all_data$state_minc %>% 
        filter(year == input$year)
      
      ggplot(plot_data, aes(x = tenure, y = estimate, fill = tenure)) +
        geom_col() +


geom_col() +
  geom_text(aes(label = scales::dollar(estimate)),
            vjust = -0.5, size = 4) +
  scale_fill_hfv() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Median Household Income by Tenure in Virginia",
       x = "", y = "Median Income") +
  theme_hfv() +
  theme(plot.title = element_markdown())
})

output$cbsa_plot <- renderPlot({
  plot_data <- all_data$cbsa_minc %>% 
    filter(year == input$year, cbsa_title == input$cbsa)
  
  ggplot(plot_data, aes(x = tenure, y = estimate, fill = tenure)) +
    geom_col() +
    geom_text(aes(label = scales::dollar(estimate)),
              vjust = -0.5, size = 4) +
    scale_fill_hfv() +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(title = paste("Median Household Income by Tenure in", input$cbsa),
         x = "", y = "Median Income") +
    theme_hfv() +
    theme(plot.title = element_markdown())
})

output$locality_plot <- renderPlot({
  plot_data <- all_data$local_minc %>% 
    filter(year == input$year, name_long == input$locality)
  
  ggplot(plot_data, aes(x = tenure, y = estimate, fill = tenure)) +
    geom_col() +
    geom_text(aes(label = scales::dollar(estimate)),
              vjust = -0.5, size = 4) +
    scale_fill_hfv() +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(title = paste("Median Household Income by Tenure in", input$locality),
         x = "", y = "Median Income") +
    theme_hfv() +
    theme(plot.title = element_markdown())
})
})
}

# NEW MODULE: Income by Age
incomeByAgeUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$state_inc_age$year), decreasing = TRUE),
                         selected = max(data$state_inc_age$year)),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

incomeByAgeServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Render income by age plots
    output$state_plot <- renderPlot({
      plot_data <- all_data$state_inc_age %>% 
        filter(year == input$year)
      
      ggplot(plot_data, aes(x = age, y = estimate, fill = age)) +
        geom_col() +
        geom_text(aes(label = scales::dollar(estimate)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(title = "Median Household Income by Age in Virginia",
             x = "", y = "Median Income") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$cbsa_plot <- renderPlot({
      plot_data <- all_data$cbsa_inc_age %>% 
        filter(year == input$year, cbsa_title == input$cbsa)
      
      ggplot(plot_data, aes(x = age, y = estimate, fill = age)) +
        geom_col() +
        geom_text(aes(label = scales::dollar(estimate)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(title = paste("Median Household Income by Age in", input$cbsa),
             x = "", y = "Median Income") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$locality_plot <- renderPlot({
      plot_data <- all_data$local_inc_age %>% 
        filter(year == input$year, name == input$locality)
      
      ggplot(plot_data, aes(x = age, y = estimate, fill = age)) +
        geom_col() +
        geom_text(aes(label = scales::dollar(estimate)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(title = paste("Median Household Income by Age in", input$locality),
             x = "", y = "Median Income") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}

# NEW MODULE: Income by Race
incomeByRaceUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$state_rinc$year), decreasing = TRUE),
                         selected = max(data$state_rinc$year)),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

incomeByRaceServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Render income by race plots
    output$state_plot <- renderPlot({
      plot_data <- all_data$state_rinc %>% 
        filter(year == input$year)
      
      ggplot(plot_data, aes(x = reorder(race, -estimate), y = estimate, fill = race)) +
        geom_col() +
        geom_text(aes(label = scales::dollar(estimate)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(title = "Median Household Income by Race in Virginia",
             x = "", y = "Median Income") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$cbsa_plot <- renderPlot({
      plot_data <- all_data$cbsa_rinc %>% 
        filter(year == input$year, cbsa_title == input$cbsa)
      
      ggplot(plot_data, aes(x = reorder(race, -estimate), y = estimate, fill = race)) +
        geom_col() +
        geom_text(aes(label = scales::dollar(estimate)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(title = paste("Median Household Income by Race in", input$cbsa),
             x = "", y = "Median Income") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$locality_plot <- renderPlot({
      plot_data <- all_data$locality_rinc %>% 
        filter(year == input$year, locality == input$locality)
      
      ggplot(plot_data, aes(x = reorder(race, -estimate), y = estimate, fill = race)) +
        geom_col() +
        geom_text(aes(label = scales::dollar(estimate)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(title = paste("Median Household Income by Race in", input$locality),
             x = "", y = "Median Income") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}

# NEW MODULE: Poverty by Race
povertyByRaceUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$poverty_race$year), decreasing = TRUE),
                         selected = max(data$poverty_race$year)),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

povertyByRaceServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Render poverty by race plots
    output$state_plot <- renderPlot({
      plot_data <- all_data$poverty_race %>% 
        filter(year == input$year, geography == "state")
      
      ggplot(plot_data, aes(x = reorder(race, -poverty_rate), y = poverty_rate, fill = race)) +
        geom_col() +
        geom_text(aes(label = scales::percent(poverty_rate, accuracy = 0.1)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(title = "Poverty Rate by Race in Virginia",
             x = "", y = "Poverty Rate") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$cbsa_plot <- renderPlot({
      plot_data <- all_data$poverty_race %>% 
        filter(year == input$year, geography == "cbsa", name == input$cbsa)
      
      ggplot(plot_data, aes(x = reorder(race, -poverty_rate), y = poverty_rate, fill = race)) +
        geom_col() +
        geom_text(aes(label = scales::percent(poverty_rate, accuracy = 0.1)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(title = paste("Poverty Rate by Race in", input$cbsa),
             x = "", y = "Poverty Rate") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$locality_plot <- renderPlot({
      plot_data <- all_data$poverty_race %>% 
        filter(year == input$year, geography == "locality", name == input$locality)
      
      ggplot(plot_data, aes(x = reorder(race, -poverty_rate), y = poverty_rate, fill = race)) +
        geom_col() +
        geom_text(aes(label = scales::percent(poverty_rate, accuracy = 0.1)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(title = paste("Poverty Rate by Race in", input$locality),
             x = "", y = "Poverty Rate") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}

# NEW MODULE: Poverty by Age
povertyByAgeUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$poverty_age$year), decreasing = TRUE),
                         selected = max(data$poverty_age$year)),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

povertyByAgeServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Render poverty by age plots
    output$state_plot <- renderPlot({
      plot_data <- all_data$poverty_age %>% 
        filter(year == input$year, geography == "state")
      
      ggplot(plot_data, aes(x = age, y = poverty_rate, fill = age)) +
        geom_col() +
        geom_text(aes(label = scales::percent(poverty_rate, accuracy = 0.1)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(title = "Poverty Rate by Age in Virginia",
             x = "", y = "Poverty Rate") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$cbsa_plot <- renderPlot({
      plot_data <- all_data$poverty_age %>% 
        filter(year == input$year, geography == "cbsa", name == input$cbsa)
      
      ggplot(plot_data, aes(x = age, y = poverty_rate, fill = age)) +
        geom_col() +
        geom_text(aes(label = scales::percent(poverty_rate, accuracy = 0.1)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(title = paste("Poverty Rate by Age in", input$cbsa),
             x = "", y = "Poverty Rate") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$locality_plot <- renderPlot({
      plot_data <- all_data$poverty_age %>% 
        filter(year == input$year, geography == "locality", name == input$locality)
      
      ggplot(plot_data, aes(x = age, y = poverty_rate, fill = age)) +
        geom_col() +
        geom_text(aes(label = scales::percent(poverty_rate, accuracy = 0.1)),
                  vjust = -0.5, size = 4) +
        scale_fill_hfv() +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(title = paste("Poverty Rate by Age in", input$locality),
             x = "", y = "Poverty Rate") +
        theme_hfv() +
        theme(plot.title = element_markdown(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}

# NEW MODULE: Housing Type
housingTypeUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$b25032$year), decreasing = TRUE),
                         selected = max(data$b25032$year)),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

housingTypeServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Process housing type data
    processed_data <- reactive({
      process_housing_type_data(all_data$b25032)
    })
    
    # Render plots
    output$state_plot <- renderPlot({
      plot_housing_type(processed_data(), "state", selected_year = input$year)
    })
    
    output$cbsa_plot <- renderPlot({
      plot_housing_type(processed_data(), "cbsa", input$cbsa, input$year)
    })
    
    output$locality_plot <- renderPlot({
      plot_housing_type(processed_data(), "locality", input$locality, input$year)
    })
  })
}

# NEW MODULE: Housing by Year Built
housingByYearUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$b25127$year), decreasing = TRUE),
                         selected = max(data$b25127$year)),
             selectInput(ns("tenure"), "Select Tenure:", 
                         choices = c("All", "Owner", "Renter"),
                         selected = "All"),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

housingByYearServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Process housing by year built data
    processed_data <- reactive({
      process_housing_by_year_built(all_data$b25127)
    })
    
    # Render plots
    output$state_plot <- renderPlot({
      plot_housing_by_year_built(processed_data(), "state", selected_year = input$year, selected_tenure = input$tenure)
    })
    
    output$cbsa_plot <- renderPlot({
      plot_housing_by_year_built(processed_data(), "cbsa", input$cbsa, input$year, input$tenure)
    })
    
    output$locality_plot <- renderPlot({
      plot_housing_by_year_built(processed_data(), "locality", input$locality, input$year, input$tenure)
    })
  })
}

# NEW MODULE: Bedrooms by Tenure
bedroomsUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$b25042$year), decreasing = TRUE),
                         selected = max(data$b25042$year)),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

bedroomsServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Process bedrooms by tenure data
    processed_data <- reactive({
      process_bedrooms_by_tenure(all_data$b25042)
    })
    
    # Render plots
    output$state_plot <- renderPlot({
      plot_bedrooms_by_tenure(processed_data(), "state", selected_year = input$year)
    })
    
    output$cbsa_plot <- renderPlot({
      plot_bedrooms_by_tenure(processed_data(), "cbsa", input$cbsa, input$year)
    })
    
    output$locality_plot <- renderPlot({
      plot_bedrooms_by_tenure(processed_data(), "locality", input$locality, input$year)
    })
  })
}

# NEW MODULE: Overcrowding
overcrowdingUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("year"), "Select Year:", 
                         choices = sort(unique(data$b25014$year), decreasing = TRUE),
                         selected = max(data$b25014$year)),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'CBSA'", ns("tabset")),
               selectInput(ns("cbsa"), "Select CBSA:", 
                           choices = data$cbsa_list, 
                           selected = data$cbsa_list[1])
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Locality'", ns("tabset")),
               selectInput(ns("locality"), "Select Locality:", 
                           choices = data$locality_list, 
                           selected = data$locality_list[1])
             )
      ),
      column(9,
             tabsetPanel(id = ns("tabset"),
                         tabPanel("State", value = "State", plotOutput(ns("state_plot"), height = "600px")),
                         tabPanel("CBSA", value = "CBSA", plotOutput(ns("cbsa_plot"), height = "600px")),
                         tabPanel("Locality", value = "Locality", plotOutput(ns("locality_plot"), height = "600px"))
             )
      )
    )
  )
}

overcrowdingServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Process overcrowding data
    processed_data <- reactive({
      process_overcrowding_data(all_data$b25014)
    })
    
    # Render plots
    output$state_plot <- renderPlot({
      plot_overcrowding(processed_data(), "state", selected_year = input$year)
    })
    
    output$cbsa_plot <- renderPlot({
      plot_overcrowding(processed_data(), "cbsa", input$cbsa, input$year)
    })
    
    output$locality_plot <- renderPlot({
      plot_overcrowding(processed_data(), "locality", input$locality, input$year)
    })
  })
}

# NEW MODULE: Homeownership Rate
homeownershipUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             plotOutput(ns("plot"), height = "600px")
      )
    )
  )
}

homeownershipServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Render homeownership rate plot
    output$plot <- renderPlot({
      ggplot(all_data$b25003_state, aes(x = year, y = ho_rate)) +
        geom_line(color = "#011E41", linewidth = 1.5) +
        geom_point(color = "#011E41", size = 3) +
        scale_y_continuous(labels = scales::percent_format(),
                           limits = c(min(all_data$b25003_state$ho_rate) * 0.95, 
                                      max(all_data$b25003_state$ho_rate) * 1.05)) +
        labs(title = "Virginia Homeownership Rate",
             x = "Year", 
             y = "Homeownership Rate") +
        theme_hfv() +
        theme(plot.title = element_markdown())
    })
  })
}

# ---- Main Shiny App ----

ui <- fluidPage(
  titlePanel("Housing Data Visualizations"),
  
  navlistPanel(
    widths = c(2, 10),
    
    "Housing Demographics",
    tabPanel("Household Type", value = "household_type", uiOutput("household_type_ui")),
    tabPanel("Living Arrangements", value = "living_arrangements", uiOutput("living_arrangements_ui")),
    tabPanel("Household Size", value = "household_size", uiOutput("household_size_ui")),
    tabPanel("Average Household Size", value = "avg_household_size", uiOutput("avg_household_size_ui")),
    
    "Income & Poverty",
    tabPanel("Median Household Income", value = "median_income", uiOutput("median_income_ui")),
    tabPanel("Income Distribution", value = "income_distribution", uiOutput("income_distribution_ui")),
    tabPanel("Income by Age", value = "income_by_age", uiOutput("income_by_age_ui")),
    tabPanel("Income by Race", value = "income_by_race", uiOutput("income_by_race_ui")),
    tabPanel("Poverty by Race", value = "poverty_by_race", uiOutput("poverty_by_race_ui")),
    tabPanel("Poverty by Age", value = "poverty_by_age", uiOutput("poverty_by_age_ui")),
    
    "Housing Characteristics",
    tabPanel("Housing Type", value = "housing_type", uiOutput("housing_type_ui")),
    tabPanel("Housing by Year Built", value = "housing_by_year", uiOutput("housing_by_year_ui")),
    tabPanel("Bedrooms by Tenure", value = "bedrooms", uiOutput("bedrooms_ui")),
    tabPanel("Overcrowding", value = "overcrowding", uiOutput("overcrowding_ui")),
    tabPanel("Homeownership Rate", value = "homeownership", uiOutput("homeownership_ui")),
    
    id = "navlist"
  )
)

server <- function(input, output, session) {
  # Load all datasets
  all_data <- reactiveVal()
  
  # Load data at startup (could be slow, consider loading on demand)
  observe({
    all_data(load_all_datasets())
  }, priority = 1000)
  
  # Render UIs for each module based on selected tab
  output$household_type_ui <- renderUI({
    req(all_data())
    householdTypeUI("household_type", all_data())
  })
  
  output$living_arrangements_ui <- renderUI({
    req(all_data())
    livingArrangementsUI("living_arrangements", all_data())
  })
  
  output$household_size_ui <- renderUI({
    req(all_data())
    householdSizeUI("household_size", all_data())
  })
  
  output$avg_household_size_ui <- renderUI({
    req(all_data())
    avgHouseholdSizeUI("avg_household_size", all_data())
  })
  
  # Add UI renders for new modules
  output$median_income_ui <- renderUI({
    req(all_data())
    medianIncomeUI("median_income", all_data())
  })
  
  output$income_distribution_ui <- renderUI({
    req(all_data())
    incomeDistributionUI("income_distribution", all_data())
  })
  
  output$income_by_age_ui <- renderUI({
    req(all_data())
    incomeByAgeUI("income_by_age", all_data())
  })
  
  output$income_by_race_ui <- renderUI({
    req(all_data())
    incomeByRaceUI("income_by_race", all_data())
  })
  
  output$poverty_by_race_ui <- renderUI({
    req(all_data())
    povertyByRaceUI("poverty_by_race", all_data())
  })
  
  output$poverty_by_age_ui <- renderUI({
    req(all_data())
    povertyByAgeUI("poverty_by_age", all_data())
  })
  
  output$housing_type_ui <- renderUI({
    req(all_data())
    housingTypeUI("housing_type", all_data())
  })
  
  output$housing_by_year_ui <- renderUI({
    req(all_data())
    housingByYearUI("housing_by_year", all_data())
  })
  
  output$bedrooms_ui <- renderUI({
    req(all_data())
    bedroomsUI("bedrooms", all_data())
  })
  
  output$overcrowding_ui <- renderUI({
    req(all_data())
    overcrowdingUI("overcrowding", all_data())
  })
  
  output$homeownership_ui <- renderUI({
    req(all_data())
    homeownershipUI("homeownership", all_data())
  })
  
  # Initialize server logic for each module
  observe({
    req(all_data())
    householdTypeServer("household_type", all_data())
    livingArrangementsServer("living_arrangements", all_data())
    householdSizeServer("household_size", all_data())
    avgHouseholdSizeServer("avg_household_size", all_data())
    
    # Add server initializations for new modules
    medianIncomeServer("median_income", all_data())
    incomeDistributionServer("income_distribution", all_data())
    incomeByAgeServer("income_by_age", all_data())
    incomeByRaceServer("income_by_race", all_data()) 
    povertyByRaceServer("poverty_by_race", all_data())
    povertyByAgeServer("poverty_by_age", all_data())
    housingTypeServer("housing_type", all_data())
    housingByYearServer("housing_by_year", all_data())
    bedroomsServer("bedrooms", all_data())
    overcrowdingServer("overcrowding", all_data())
    homeownershipServer("homeownership", all_data())
  })
}

# Run the application
shinyApp(ui = ui, server = server)