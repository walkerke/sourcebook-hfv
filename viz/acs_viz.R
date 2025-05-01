# Develop basic viz structure for all Sourcebook visualizations that
# utilize ACS data. Use this script to design and finalize visualizations 
# that will become Shiny apps.

library(tidyverse)
library(ggtext)
library(hdatools)
library(ggtext)
library(ggiraph)


## ---- Household Type ----
# The first visualization is based on Table B11021 from the ACS 5-year estimates. 
# The visualization provides the distribution of household types for a certain 
# geography in a given year.


# Pull latest data in.

hh_type <- read_rds("data/hh_type.rds")

# Create a list of all unique CBSAs and localities in Virginia based on the data.
cbsa_list <- sort(unique(hh_type$cbsa_title))
locality_list <- sort(unique(hh_type$name_long))

# Calculate percentage based on year and geographic level, and then
# aggregate data for each geographic level. 

locality_hh <- hh_type |> 
  group_by(year, name_long) |> 
  mutate(percent = estimate/sum(estimate))

cbsa_hh <- hh_type |> 
  group_by(year, cbsa_title, type, subtype) |> 
  summarise(estimate = sum(estimate)) |> 
  group_by(year, cbsa_title) |> 
  mutate(percent = estimate/sum(estimate))

# Aggregate data to the state-level for faster processing.
state_hh <- hh_type |> 
  group_by(year, type, subtype) |> 
  summarise(estimate = sum(estimate)) |> 
  group_by(year) |> 
  mutate(percent = estimate/sum(estimate))

title_text <- "<b><span style=' color:#011E41'>Householder with no partner</span></b> and 
<b><span style=' color:#40C0C0'>Married or cohabitating couple"

# Filter placeholders.

locality <- locality_hh |> 
  filter(year == 2023, 
         name_long == "Accomack County") |> 
  group_by(type) |> 
  mutate(rank_within_type = rank(percent, ties.method = "first")) |> 
  ungroup()

cbsa <- cbsa_hh |> 
  filter(year == 2023,
         cbsa_title == "Big Stone Gap, VA") |> 
  group_by(type) |> 
  mutate(rank_within_type = rank(percent, ties.method = "first")) |> 
  ungroup()

state <- state_hh |> 
  filter(year == 2023) |> 
  group_by(type) |> 
  mutate(rank_within_type = rank(percent, ties.method = "first")) |> 
  ungroup()



ggplot(locality,
       aes(x = reorder(subtype, rank_within_type),
           y = percent,
           fill = type)) + 
  geom_col() +
  # Match text color to bar fill color
  geom_text(aes(label = scales::percent(percent, accuracy = 1),
                color = type),  # Use the same grouping variable for color
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3.5) +
  # Make sure text colors match fill colors
  scale_color_hfv() +  # This should use the same palette as scale_fill_hfv()
  labs(title = title_text,
       subtitle = locality$name_long) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_hfv() +
  scale_fill_hfv() +
  # Hide the color legend since it's redundant with the fill legend
  guides(color = "none") +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.8,
      margin = margin(t = 5)
    ),
    strip.text = element_blank()
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  facet_grid(cols = vars(type), scales = "free_x", space = "free")

ggplot(cbsa,
       aes(x = reorder(subtype, rank_within_type),
           y = percent,
           fill = type)) + 
  geom_col() +
  # Match text color to bar fill color
  geom_text(aes(label = scales::percent(percent, accuracy = 1),
                color = type),  # Use the same grouping variable for color
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3.5) +
  # Make sure text colors match fill colors
  scale_color_hfv() +  # This should use the same palette as scale_fill_hfv()
  labs(title = "Household Composition by Type") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_hfv() +
  scale_fill_hfv() +
  # Hide the color legend since it's redundant with the fill legend
  guides(color = "none") +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.8,
      margin = margin(t = 5)
    )
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  facet_grid(cols = vars(type), scales = "free_x", space = "free")


ggplot(state,
       aes(x = reorder(subtype, rank_within_type),
           y = percent,
           fill = type)) + 
  geom_col() +
  # Match text color to bar fill color
  geom_text(aes(label = scales::percent(percent, accuracy = 1),
                color = type),  # Use the same grouping variable for color
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3.5) +
  # Make sure text colors match fill colors
  scale_color_hfv() +  # This should use the same palette as scale_fill_hfv()
  labs(title = "Household Composition by Type") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_hfv() +
  scale_fill_hfv() +
  # Hide the color legend since it's redundant with the fill legend
  guides(color = "none") +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.8,
      margin = margin(t = 5)
    )
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  facet_grid(cols = vars(type), scales = "free_x", space = "free")


## ---- Living Arrangements of Adults -----
# The following visualization is based on Table B09021 from the ACS 5-year estimates. 
# The visualization provides the distribution of living arrangements among adults 18 years
# and older. The data is meant to be filterable by age group and year for all visualizations.
# But the data can also be filtered by geography for CBSA and locality.

lvng_arr <- read_rds("data/lvng_arr.rds")

# Create a list of all unique CBSAs and localities in Virginia based on the data.
cbsa_list <- sort(unique(lvng_arr$cbsa_title))
locality_list <- sort(unique(lvng_arr$name_long))

# Aggregate data and calculate percentage.

locality_la <- lvng_arr |> 
  group_by(year, name_long) |> 
  mutate(percent = estimate/sum(estimate))

cbsa_la <- lvng_arr |> 
  group_by(year, cbsa_title, age, type) |> 
  summarise(estimate = sum(estimate)) |> 
  group_by(year, cbsa_title) |> 
  mutate(percent = estimate/sum(estimate))

state_la <- lvng_arr |> 
  group_by(year, age, type) |> 
  summarise(estimate = sum(estimate)) |> 
  group_by(year) |> 
  mutate(percent = estimate/sum(estimate))

title_text <- "<b><span style=' color:#011E41'>Living arrangement of adults</span></b>"

# Filter placeholders for shiny app.

locality <- locality_la |> 
  filter(year == 2023, 
         age == "All ages",
         name_long == "Accomack County") |> 
  ungroup()

cbsa <- cbsa_la |> 
  filter(year == 2023,
         age == "All ages",
         cbsa_title == "Big Stone Gap, VA") |> 
  ungroup()

state <- state_la |> 
  filter(year == 2023,
         age == "All ages") |> 
  ungroup()



ggplot(locality,
       aes(x = reorder(type, percent),
           y = percent,
           fill = type)) + 
  geom_col() +
  # Match text color to bar fill color
  geom_text(aes(label = scales::percent(percent, accuracy = 1),
                color = type),  # Use the same grouping variable for color
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3.5) +
  # Make sure text colors match fill colors
  scale_color_hfv() +  # This should use the same palette as scale_fill_hfv()
  labs(title = title_text,
       subtitle = locality$name_long) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_hfv() +
  scale_fill_hfv() +
  # Hide the color legend since it's redundant with the fill legend
  guides(color = "none") +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.8,
      margin = margin(t = 5)
    ),
    strip.text = element_blank()
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 

ggplot(cbsa,
       aes(x = reorder(type, percent),
           y = percent,
           fill = type)) + 
  geom_col() +
  # Match text color to bar fill color
  geom_text(aes(label = scales::percent(percent, accuracy = 1),
                color = type),  # Use the same grouping variable for color
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3.5) +
  # Make sure text colors match fill colors
  scale_color_hfv() +  # This should use the same palette as scale_fill_hfv()
  labs(title = title_text,
       subtitle = locality$name_long) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_hfv() +
  scale_fill_hfv() +
  # Hide the color legend since it's redundant with the fill legend
  guides(color = "none") +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.8,
      margin = margin(t = 5)
    )
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 


ggplot(state,
       aes(x = reorder(type, percent),
           y = percent,
           fill = type)) + 
  geom_col() +
  # Match text color to bar fill color
  geom_text(aes(label = scales::percent(percent, accuracy = 1),
                color = type),  # Use the same grouping variable for color
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3.5) +
  # Make sure text colors match fill colors
  scale_color_hfv() +  # This should use the same palette as scale_fill_hfv()
  labs(title = title_text,
       subtitle = locality$name_long) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_hfv() +
  scale_fill_hfv() +
  # Hide the color legend since it's redundant with the fill legend
  guides(color = "none") +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.8,
      margin = margin(t = 5)
    )
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 

# Create a shiny app based on the visualizations above. Each visualization will be a 
# different tab: Statewide, CBSA, and Locality. And then there should be a filter for 
# year and age group on each visualization and filter for CBSA on CBSA visualization and 
# locality filter for locality visualization. Can you help me?

## ---- Households by size -----
# The following visualization is based on Table B09021 from the ACS 5-year estimates. 
# The visualization provides the distribution of living arrangements among adults 18 years
# and older. The data is meant to be filterable by age group and year for all visualizations.
# But the data can also be filtered by geography for CBSA and locality.

hh_size <- read_rds("data/hh_size.rds") |> 
  mutate(tenure = case_when(
    tenure == "Owner" ~ "Homeowner",
    TRUE ~ tenure
  ))

# Create a list of all unique CBSAs and localities in Virginia based on the data.
cbsa_list <- sort(unique(hh_size$cbsa_title))
locality_list <- sort(unique(hh_size$name_long))

# Aggregate data and calculate percentage.

locality_size <- hh_size |> 
  pivot_wider(
    id_cols = c(year, name_long, hhsize),
    names_from = tenure,
    values_from = estimate
  ) |> 
  mutate(All = Renter + Homeowner) |> 
  pivot_longer(
    cols = c(Renter, Homeowner, All),
    names_to = "tenure",
    values_to = "estimate"
  ) |>
  arrange(name_long, tenure, year) |> 
  group_by(name_long, tenure, hhsize) |> 
  mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) |> 
  group_by(year, name_long, tenure) |> 
  mutate(percent = estimate/sum(estimate)) |> 
  ungroup()
  

cbsa_size <- hh_size |> 
  group_by(year, cbsa_title, tenure, hhsize) |> 
  summarise(estimate = sum(estimate)) |> 
  pivot_wider(
    id_cols = c(year, cbsa_title, hhsize),
    names_from = tenure,
    values_from = estimate
  ) |> 
  mutate(All = Renter + Homeowner) |> 
  pivot_longer(
    cols = c(Renter, Homeowner, All),
    names_to = "tenure",
    values_to = "estimate"
  ) |> 
  arrange(cbsa_title, tenure, year) |> 
  group_by(cbsa_title, tenure, hhsize) |> 
  mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) |> 
  group_by(year,cbsa_title, tenure) |> 
  mutate(percent = estimate/sum(estimate)) |>
  ungroup()

state_size <- hh_size|>
  group_by(year, tenure, hhsize) |> 
  summarise(estimate = sum(estimate)) |>  
  pivot_wider(
    id_cols = c(year, hhsize),
    names_from = tenure,
    values_from = estimate
  ) |> 
  mutate(All = Renter + Homeowner) |> 
  pivot_longer(
    cols = c(Renter, Homeowner, All),
    names_to = "tenure",
    values_to = "estimate"
  ) |>
  group_by(year, tenure, hhsize) |> 
  summarise(estimate = sum(estimate)) |> 
  arrange(tenure, year) |> 
  group_by(tenure, hhsize) |> 
  mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) |> 
  group_by(year, tenure) |> 
  mutate(percent = estimate/sum(estimate)) |> 
  ungroup()

title_text <- "<span style=' color:#011E41'>household distribution by size</span>"

# Filter placeholders.

locality <- locality_size |> 
  filter(name_long == "Accomack County",
         year == 2010 | year == 2023,
         tenure == "All") |> 
  mutate(year = as.character(year)) |> 
  arrange(year, hhsize) |> 
  group_by(hhsize) |>
  mutate(pct_change = case_when(
    year == "2023" ~ ((estimate - first(estimate)) / first(estimate)),
    TRUE ~ NA_real_  # This assigns NA to all other years (2010)
  )) |> 
  ungroup()


cbsa <- cbsa_size |> 
  filter(tenure == "Homeowner",
         cbsa_title == "Big Stone Gap, VA",
         year == 2010 | year == 2023) |> 
  mutate(year = as.character(year)) |> 
  arrange(year, hhsize) |> 
  group_by(hhsize) |>
  mutate(pct_change = case_when(
    year == "2023" ~ ((estimate - first(estimate)) / first(estimate)),
    TRUE ~ NA_real_  # This assigns NA to all other years (2010)
  )) |> 
  ungroup() 


state <- state_size |> 
  filter(tenure == "Homeowner",
         year == 2010 | year == 2023) |> 
  mutate(year = as.character(year)) |> 
  arrange(year, hhsize) |> 
  group_by(hhsize) |>
  mutate(pct_change = case_when(
    year == "2023" ~ ((estimate - first(estimate)) / first(estimate)),
    TRUE ~ NA_real_  # This assigns NA to all other years (2010)
  )) |> 
  ungroup() 


# Model visualization for localities.
ggplot(locality,
       aes(x = year,
           y = estimate,
           fill = year)) + 
  geom_col() +
  facet_wrap(~hhsize, nrow = 1) +
  # Add percent change labels only for 2023 data
  geom_text(
    data = filter(locality, year == "2023"),
    aes(label = scales::percent(pct_change, accuracy = 0.1)),
    position = position_stack(),
    vjust = -0.5,
    size = 3.5
  ) +
  theme_hfv() +
  scale_fill_hfv() +
  # Remove existing vertical grid lines within panels
  theme(
    panel.grid.major.x = element_blank(),   # Remove vertical grid lines inside panels
    panel.spacing = unit(1, "lines"),       # Increase space between facets
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5), # Add border around panels
    strip.background = element_blank()      # Remove facet label background
  ) +
  scale_y_continuous(labels = number_format(big.mark = ",")) +
  labs(title = paste(locality$tenure, "households by size"))


# Model visualization for CBSA.
ggplot(cbsa,
       aes(x = year,
           y = estimate,
           fill = year)) + 
  geom_col() +
  facet_wrap(~hhsize, nrow = 1) +
  # Add percent change labels only for 2023 data
  geom_text(
    data = filter(cbsa, year == "2023"),
    aes(label = scales::percent(pct_change, accuracy = 0.1)),
    position = position_stack(),
    vjust = -0.5,
    size = 3.5
  ) +
  theme_hfv() +
  scale_fill_hfv() +
  # Remove existing vertical grid lines within panels
  theme(
    panel.grid.major.x = element_blank(),   # Remove vertical grid lines inside panels
    panel.spacing = unit(1, "lines"),       # Increase space between facets
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5), # Add border around panels
    strip.background = element_blank()      # Remove facet label background
  ) +
  scale_y_continuous(labels = number_format(big.mark = ",")) +
  labs(title = paste(cbsa$tenure, "households by size"))

# Model visualization for State.
ggplot(state,
       aes(x = year,
           y = estimate,
           fill = year)) + 
  geom_col() +
  facet_wrap(~hhsize, nrow = 1) +
  # Add percent change labels only for 2023 data
  geom_text(
    data = filter(state, year == "2023"),
    aes(label = scales::percent(pct_change, accuracy = 0.1)),
    position = position_stack(),
    vjust = -0.5,
    size = 3.5
  ) +
  theme_hfv() +
  scale_fill_hfv() +
  # Remove existing vertical grid lines within panels
  theme(
    panel.grid.major.x = element_blank(),   # Remove vertical grid lines inside panels
    panel.spacing = unit(1, "lines"),       # Increase space between facets
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5), # Add border around panels
    strip.background = element_blank()      # Remove facet label background
  ) +
  scale_y_continuous(labels = number_format(big.mark = ",")) +
  labs(title = paste(state$tenure, "households by size"))


# Create a shiny app based on the visualizations above. Each visualization will be a 
# different tab: Statewide, CBSA, and Locality. And then there should be a filter for 
# year and age group on each visualization and filter for CBSA on CBSA visualization and 
# locality filter for locality visualization. Can you help me?


## ---- Average Household Size -----

avg_size <- read_rds("data/avg_hh_size.rds") %>% 
  mutate(tenure = case_when(
    tenure == "Owner" ~ "Homeowner",
    TRUE ~ tenure
  ))

locality <- avg_size %>% 
  filter(geography == "locality",
         tenure == "All",
         name == "Accomack County")

cbsa <- avg_size %>% 
  filter(geography == "cbsa", 
         tenure == "Homeowner", 
         name == "Blacksburg-Christiansburg, VA")

state <- avg_size %>% 
  filter(geography == "state",
         tenure == "Homeowner")


ggplot(locality,
       aes(x = year,
           y = estimate))  +
  geom_line(linewidth = 1, color = "#011E41") +
  geom_point(size = 3, color = "#011E41") +
  geom_smooth(method = "loess", se = TRUE, color = "#40C0C0", fill = "#40C0C0", alpha = 0.2) +
  labs(title = "Average Household Size Over Time",
       subtitle = locality$name[1],
       x = "Year",
       y = "Average Household Size") +
  scale_y_continuous(limits = c(min(locality$estimate) * 0.95, max(locality$estimate) * 1.05),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme_hfv()


ggplot(cbsa,
       aes(x = year,
           y = estimate)) +
  geom_line(linewidth = 1, color = "#011E41") +
  geom_point(size = 3, color = "#011E41") +
  geom_smooth(method = "loess", se = TRUE, color = "#40C0C0", fill = "#40C0C0", alpha = 0.2) +
  labs(title = paste(cbsa$tenure[1], "Average Household Size Over Time"),
       subtitle = cbsa$name[1],
       x = "Year",
       y = "Average Household Size") +
  scale_y_continuous(limits = c(min(cbsa$estimate) * 0.95, max(cbsa$estimate) * 1.05),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme_hfv()


# Calculate state-wide average and add annotations for min and max points
state_plot <- state %>%
  mutate(label_point = year == min(year) | year == max(year) | estimate == max(estimate) | estimate == min(estimate))

ggplot(state_plot,
       aes(x = year,
           y = estimate)) +
  geom_line(linewidth = 1, color = "#011E41") +
  geom_point(size = 3, color = "#011E41") +
  geom_smooth(method = "loess", se = TRUE, color = "#40C0C0", fill = "#40C0C0", alpha = 0.2) +
  geom_text(data = filter(state_plot, label_point),
            aes(label = scales::number(estimate, accuracy = 0.01)),
            vjust = -0.8, hjust = 0.5, size = 3.5) +
  labs(title = paste(state$tenure[1], "Average Household Size in Virginia"),
       subtitle = "2010-2023",
       x = "Year",
       y = "Average Household Size") +
  scale_y_continuous(limits = c(min(state$estimate) * 0.93, max(state$estimate) * 1.07),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme_hfv()

# Create shiny apps for the above where each plot is its own tab and allow there to be filters 
# that adjust the geography and tenure. The filter for tenure should allow you to select more than 
# one tenure option at a time. 




## ---- Median Household Income ---- ##

state_minc <- read_rds("data/b25119_state.rds")

cbsa_minc <- read_rds("data/b25119_cbsa.rds")

local_minc <- read_rds("data/b25119_local.rds")

state <- state_minc %>% 
  filter(state == "Virginia",
         tenure == "All households")

cbsa <- cbsa_minc %>% 
  filter(cbsa == "Blacksburg-Christiansburg-Radford, VA Metro Area",
         tenure == "All households")

local <- local_minc %>% 
  filter(locality == "Accomack County",
         tenure == "All households")

# Shiny app should be filterable by state and you should be able to set y as estimate or adjusted, 
# depending on whether you want to see real (adjusted) or nominal (estimate) values

ggplot(state,
       aes(x = year, 
           y = estimate)) +
  geom_line() +
  geom_point() +
  theme_hfv() +
  scale_y_continuous(labels = scales::dollar_format())


ggplot(cbsa,
       aes(x = year, 
           y = estimate)) +
  geom_line() +
  geom_point() +
  theme_hfv() +
  scale_y_continuous(labels = scales::dollar_format())

ggplot(local,
       aes(x = year, 
           y = estimate)) +
  geom_line() +
  geom_point() +
  theme_hfv() +
  scale_y_continuous(labels = scales::dollar_format())

# Create shiny apps for the above where each plot is its own tab and allow there to be filters 
# that adjust the geography and tenure. The filter for tenure should allow you to select more than 
# one tenure option at a time. There should also be a filter that allows you to switch between 
# estimate and adjusted as the y value in the plot.

## ---- Income Distribution by Tenure ---- 

# The below provides data from Table B25118 of the ACS. It shows the distribution 
# of households based on income and tenure. 

# Read in the latest data. Use the here package in the Shiny app.

inc_dist <- read_rds("data/b25118_data.rds")


# Aggregate data based on different geographic levels.

income_order <- c("Less than $15,000", "$15,000 to $24,999", "$25,000 to $49,999",
                  "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $149,999",
                  "$150,000 or more")

state <- inc_dist %>% 
  group_by(year, tenure, income) %>% 
  summarise(estimate = sum(estimate)) %>% 
  mutate(income = factor(income, levels = income_order))

cbsa <- inc_dist %>% 
  group_by(year, cbsa_title, tenure, income) %>% 
  summarise(estimate = sum(estimate)) %>% 
  mutate(income = factor(income, levels = income_order))

local <- inc_dist %>% 
  mutate(income = factor(income, levels = income_order))

# Set filter options below. Each filter will present itself in the Shiny app.

state <- state %>% 
  filter(year == 2023)

cbsa <- cbsa %>% 
  filter(cbsa_title == "Non-Metro",
         year == 2023)

local <- local %>% 
  filter(name_long == "Accomack County",
         year == 2023)


# Create a data visualization that shows the distribution of households based
# on household income. The graphic shows the difference between homeowners and
# renters in a given area, in a given year. Homeowner and renter households should 
# be visualized at the same time using the facet_wrap() function with one on top of 
# the other so users can easily compare the two.

ggplot(state,
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
    axis.text.x = element_text(angle = 45,  # Angle the text
                               hjust = 1,    # Horizontal justification
                               vjust = 1,    # Vertical justification
                               size = 10,     # Smaller text size
                               lineheight = 0.9)) +  # Reduced line height
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))  # Wrap text at 10 characters


ggplot(cbsa,
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
    axis.text.x = element_text(angle = 45,  # Angle the text
                               hjust = 1,    # Horizontal justification
                               vjust = 1,    # Vertical justification
                               size = 10,     # Smaller text size
                               lineheight = 0.9)) +  # Reduced line height
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))  # Wrap text at 10 characters


ggplot(local,
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
    axis.text.x = element_text(angle = 45,  # Angle the text
                               hjust = 1,    # Horizontal justification
                               vjust = 1,    # Vertical justification
                               size = 10,     # Smaller text size
                               lineheight = 0.9)) +  # Reduced line height
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))  # Wrap text at 10 characters


# Create shiny apps for the above where each plot is its own tab and allow there to be filters 
# that adjust the geography and year. 


## ---- Median Household Income by Householder Age ---- 

# The following creates visualizations based on Table B19049 from the ACS. 
# The data was pulled for different geographic levels because medians cannot
# be aggregated. The visualizations will provide a line graph showing the 
# change in median household income by different age groups from 2010 to the latest
# ACS year. It will also provide the option to see the line graph in both real and
# current dollars. 

# Read in the latest data. Use the here package in the Shiny app.

state_inc_age <- read_rds("data/b19049_state.rds")

cbsa_inc_age <- read_rds("data/b19049_cbsa.rds")

local_inc_age <- read_rds("data/b19049_locality.rds")

# Set filter options below. Each filter will present itself in the Shiny app.

state <- state_inc_age %>% 
  filter(state == "Virginia")

cbsa <- cbsa_inc_age %>% 
  filter(cbsa == "Richmond, VA Metro Area")

local <- local_inc_age %>% 
  filter(locality == "Chesterfield County") %>% 
  mutate(estimate = as.numeric(estimate),
         adjusted = as.numeric(estimate))

# Create visualizations for each geographic level to set the style and form of 
# the plot.

ggplot(state,
       aes(
         x = year,
         y = estimate,
         color = age)) +
  geom_line() + 
  geom_point() +
  theme_hfv() +
  scale_color_hfv() +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(
    breaks = unique(state$year),  # Show only the years in your dataset
    labels = unique(state$year)   # Use those same years as labels
  )


ggplot(cbsa,
       aes(
         x = year,
         y = estimate,
         color = age)) +
  geom_line() + 
  geom_point() +
  theme_hfv() +
  scale_color_hfv() +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(
    breaks = unique(cbsa$year),  # Show only the years in your dataset
    labels = unique(cbsa$year)   # Use those same years as labels
  )

ggplot(local,
       aes(
         x = year,
         y = estimate,
         color = age,
         group = age)) +
  geom_line() + 
  geom_point() +
  theme_hfv() +
  scale_color_hfv() +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_discrete(
    breaks = unique(local$year),  # Show only the years in your dataset
    labels = unique(local$year)   # Use those same years as labels
  )

  # Create shiny apps for the above where each plot is its own tab and allow there to be filters 
  # that adjust the geography. There should also be a filter that allows you to switch between 
  # 'estimate' and 'adjusted' as the y value in the plot.
  
## ---- Median Household Income by Race/Ethnicity - Table B19013B-I -----

# The following creates visualizations based on Table B19049 from the ACS. 
# The data was pulled for different geographic levels because medians cannot
# be aggregated. The visualizations will provide a line graph showing the 
# change in median household income by different age groups from 2010 to the latest
# ACS year. It will also provide the option to see the line graph in both real and
# current dollars. 


# Read in the latest data. Use the here package in the Shiny app.

  locality_rinc <- read_rds("data/b19013_locality.rds") %>% 
  cbsa_rinc <- read_rds("data/b19013_cbsa.rds")
  state_rinc <- read_rds("data/b19013_state.rds")

  # Set filter options below. Each filter will present itself in the Shiny app
  
  state <- state_rinc %>% 
    filter(year == 2023,
           state == "Virginia")
  
  cbsa <- cbsa_rinc %>% 
    filter(year == 2023,
           CBSA == "Richmond, VA Metro Area")
  
  local <- locality_rinc %>% 
    filter(year == 2023,
           locality == "Richmond city")

# Create visualizations for each geographic level to set the style and form of 
# the plot. Visualizations show a comparison between different race or ethnic groups
# and their median household incomes. There is an option to show both the 'estimate'
# and 'adjusted' values.
  
  ggplot(state,
         aes(
           x = reorder(race, estimate),
           y = estimate,
           fill = race)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = scales::dollar_format())
  
  
  # First, get the actual unique race values from your data
  race_levels <- unique(state$race)
  
  # Create color vector without names first
  color_values <- c("#E0592A", "#259591", "#011E41", "#40C0C0", 
                    "#FFC658", "#FF7276", "#8B85CA", "#B1005F")
  
  # Then create a named vector matching your actual data values
  race_colors <- setNames(color_values[1:length(race_levels)], race_levels)
  
  # Now use that in your plot with labels
  ggplot(state %>% 
           drop_na(),
         aes(
           x = reorder(race, estimate),
           y = estimate,
           fill = race)) +
    geom_col() +
    # Add the value labels at the end of each bar, matching the fill color
    geom_text(aes(label = scales::dollar(estimate), color = race),
              hjust = -0.2) +  # Position labels just outside the bars
    # Make the text colors match the fill colors
    scale_color_manual(values = race_colors) +
    # Set the fill colors
    scale_fill_manual(values = race_colors) +
    # Extend the plot area to make room for labels
    coord_flip(clip = "off") +
    # Format y-axis with dollar signs
    scale_y_continuous(labels = scales::dollar_format()) +
    # Add some spacing on the right for the labels
    theme_hfv() +
    theme(plot.margin = margin(0.5, 2, 0.5, 0.5, "cm")) +
    # Hide the color legend since it's redundant with labels
    guides(color = "none")
  
  ggplot(cbsa %>% 
           drop_na(),
         aes(
           x = reorder(race, estimate),
           y = estimate,
           fill = race)) +
    geom_col() +
    # Add the value labels at the end of each bar, matching the fill color
    geom_text(aes(label = scales::dollar(estimate), color = race),
              hjust = -0.2) +  # Position labels just outside the bars
    # Make the text colors match the fill colors
    scale_color_manual(values = race_colors) +
    # Set the fill colors
    scale_fill_manual(values = race_colors) +
    # Extend the plot area to make room for labels
    coord_flip(clip = "off") +
    # Format y-axis with dollar signs
    scale_y_continuous(labels = scales::dollar_format()) +
    # Add some spacing on the right for the labels
    theme_hfv() +
    theme(plot.margin = margin(0.5, 2, 0.5, 0.5, "cm")) +
    # Hide the color legend since it's redundant with labels
    guides(color = "none")
  
  ggplot(local %>% 
           drop_na(),
         aes(
           x = reorder(race, estimate),
           y = estimate,
           fill = race)) +
    geom_col() +
    # Add the value labels at the end of each bar, matching the fill color
    geom_text(aes(label = scales::dollar(estimate), color = race),
              hjust = -0.2) +  # Position labels just outside the bars
    # Make the text colors match the fill colors
    scale_color_manual(values = race_colors) +
    # Set the fill colors
    scale_fill_manual(values = race_colors) +
    # Extend the plot area to make room for labels
    coord_flip(clip = "off") +
    # Format y-axis with dollar signs
    scale_y_continuous(labels = scales::dollar_format()) +
    # Add some spacing on the right for the labels
    theme_hfv() +
    theme(plot.margin = margin(0.5, 2, 0.5, 0.5, "cm")) +
    # Hide the color legend since it's redundant with labels
    guides(color = "none")
  
  
  # Create shiny apps for the visualizations above. There should be tabs 
  # for each geography. Filters are available for different geographies. For 
  # example, the state visualization allows you to filter for each state. 
  # All visualizations should allow you to filter for different years as well.
  
  ## ---- Poverty Rate by Race and Ethnicity - Table B17001 ----
  
  # The following creates visualizations based on Table B17001 from the ACS, which
  # will be incorporated into a Shiny app.The visualizations will provide a line graph 
  # showing the poverty rates of different households based on race and ethnicity from 
  # 2010 to the latest year.
  
  # Read in the latest data. Use the here package in the Shiny app.
  
  poverty_race <- read_rds("data/poverty_race.rds") 
  
  # Aggregate data based on geographic levels and calculate the poverty rate.
  
  pov_race_state <- poverty_race %>% 
    group_by(year, race) %>% 
    summarise(estimate = sum(estimate),
              totalrace = sum(totalrace)) %>% 
    mutate(rate = estimate/totalrace) %>% 
    ungroup()
  
  pov_race_cbsa <- poverty_race %>% 
    group_by(year, race, cbsa_title) %>% 
    summarise(estimate = sum(estimate),
              totalrace = sum(totalrace)) %>% 
    mutate(rate = estimate/totalrace) %>% 
    ungroup()
  
  pov_race_local <- poverty_race
  
  
  # Set filter options below. Each filter will present itself in the Shiny app. 
  # State visualization does not have any filters. 
  
  state <- pov_race_state
  
  # Calculate the mean rate for each race (you can use max() or last() instead)
  state_summary <- state %>%
    group_by(race) %>%
    summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
    arrange(desc(mean_rate))  # Arrange in descending order
  
  # Calculate the mean rate for each race to help determine order of 
  # facets.   
  state <- state %>%
    mutate(race_ordered = factor(race, levels = state_summary$race))
  
  # CBSA filter will be the CBSA.
  
  cbsa <- pov_race_cbsa %>% 
    filter(cbsa_title == "Richmond, VA")
  
  # Calculate the mean rate for each race (you can use max() or last() instead)
  cbsa_summary <- cbsa %>%
    group_by(race) %>%
    summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
    arrange(desc(mean_rate))  # Arrange in descending order
  
  # Calculate the mean rate for each race to help determine order of 
  # facets.   
  cbsa <- cbsa %>%
    mutate(race_ordered = factor(race, levels = cbsa_summary$race))
  
  
  # Calculate the mean rate for each race to help determine order of 
  # facets. 
  local_summary <- local %>%
    group_by(race) %>%
    summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
    arrange(desc(mean_rate))  # Arrange in descending order
  
  # Create a new factor with levels ordered by the mean rate
  local <- local %>%
    mutate(race_ordered = factor(race, levels = local_summary$race))
  
  # Local filter will be based on the locality. 
  
  local <- pov_race_local %>% 
    filter(locality == "Richmond city") 
  
  
  # Create data visualizations that compare poverty rate by race over time. Data is
  # faceted based on race/ethnicity category. The Shiny app should have interactivity 
  # so that users can explore the data that is presented on the visualization. In addition,
  # the latest rate value should be shown on the visualization as a label. 
  
  ggplot(state,
         aes(
           x = year,
           y = rate,
           color = race_ordered,
           group = race_ordered)) +
    geom_line(linewidth = 1) +  # Make lines thicker
    geom_point(size = 2) +      # Make points larger
    facet_wrap(~race_ordered, ncol = 3) +  # Use 3 columns instead of 1 row
    scale_color_manual(values = race_colors) +
    # Better x-axis formatting - show fewer years
    scale_x_discrete(breaks = seq(min(state$year), max(state$year), by = 5)) +  
    # Format y-axis as percentage
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, NA)) +  # Start y-axis at 0
    # Improve theme elements
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),  # Larger facet titles
      legend.position = "none",  # Remove redundant legend
      panel.spacing = unit(1.5, "lines"),  # More space between facets
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      plot.title = element_text(size = 14, face = "bold"),  # Larger plot title
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      axis.title = element_blank()# More margin space
    ) 
  
  ggplot(cbsa,
         aes(
           x = year,
           y = rate,
           color = race_ordered,
           group = race_ordered)) +
    geom_line(linewidth = 1) +  # Make lines thicker
    geom_point(size = 2) +      # Make points larger
    facet_wrap(~race_ordered, ncol = 3) +  # Use 3 columns instead of 1 row
    scale_color_manual(values = race_colors) +
    # Better x-axis formatting - show fewer years
    scale_x_discrete(breaks = seq(min(cbsa$year), max(cbsa$year), by = 5)) +  
    # Format y-axis as percentage
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, NA)) +  # Start y-axis at 0
    # Improve theme elements
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),  # Larger facet titles
      legend.position = "none",  # Remove redundant legend
      panel.spacing = unit(1.5, "lines"),  # More space between facets
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      plot.title = element_text(size = 14, face = "bold"),  # Larger plot title
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      axis.title = element_blank()# More margin space
    ) 
  
  
  ggplot(local,
         aes(
           x = year,
           y = rate,
           color = race_ordered,
           group = race_ordered)) +
    geom_line(linewidth = 1) +  # Make lines thicker
    geom_point(size = 2) +      # Make points larger
    facet_wrap(~race_ordered, ncol = 3) +  # Use 3 columns instead of 1 row
    scale_color_manual(values = race_colors) +
    # Better x-axis formatting - show fewer years
    scale_x_discrete(breaks = seq(min(local$year), max(local$year), by = 5)) +  
    # Format y-axis as percentage
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, NA)) +  # Start y-axis at 0
    # Improve theme elements
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),  # Larger facet titles
      legend.position = "none",  # Remove redundant legend
      panel.spacing = unit(1.5, "lines"),  # More space between facets
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      plot.title = element_text(size = 14, face = "bold"),  # Larger plot title
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      axis.title = element_blank()# More margin spac
    ) 
  
  # Create a Shiny app based on the above. Each geographic level should be its own tab
  # State, CBSA, and locality. The filter should be present on the CBSA and locality plot
  # as a drop down filter.
  
  ## ---- Poverty Rate by Age - Table B17001 -----
  
  # The following creates visualizations based on Table B17001 from the ACS, which
  # will be incorporated into a Shiny app.The visualizations will provide a line graph 
  # showing the poverty rates of different households based on age group from 
  # 2010 to the latest year.
  
  # Read in the latest data. Use the here package in the Shiny app.
  
  poverty_age <- read_rds("data/poverty_age.rds")

  # Aggregate data based on geographic levels and calculate the poverty rate.
  
  pov_age_state <- poverty_age %>% 
    group_by(year, age) %>% 
    summarise(estimate = sum(estimate),
              totalage = sum(totalage)) %>% 
    mutate(rate = estimate/totalage) %>% 
    ungroup()
  
  pov_age_cbsa <- poverty_age %>% 
    group_by(year, age, cbsa_title) %>% 
    summarise(estimate = sum(estimate),
              totalage = sum(totalage)) %>% 
    mutate(rate = estimate/totalage) %>% 
    ungroup()
  
  pov_age_local <- poverty_age  
  
  # Set placeholders for filters by year. The state visualization will not 
  # have any filters.
  
  state <- pov_age_state 

  cbsa <- pov_age_cbsa %>% 
    filter(cbsa_title == "Richmond, VA") 
  
  local <- pov_age_local %>% 
    filter(locality == "Richmond city")
  
  # Create data visualizations that compare poverty rate by age over time.
  
  ggplot(state,
         aes(
           x = year,
           y = rate,
           color = age,
           group = age
         )) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    # Change to 2 rows instead of 1 for better proportions
    facet_wrap(~age_group, nrow = 1) +
    # Create a custom color palette for age groups
    scale_color_manual(values = c(
      "17 years and under" = "#FFC658", # Desert
      "18 to 24 years" = "#E0592A",     # HousingX Orange
      "25 to 34 years" = "#259591",     # Grass
      "35 to 44 years" = "#40C0C0",     # Sky
      "45 to 54 years" = "#8B85CA",     # Lilac
      "55 to 64 years" = "#B1005F",     # Berry
      "65 years and over" = "#FF7276"   # HousingX Red
    )) +
    # Format y-axis as percentage
    scale_y_continuous(labels = scales::percent_format()) +
    # Show fewer years on x-axis
    scale_x_discrete(breaks = seq(min(state$year), max(state$year), by = 5)) +
    # Clean up the appearance
    theme_minimal() +
    theme(
      # Remove redundant legend
      legend.position = "none",
      # Increase facet title size
      strip.text = element_text(size = 12, face = "bold"),
      # More space between facets
      panel.spacing = unit(1.5, "lines"),
      # Remove minor gridlines
      panel.grid.minor = element_blank(),
      # Add more margin space
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      # Increase title text size
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ggplot(cbsa,
         aes(
           x = year,
           y = rate,
           color = age,
           group = age
         )) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    # Change to 2 rows instead of 1 for better proportions
    facet_wrap(~age_group, nrow = 1) +
    # Create a custom color palette for age groups
    scale_color_manual(values = c(
      "17 years and under" = "#FFC658", # Desert
      "18 to 24 years" = "#E0592A",     # HousingX Orange
      "25 to 34 years" = "#259591",     # Grass
      "35 to 44 years" = "#40C0C0",     # Sky
      "45 to 54 years" = "#8B85CA",     # Lilac
      "55 to 64 years" = "#B1005F",     # Berry
      "65 years and over" = "#FF7276"   # HousingX Red
    )) +
    # Format y-axis as percentage
    scale_y_continuous(labels = scales::percent_format()) +
    # Show fewer years on x-axis
    scale_x_discrete(breaks = seq(min(cbsa$year), max(cbsa$year), by = 5)) +
    # Clean up the appearance
    theme_minimal() +
    theme(
      # Remove redundant legend
      legend.position = "none",
      # Increase facet title size
      strip.text = element_text(size = 12, face = "bold"),
      # More space between facets
      panel.spacing = unit(1.5, "lines"),
      # Remove minor gridlines
      panel.grid.minor = element_blank(),
      # Add more margin space
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      # Increase title text size
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ggplot(local,
         aes(
           x = year,
           y = rate,
           color = age,
           group = age
         )) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~age_group, nrow = 1) +
    # Create a custom color palette for age groups
    scale_color_manual(values = c(
      "17 years and under" = "#FFC658", # Desert
      "18 to 24 years" = "#E0592A",     # HousingX Orange
      "25 to 34 years" = "#259591",     # Grass
      "35 to 44 years" = "#40C0C0",     # Sky
      "45 to 54 years" = "#8B85CA",     # Lilac
      "55 to 64 years" = "#B1005F",     # Berry
      "65 years and over" = "#FF7276"   # HousingX Red
    )) +
    # Format y-axis as percentage
    scale_y_continuous(labels = scales::percent_format()) +
    # Show fewer years on x-axis
    scale_x_discrete(breaks = seq(min(local$year), max(local$year), by = 5)) +
    # Clean up the appearance
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title = element_blank(),
      # Increase facet title size
      strip.text = element_text(size = 12, face = "bold"),
      # More space between facets
      panel.spacing = unit(1.5, "lines"),
      # Remove minor gridlines
      panel.grid.minor = element_blank(),
      # Add more margin space
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      # Increase title text size
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      )
  
  # Create shiny apps for the plots above where each plot/geographic-level has its
  # own tab. Then filters should exist for the CBSA and locality plots, so that you can
  # filter for individual CBSA or locality. There should be interactivity that allows
  # users to interact with the data and see what the value of each point is. There should
  # be labels at the end of each line to show the latest value.
  
## ---- Housing Type by Tenure - Table B25032 -----
  
  # The following creates visualizations based on Table B17001 from the ACS, which
  # will be incorporated into a Shiny app.The visualizations will provide a line graph 
  # showing the poverty rates of different households based on age group from 
  # 2010 to the latest year.
  
  # Read in the latest data. Use the here package in the Shiny app.
  
  b25032 <- read_rds("data/b25032.rds")
  
  # Aggregate data based on geographic levels and calculate what percentage the value is out of 
  # the tenure group ('percent') and then the geographic level ('percent total')
  
  state_housing <- b25032 %>% 
    group_by(year, tenure, type) %>% 
    summarise(estimate = sum(estimate)) %>% 
    group_by(year,tenure) %>% 
    mutate(percent = estimate/sum(estimate)) %>% 
    group_by(year) %>% 
    mutate(percent_total = estimate/sum(estimate))
  
  cbsa_housing <- b25032 %>% 
    group_by(year, cbsa_title, tenure, type) %>% 
    summarise(estimate = sum(estimate)) %>% 
    group_by(year, cbsa_title, tenure) %>% 
    mutate(percent = estimate/sum(estimate)) %>% 
    group_by(year, cbsa_title) %>% 
    mutate(percent_total = estimate/sum(estimate))

  local_housing <- b25032  %>% 
    group_by(year, name_long, tenure) %>% 
    mutate(percent = estimate/sum(estimate)) %>% 
    group_by(year, name_long) %>% 
    mutate(percent_total = estimate/sum(estimate))
  
  # Set placeholders for filters by year and location. The state visualization will
  # not have any filters for location.
  
  state <- state_housing %>% 
    filter(year == 2023)
  
  cbsa <- cbsa_housing %>% 
    filter(year == 2023,
           cbsa_title == "Richmond, VA")
  
  local <- local_housing %>% 
    filter(year == 2023,
           name_long == "Richmond City")
  
  # Create a variable for the title text.
  
  title_text <- "Distribution of housing type"
  
  
  # Create data visualizations based on the above that set style and form
  # for the Shiny app.
  
ggplot(state, 
       aes(x = reorder(type, -percent), 
           y = percent, 
           fill = tenure)) +
  geom_col(position = "dodge") +
  # Add labels that match fill color
  geom_text(aes(label = scales::percent(percent, accuracy = 1), 
                color = tenure),
            position = position_dodge(width = 0.9),
            hjust = -0.2) +  # Negative hjust moves labels outside bars
  facet_wrap(~tenure) +
  scale_fill_hfv() + 
  # Use the same color palette for text as for fill
  scale_color_hfv() +
  coord_flip() +
  # Expand the plot area to make room for labels
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.2))) +  # Add 20% expansion on right side
  labs(
    title = title_text
  ) +
  theme_hfv() +
  theme(legend.position = "none",
        strip.text = element_blank())
  
  ggplot(cbsa, 
         aes(x = reorder(type, -percent), 
             y = percent, 
             fill = tenure)) +
    geom_col(position = "dodge") +
    # Add labels that match fill color
    geom_text(aes(label = scales::percent(percent, accuracy = 1), 
                  color = tenure),
              position = position_dodge(width = 0.9),
              hjust = -0.2) +  # Negative hjust moves labels outside bars
    facet_wrap(~tenure) +
    scale_fill_hfv() + 
    # Use the same color palette for text as for fill
    scale_color_hfv() +
    coord_flip() +
    # Expand the plot area to make room for labels
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.2))) +  # Add 20% expansion on right side
    labs(
      title = title_text
    ) +
    theme_hfv() +
    theme(legend.position = "none",
          strip.text = element_blank())
  
  ggplot(local, 
         aes(x = reorder(type, -percent), 
             y = percent, 
             fill = tenure)) +
    geom_col(position = "dodge") +
    # Add labels that match fill color
    geom_text(aes(label = scales::percent(percent, accuracy = 1), 
                  color = tenure),
              position = position_dodge(width = 0.9),
              hjust = -0.2) +  # Negative hjust moves labels outside bars
    facet_wrap(~tenure) +
    scale_fill_hfv() + 
    # Use the same color palette for text as for fill
    scale_color_hfv() +
    coord_flip() +
    # Expand the plot area to make room for labels
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.2))) +  # Add 20% expansion on right side
    labs(
      title = title_text
    ) +
    theme_hfv() +
    theme(legend.position = "none",
          strip.text = element_blank())
  
  # Create shiny apps for the above three plots where each visualization is its own tab. The filter should
  # allow users to change between different years for each plot and different locations. There should be interactivity
  # that allows users to hover over a bar to see the actual value.
  
  ## ---- Housing Type by Tenure and Year Built ----- ##
  
  b25127 <- read_rds("data/b25127.rds")
  
  # Define the desired order for structure types
  structure_order <- c("1, detached or attached", 
                       "2 to 4", 
                       "5 to 19", 
                       "20 to 49", 
                       "50 or more", 
                       "Mobile home, boat, RV, van, etc.") 
  
  state_housing_built <- b25127 |> 
    group_by(year, tenure, yrbuilt, structure) |> 
    summarise(estimate = sum(estimate)) |> 
    ungroup() |> 
    mutate(structure = factor(structure, levels = structure_order))
  
  cbsa_housing_built <-  b25127 |> 
    group_by(year, cbsa_title, tenure, yrbuilt, structure) |> 
    summarise(estimate = sum(estimate))  |> 
    ungroup() |> 
    mutate(structure = factor(structure, levels = structure_order))
  
  local_housing_built <-  b25127  |> 
    mutate(structure = factor(structure, levels = structure_order))
  
  # Define a color palette using the HousingX colors
  housing_palette <- c(
    "1, detached or attached" = "#011E41",  # Shadow - dark blue for largest category
    "2 to 4" = "#259591",                   # Grass - teal
    "5 to 19" = "#8B85CA",                  # Lilac
    "20 to 49" = "#B1005F",                 # Berry
    "50 or more" = "#E0592A",               # Desert - orange
    "Mobile home, boat, RV, van, etc." = "#FFC658"  # Orange HousingX - gold
  )
  
  # Create filter placeholders.
  
  state <- state_housing_built |> 
    filter(year == 2023)
  
  cbsa <- cbsa_housing_built |> 
    filter(year == 2023, 
           cbsa_title == "Richmond, VA")
  
  local <- local_housing_built |>
    filter(year == 2023, 
           name_long == "Richmond City")
  
  # Create visualizations showing the distribution of housing type and tenure by
  # structure type.
  
  ggplot(state,
         aes(x = yrbuilt,
             y = estimate,
             fill = structure)) +
    geom_col(position = "stack") +
    facet_wrap(~tenure) +
    coord_flip() +
    scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    labs(title = "Distribution of housing stock by year built and tenure") +
    theme_minimal() +
    scale_fill_manual(values = housing_palette) +
    theme(legend.title = element_blank(),
          axis.title = element_blank(),
          plot.title.position = "plot",
          legend.position = "bottom",
          legend.direction = "horizontal"
    )
  
  
  ggplot(cbsa,
         aes(x = yrbuilt,
             y = estimate,
             fill = structure)) +
    geom_col(position = "stack") +
    facet_wrap(~tenure) +
    coord_flip() +
    scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    labs(title = "Distribution of housing stock by year built and tenure") +   
    theme_minimal() +
    scale_fill_manual(values = housing_palette) +
    theme(legend.title = element_blank(),
          axis.title = element_blank(),
          plot.title.position = "plot",
          legend.position = "bottom",
          legend.direction = "horizontal"
    )
  
  
  ggplot(local,
         aes(x = yrbuilt,
             y = estimate,
             fill = structure)) +
    geom_col(position = "stack") +
    facet_wrap(~tenure) +
    coord_flip() +
    scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    labs(title = "Distribution of housing stock by year built and tenure") +
    theme_minimal() +
    scale_fill_manual(values = housing_palette) +
    theme(legend.title = element_blank(),
          axis.title = element_blank(),
          plot.title.position = "plot",
          legend.position = "bottom",
          legend.direction = "horizontal"
    )
# Create shiny apps for the above where each plot geography is its own tab. There should only be a
# filter for geography in the CBSA and locality tabs. There is no need for a year filter in this shiny
# app.
  
## ---- Tenure by Bedrooms: Table B25042 ----
  
  # The following creates visualizations based on Table B25042 from the ACS, which
  # will be incorporated into a Shiny app. The visualizations will provide a comparative 
  # bar chart that shows the distribution of housing units based on bedroom count and tenure.
  
  # Read in the latest data. Use the here package in the Shiny app.
  
b25042 <- read_rds("data/b25042.rds")
  
# Define the desired order for bedrooms in the visualization.
bedroom_order <- c("No bedroom", "1 bedroom", "2 bedrooms", "3 bedrooms", 
                     "4 bedrooms", "5 or more bedrooms")

# Aggregate the data based on geographic level and re-level the br variable
# based on the desired order.
  
state_bed <- b25042 |> 
  group_by(year, tenure, br) |> 
  summarise(estimate = sum(estimate)) %>%
  mutate(br = factor(br, levels = bedroom_order))

cbsa_bed <- b25042 |> 
  group_by(year, cbsa_title, tenure, br) |> 
  summarise(estimate = sum(estimate)) %>%
  mutate(br = factor(br, levels = bedroom_order))

local_bed <- b25042 |> 
  group_by(year, name_long, tenure, br) |> 
  summarise(estimate = sum(estimate)) %>%
  mutate(br = factor(br, levels = bedroom_order))

# Set placeholders for filters by year and location. The state visualization will
# not have any filters for location.

state <- state_bed |> 
  filter(year == 2023)


cbsa <- cbsa_bed |> 
  filter(year == 2023,
         cbsa_title == "Richmond, VA")

local <- local_bed |> 
  filter(year == 2023,
         name_long == "Richmond City")


# Create visualizations for the basis of the Shiny apps. These visualizations set the 
# style and form of the plots. The hdatools package needs to be called to utilize the 
# scale_fill_hfv() function. tidyverse package should be called at the beginning.

ggplot(state,
       aes(
         x = br,
         y = estimate,
         fill = tenure)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~tenure) +
  scale_fill_hfv() +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  labs(title = "Distribution of housing be bedroom count and tenure") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    legend.position = "none",
    axis.title = element_blank()
  )


ggplot(cbsa,
       aes(
         x = br,
         y = estimate,
         fill = tenure)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~tenure) +
  scale_fill_hfv() +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  labs(title = "Distribution of housing be bedroom count and tenure") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    legend.position = "none",
    axis.title = element_blank()
  )


ggplot(local,
       aes(
         x = br,
         y = estimate,
         fill = tenure)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~tenure) +
  scale_fill_hfv() +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  labs(title = "Distribution of housing be bedroom count and tenure") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    legend.position = "none",
    axis.title = element_blank()
  )

# Create shiny apps for the above where each plot geography is its own tab. There should only be a
# filter for geography in the CBSA and locality tabs. The year filter should be present across all tabs.
# There should be interactivity that allows users to hover over a bar to see the actual value of the bar.

## ---- Overcrowding ----

# The following creates visualizations based on Table B25042 from the ACS, which
# will be incorporated into a Shiny app. The visualizations will provide a comparative 
# bar chart that shows the distribution of housing units based on bedroom count and tenure.

# Read in the latest data. Use the here package in the Shiny app.

b25014 <- read_rds("data/b25014.rds")

# Aggregate the data based on geographic level and then calculate what percentage of households
# are overcrowded or overcrowded based on the year and tenure.

state_crowd <- b25014 |> 
  group_by(year, tenure,overcrowded) |> 
  summarise(estimate = sum(estimate))  |> 
  ungroup() |> 
  group_by(year, tenure) |> 
  mutate(percent = estimate/sum(estimate))


cbsa_crowd <- b25014 |> 
  group_by(year, cbsa_title, tenure, overcrowded) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  group_by(year, cbsa_title, tenure) |> 
  mutate(percent = estimate/sum(estimate))

local_crowd <- b25014 |> 
  group_by(year, name_long, tenure, opr, overcrowded) |> 
  summarise(estimate = sum(estimate))  |> 
  ungroup() |> 
  group_by(year, name_long, tenure) |> 
  mutate(percent = estimate/sum(estimate))



state <- state_crowd |> 
  filter(year == 2023) |> 
  filter(overcrowded != "Not overcrowded")

cbsa <- cbsa_crowd |> 
  filter(year == 2023, 
         cbsa_title == "Richmond, VA") |> 
  filter(overcrowded != "Not overcrowded")

local <- local_crowd |> 
  filter(year == 2023, 
         name_long == "Richmond City") |> 
  filter(overcrowded != "Not overcrowded")


ggplot(state,
       aes(
         x = overcrowded,
         y = percent,
         fill = tenure)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 0.03),  # Adjust based on your data
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_hfv() +
  labs(
    title = "Housing Overcrowding Rates by Tenure",
    subtitle = "Renters experience higher overcrowding rates than homeowners",
    x = "Overcrowding Category",
    y = "Percent of Households",
    fill = "Housing Tenure"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )


ggplot(cbsa,
       aes(
         x = overcrowded,
         y = percent,
         fill = tenure)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 0.03),  # Adjust based on your data
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_hfv() +
  labs(
    title = "Housing Overcrowding Rates by Tenure",
    subtitle = "Renters experience higher overcrowding rates than homeowners",
    x = "Overcrowding Category",
    y = "Percent of Households",
    fill = "Housing Tenure"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )


ggplot(local,
       aes(
         x = overcrowded,
         y = percent,
         fill = tenure)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 0.03),  # Adjust based on your data
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_hfv() +
  labs(
    title = "Housing Overcrowding Rates by Tenure",
    subtitle = "Renters experience higher overcrowding rates than homeowners",
    x = "Overcrowding Category",
    y = "Percent of Households",
    fill = "Housing Tenure"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

# Create shiny apps for the above where each plot geography is its own tab. There should only be a
# filter for geography in the CBSA and locality tabs. The year filter should be present across all tabs.

## ----- Homeownership Rate: Table B25003 -----


# Create an interconnected visualization of a map and a line graph showing the homeownership rate of Virginia.
# The map is interactive so that when users click on a jurisdiction, the line graph will adjust to show the
# homeownership rate in that jurisdiction over time.

library(tigris)
library(leaflet)
library(sf)
library(dplyr)

# Only load packages you actually need
# mapgl and tidyverse are loaded but not used directly

# Get only the 50 states plus DC
options(tigris_use_cache = TRUE)  # Cache tigris data for faster future use
states_us <- states(cb = TRUE) %>%  # Use cb=TRUE for cartographic boundaries (smaller)
  filter(STUSPS %in% c(state.abb, "DC")) # 50 states + DC

# Read and prepare data
b25003_state <- readRDS("data/b25003_state.rds") %>% 
  mutate(ho_rate = est_owner/est_all,
         GEOID = fips) %>%
  filter(year == 2023)

# More efficient join
b25003_state_geo <- left_join(states_us, b25003_state, by = "GEOID") %>%
  # Transform to WGS84
  st_transform(4326) %>%
  # More careful simplification based on area
  st_simplify(preserveTopology = TRUE, 
              dTolerance = 0.001 * sqrt(as.numeric(st_area(.)))) 

# Create color palette
pal <- colorBin(
  palette = "Blues",
  domain = b25003_state_geo$ho_rate,
  bins = seq(0.3, 0.8, by = 0.05)
)

# Create map with better positioning for US
leaflet(b25003_state_geo) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal(ho_rate),
    weight = 1,
    opacity = 1,
    color = "#333333",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    # Add tooltips for better interactivity
    label = ~paste0(NAME, ": ", round(ho_rate * 100, 1), "% homeownership"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto"
    )
  ) %>%
  # Set view to focus on continental US
  setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
  # Add legend
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~ho_rate,
    title = "Homeownership Rate",
    opacity = 0.7,
    labFormat = labelFormat(suffix = "%", transform = function(x) x * 100)
  )

,
    label = ~paste0(NAME, ": ", round(ho_rate * 100, 1), "% homeownership rate"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~ho_rate,
    title = "Homeownership Rate",
    opacity = 0.7,
    labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x)
  )





