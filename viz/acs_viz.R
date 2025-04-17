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



