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