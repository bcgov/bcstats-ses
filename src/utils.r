#' Plot a Map for a Given Census Subdivision (CSD) Showing the Average Distance to a Specific Facility
#'
#' This function generates a map for a specified Census Subdivision (CSD) that displays the average distance to a given facility (e.g., servicebc, schools, hospitals).
#'
#' @param csd Character string specifying the name of the Census Subdivision (CSD) to plot.
#' @param facility Character string specifying the type of facility to plot (e.g., "servicebc", "schools", "hospitals").
#' @param fill_var Character string specifying the variable name in the data frame that contains the average distance values.
#' @param fill_var_name Character string specifying the name of the variable to use in the legend and labels.
#' @param csd_sf Simple Features object containing the CSD boundaries and average distance data.
#' @param facility_sf Simple Features object containing the locations of the service facilities.
#' @param min_val Numeric value specifying the minimum value for the fill scale (default: 0).
#' @param max_val Numeric value specifying the maximum value for the fill scale (default: 12).
#' @param step Numeric value specifying the step size for the fill scale breaks (default: 1.5).
#' @param label_var Logical value indicating whether to include labels showing the average distance values (default: FALSE).
#'
#' @return A ggplot object representing the map.
#' @export
plot_csd_facility_map_fn <- function(
    csd,
    facility,
    fill_var,
    fill_var_name,
    csd_sf,
    facility_sf,
    min_val = 0,
    max_val = 12,
    step = 1.5,
    label_var = FALSE
) {
  # Define constants for better readability
  CONST_MIN_VAL <- min_val
  CONST_MAX_VAL <- max_val
  CONST_STEP <- step
  
  # Filter CSD data for the specified CSD and facility
  csd_data <- csd_sf %>%
    filter(MUN_NAME_2021 == csd) %>%
    filter(TAG == facility)
  
  # Filter facility data for the specified CSD and facility
  facility_data <- facility_sf %>%
    filter(MUN_NAME_2021 == csd) %>%
    filter(TAG == facility)
  
  # Create the map plot
  plot <- csd_data %>%
    ggplot() +
    geom_sf(aes(fill = !!sym(fill_var))) +
    scale_fill_viridis_c(
      limits = c(CONST_MIN_VAL, CONST_MAX_VAL),
      breaks = seq(CONST_MIN_VAL, CONST_MAX_VAL, by = CONST_STEP)
    )
  
  # Add labels if requested
  if (label_var) {
    plot <- plot + geom_sf_label(aes(label = format(!!sym(fill_var), digits = 2)), colour = "black")
  }
  
  # Add title, theme, and legend
  plot <- plot +
    labs(
      subtitle = glue::glue("{fill_var_name} to the nearest {facility} in {csd}"),
      x = "Longitude",
      y = "Latitude"
    ) +
    my_map_theme +
    guides(fill = guide_legend(title = glue::glue("Avg. {stringr::str_to_title(fill_var_name)} to the {facility}")))
  
  # Add facility points
  plot <- plot +
    geom_point(
      data = facility_data,
      aes(geometry = geometry),
      shape = 23,
      color = "red",
      fill = "red",
      size = 5,
      alpha = 0.5,
      stat = "sf_coordinates"
    )
  
  return(plot)
}



#' Compare Two CSDs in a Map
#'
#' This function compares two Census Subdivisions (CSDs) by plotting their remoteness measurement on a map.
#' It generates two separate plots for each CSD, displaying the average distance to a specified service (e.g., Service BC office),
#' and combines them into a single plot with a shared title.
#'
#' @param CSD1 The name of the first Census Subdivision to compare.
#' @param CSD2 The name of the second Census Subdivision to compare.
#' @param facility The name of the facility to measure remoteness from (e.g., "servicebc").
#' @param fill_var The variable name in the csd_sf data frame that represents the remoteness measurement (e.g., "AVG_DRV_DIST").
#' @param fill_var_name A descriptive name for the fill_var variable to use in the plot legend.
#' @param csd_sf A simple features data frame containing the Census Subdivision boundaries and remoteness measurement data.
#' @param facility_sf A simple features data frame containing the facility locations.
#' @param label_var1 A logical indicating whether to label the first CSD plot with facility names.
#' @param label_var2 A logical indicating whether to label the second CSD plot with facility names.
#'
#' @return This function returns a combined plot of the two CSDs, displaying their remoteness measurement, and saves the plot to a file.
#' @export
compare_two_csd_in_map <- function(
    CSD1 = "Vancouver",
    CSD2 = "Smithers",
    facility = "servicebc",
    fill_var = "AVG_DRV_DIST",
    fill_var_name = "Average distance",
    csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf,
    facility_sf = facility_sf,
    label_var1 = FALSE,
    label_var2 = TRUE
) {
  # two CSDs maps should share the same color scale
  
  color_range <- csd_sf %>%
    filter(MUN_NAME_2021 == c(CSD1, CSC2)) %>%
    filter(TAG %in% c(facility)) %>% 
    filter(!is.na(!!sym(fill_var))) %>%
    pull(!!sym(fill_var)) %>%
    range()
  
  min_val <- floor(color_range[1])
  max_val <- ceiling(color_range[2])
  step <- (max_val - min_val) / (12 - 1)
  
  
  # Define a helper function to generate a single CSD plot
  plot_csd <- function(CSD, label_var, min_val, max_val, step) {
    
    plot_csd_facility_map_fn(
      CSD = CSD,
      facility = facility,
      fill_var = fill_var,
      fill_var_name = fill_var_name,
      csd_sf = csd_sf,
      facility_sf = facility_sf,
      min_val = min_val,
      max_val = max_val,
      step = step,
      label_var = label_var
    )
  }
  
  # Generate plots for both CSDs
  plot_a <- plot_csd(CSD1, label_var1)
  plot_b <- plot_csd(CSD2, label_var2)
  
  # Combine the two plots using patchwork and add a shared title
  combined_plot <- (plot_a + plot_b +
                      plot_layout(ncol = 2, guides = "collect") &
                      theme(legend.position = "bottom")) +
    plot_annotation(
      title = glue::glue("Remoteness measurement: {CSD1} vs {CSD2}"),
      theme = theme(
        plot.title = element_text(
          size = 16,
          face = "bold",
          hjust = 0.5
        )
      )
    )
  
  # Display the combined plot
  print(combined_plot)
  
  # Save the combined plot
  ggsave(
    use_network_path(
      glue::glue(
        "2024 SES Index/data/output/remoteness/two_csd_{CSD1}_{CSD2}_avg_address_dist_{facility}_by_da.png"
      )
    ),
    plot = combined_plot,
    width = 14,
    height = 7,
    dpi = 300
  )
}
