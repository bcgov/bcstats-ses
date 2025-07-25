# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

###########################################################################################
# Validation: color map
###########################################################################################

my_map_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


#' Plot a Map for addresses in BC Showing the Average Distance to a Specific Facility
#'
#' This function generates a map for a specified Census Subdivision (CSD) that displays the average distance to a given facility (e.g., servicebc, schools, hospitals).
#'
#' @param csd Character string specifying the name of the Census Subdivision (CSD) to plot.
#' @param facility_name Character string specifying the type of facility to plot (e.g., "servicebc", "schools", "hospitals").
#' @param fill_var Character string specifying the variable name in the data frame that contains the average distance values.
#' @param fill_var_name Character string specifying the name of the variable to use in the legend and labels.
#' @param label_var Logical value indicating whether to include labels showing the average distance values (default: FALSE).
#' @return A ggplot object representing the map.
#' @export
#'
#'
plot_bc_address_map <- function(
  data,
  address_sf = address_sf_with_da,
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average Drive Distance",
  facility_name = "servicebc",
  save_png = T
) {
  address_avg_dist_da_facility_p <- ggplot(
    data = data %>% filter(TAG_2 == facility_name)
  ) +
    geom_sf(
      aes(fill = !!sym(fill_var)),
      color = "gray"
    ) +
    scale_fill_viridis_c(option = "viridis") +
    geom_sf(
      data = address_sf %>% filter(TAG_2 == facility_name),
      color = "red",
      size = 1,
      fill = "red",
      alpha = 0.1
    ) +
    theme_minimal() +
    labs(
      title = glue::glue(
        "{fill_var_name} to {facility_name}"
      )
    ) +
    my_map_theme +
    guides(
      fill = guide_legend(
        title = glue::glue("{fill_var_name}")
      )
    )

  print(address_avg_dist_da_facility_p)
  # Save the plot
  if (save_png) {
    ggsave(
      glue::glue(
        "./out/address_{facility_name}_{fill_var_name}_with_da_map.png"
      ),
      address_avg_dist_da_facility_p,
      width = 10,
      height = 8,
      dpi = 300
    )
  }
}

#' Plot a Map of Census Subdivisions (CSDs) Showing the Average Distance to a Specific Facility
#'
#' This function generates a map of Census Subdivisions (CSDs) displaying the average distance to a specified facility (e.g., servicebc, schools, hospitals).
#'
#' @param csd_shapefile Simple Features object containing the CSD boundaries and average distance data.
#' @param fill_var Character string specifying the variable name in the data frame that contains the average distance values.
#' @param fill_var_name Character string specifying the name of the variable to use in the legend and labels.
#' @param facility_name Character string specifying the type of facility to plot (e.g., "servicebc", "schools", "hospitals").
#' @param save_png Logical value indicating whether to save the plot as a PNG file (default: TRUE).
#'
#' @return A ggplot object representing the CSD map.
#' @export

plot_csd_avg_map_fn <- function(
  csd_shapefile,
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average Drive Distance",
  facility_name = "servicebc",
  save_png = T
) {
  csd_p <- ggplot(
    data = csd_shapefile %>% filter(TAG_2 == facility_name)
  ) +
    geom_sf(aes(fill = !!sym(fill_var)), color = "gray") +
    scale_fill_viridis_c(option = "viridis") +
    labs(
      title = glue::glue("{fill_var_name} to {facility_name} by CSD"),
      subtitle = "Remoteness measurement in BC",
      x = "Longitude",
      y = "Latitude"
    ) +
    my_map_theme +
    guides(
      fill = guide_legend(title = glue::glue("{fill_var_name}"))
    )

  print(csd_p)

  # Save the plot
  if (save_png) {
    ggsave(
      glue::glue("./out/csd_{fill_var_name}_dist_{facility_name}.png"),
      csd_p,
      width = 10,
      height = 8,
      dpi = 300
    )
    ggsave(
      use_network_path(
        glue::glue(
          "2024 SES Index/data/output/remoteness/csd_{fill_var_name}_{facility_name}.png"
        )
      ),
      csd_p,
      width = 10,
      height = 8,
      dpi = 300
    )
  }
}


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
    filter(TAG_2 == facility)

  # Filter facility data for the specified CSD and facility
  facility_data <- facility_sf %>%
    filter(MUN_NAME_2021 == csd) %>%
    filter(TAG_2 == facility)

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
    plot <- plot +
      geom_sf_label(
        aes(label = format(!!sym(fill_var), digits = 2)),
        colour = "black"
      )
  }

  # Add title, theme, and legend
  plot <- plot +
    labs(
      subtitle = glue::glue(
        "{fill_var_name} to {facility}"
      ),
      x = "Longitude",
      y = "Latitude"
    ) +
    my_map_theme +
    guides(
      fill = guide_legend(
        title = glue::glue(
          "{stringr::str_to_title(fill_var_name)}"
        )
      )
    )

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
  label_var2 = TRUE,
  save_png = T
) {
  # two CSDs maps should share the same color scale

  color_range <- csd_sf %>%
    filter(MUN_NAME_2021 == c(CSD1, CSD2)) %>%
    filter(TAG_2 %in% c(facility)) %>%
    filter(!is.na(!!sym(fill_var))) %>%
    pull(!!sym(fill_var)) %>%
    range()

  min_val <- floor(color_range[1])
  max_val <- ceiling(color_range[2])
  step <- (max_val - min_val) / (12 - 1)

  # Generate plots for both CSDs
  plot_a <- plot_csd_facility_map_fn(
    CSD1,
    facility,
    fill_var,
    fill_var_name,
    csd_sf,
    facility_sf,
    min_val,
    max_val,
    step,
    label_var1
  )

  plot_b <- plot_csd_facility_map_fn(
    CSD2,
    facility,
    fill_var,
    fill_var_name,
    csd_sf,
    facility_sf,
    min_val,
    max_val,
    step,
    label_var2
  )

  # Combine the two plots using patchwork and add a shared title
  combined_plot <- (plot_a +
    plot_b +
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
  if (save_png) {
    ggsave(
      glue::glue(
        "./out/{CSD1}_{CSD2}_{facility}_{stringr::str_to_title(fill_var_name)}.png"
      ),
      combined_plot,
      width = 14,
      height = 7,
      dpi = 300
    )

    ggsave(
      use_network_path(
        glue::glue(
          "2024 SES Index/data/output/remoteness/two_csd_{CSD1}_{CSD2}_{facility}_{stringr::str_to_title(fill_var_name)}.png"
        )
      ),
      plot = combined_plot,
      width = 14,
      height = 7,
      dpi = 300
    )
  }
}


tbl_long_cols_mssql <- function(con, schema, table) {
  # Build table name as "schema.table"
  table_full <- DBI::Id(schema = schema, table = table)

  # Get columns info including data type and character length
  cols_info <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT 
         COLUMN_NAME as name, 
         DATA_TYPE as data_type,
         CHARACTER_MAXIMUM_LENGTH as column_size
       FROM INFORMATION_SCHEMA.COLUMNS
       WHERE TABLE_SCHEMA = '%s' AND TABLE_NAME = '%s'",
      schema,
      table
    )
  )

  # Create a sorting field with 3 priority groups:
  # 1. Numeric columns (first)
  # 2. Regular character columns (middle)
  # 3. MAX length columns (-1) (last)
  cols_sorted <- cols_info %>%
    dplyr::mutate(
      # Group 1: Numeric types (int, decimal, etc.)
      # Group 2: Normal character columns
      # Group 3: MAX length columns (-1)
      priority_group = dplyr::case_when(
        # Numeric types (these have NULL/NA in column_size)
        is.na(column_size) ~ 1,
        # MAX length types
        column_size == -1 ~ 3,
        # Regular character types
        TRUE ~ 2
      ),
      # Within each priority group, sort by actual size (or 0 for numeric)
      sort_size = dplyr::if_else(
        is.na(column_size),
        0, # Numeric columns get size 0
        as.numeric(column_size)
      )
    ) %>%
    # First sort by priority group, then by size within group
    dplyr::arrange(priority_group, sort_size) %>%
    dplyr::pull(name)

  # Return the table with columns ordered by our defined logic
  dplyr::tbl(con, dbplyr::in_schema(schema, table)) %>%
    dplyr::select(dplyr::all_of(cols_sorted))
}


# Function to download, unzip, and read dataset
download_and_process_dataset <- function(
  url,
  csv_folder,
  file_name,
  filter_pattern = "scores"
) {
  csv_file <- file.path(csv_folder, file_name)

  if (!file.exists(csv_file)) {
    message(paste0(
      file_name,
      " does not exist. Proceeding to download and process."
    ))

    # Download the ZIP file
    temp_zip <- tempfile(fileext = ".zip")
    download.file(url = url, destfile = temp_zip)

    # Create directory if it doesn't exist
    if (!dir.exists(csv_folder)) {
      dir.create(csv_folder, recursive = TRUE)
    }

    # Unzip the file
    unzip(temp_zip, exdir = csv_folder)

    # List files in the extracted directory to find the CSV file
    csv_files <- list.files(
      path = csv_folder,
      pattern = "\\.csv$",
      full.names = TRUE
    )

    # Filter files that contain the specified pattern
    csv_files <- csv_files[str_detect(csv_files, filter_pattern)]
    data_csv_file <- csv_files[str_detect(csv_files, file_name)]

    message(paste0("Data file found: ", paste(data_csv_file, collapse = ", ")))

    # Read the CSV file(s)
    if (length(data_csv_file) > 0) {
      # Read the first CSV file (or adapt if there are multiple files)
      data <- read_csv(data_csv_file) %>%
        janitor::clean_names(case = "screaming_snake")

      # # Save the processed data
      # write_csv(data, csv_file)

      # Display first few rows
      message("Successfully processed data. First few rows:")
      print(head(data))

      # Return the data
      return(data)
    } else {
      warning("No CSV files found in the extracted directory")
      return(NULL)
    }

    # Clean up the temporary zip file
    unlink(temp_zip)
  } else {
    message(paste0(file_name, " already exists. Reading from file."))
    # If the CSV file already exists, read it directly
    data <- read_csv(csv_file) %>%
      janitor::clean_names(case = "screaming_snake")
    return(data)
  }
}

read_note <- function(csv_folder, file_name, filter_pattern = "notes") {
  # List files in the extracted directory to find the CSV file
  csv_files <- list.files(
    path = csv_folder,
    pattern = "\\.csv$",
    full.names = TRUE
  )

  # Filter files that contain the specified pattern
  note_csv_files <- csv_files[str_detect(csv_files, filter_pattern)]

  note_csv_file <- note_csv_files[str_detect(note_csv_files, file_name)]

  message(paste0("Note file found: ", paste(note_csv_file, collapse = ", ")))

  # Read the CSV file(s)
  if (length(note_csv_file) > 0) {
    # Read the first CSV file (or adapt if there are multiple files)
    note_data <- read_csv(note_csv_file[1]) %>%
      janitor::clean_names(case = "screaming_snake")

    # # Save the processed data
    # write_csv(data, csv_file)

    # Display first few rows
    message("Successfully processed note data. First few rows:")
    print(head(note_data))

    # Return the data
    return(note_data)
  } else {
    warning("No CSV files found in the directory")
    return(NULL)
  }
}
