# BC Socio-Economic Index Shiny App
# Copyright 2025 Province of British Columbia

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(readr)
library(stringr)
library(scales)
library(RColorBrewer)

# Load sample data (replace with your actual data files)
# For demo purposes, creating sample data structure
create_sample_data <- function() {
  set.seed(123)

  regions <- c(
    "Fernie",
    "Arrow Lakes",
    "Burnaby Heights/Capital Hill",
    "Vancouver Downtown",
    "Surrey Central",
    "Richmond Centre",
    "North Vancouver",
    "West Vancouver"
  )
  years <- 2016:2023
  models <- c("Longitudinal", "Detail")
  indices <- c(
    "COMMUNITY_0_100",
    "ECON_0_100",
    "EDUC_0_100",
    "HEALTH_0_100",
    "SEI_INDEX_0_100",
    "TOTAL_INDEX_0_100"
  )

  # Create sample long format data
  sample_data <- expand.grid(
    # UID = 1:length(regions),
    REGION_NAME = regions,
    CALENDAR_YEAR = years,
    MODEL_TYPE = models,
    INDEX_TYPE = indices,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      AVERAGE_AGE = runif(n(), 35, 45),
      POPULATION_ESTIMATE = sample(5000:50000, n(), replace = TRUE),
      TOTAL = runif(n(), 30, 70),
      REGION_LEVEL = "CHSA",
      INDEX_LABEL = case_when(
        INDEX_TYPE == "COMMUNITY_0_100" ~ "Community",
        INDEX_TYPE == "ECON_0_100" ~ "Economy",
        INDEX_TYPE == "EDUC_0_100" ~ "Education",
        INDEX_TYPE == "HEALTH_0_100" ~ "Health",
        INDEX_TYPE == "SEI_INDEX_0_100" ~ "SEI Index",
        INDEX_TYPE == "TOTAL_INDEX_0_100" ~ "Total Index"
      )
    )

  # Create contribution data
  factors <- c(
    "COMMUNITY_SING_PARENT_RATIO",
    "COMMUNITY_AVG_LONE_PARENT_AT_BIRTH",
    "COMMUNITY_CYIC_PER_1000",
    "COMMUNITY_TA_PROP",
    "COMMUNITY_DA_PROP",
    "ECON_MEDIAN_INCOME",
    "ECON_UNEMPLOYMENT_RATE",
    "ECON_LOW_INCOME",
    "EDUC_NO_CERTIFICATE",
    "EDUC_UNIVERSITY_DEGREE",
    "HEALTH_LIFE_EXPECTANCY"
  )

  contribution_data <- expand.grid(
    # UID = 1:length(regions),
    REGION_NAME = regions,
    CALENDAR_YEAR = years,
    MODEL_TYPE = models,
    INDEX_TYPE = indices,
    FACTOR = factors,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      AVERAGE_AGE = runif(n(), 35, 45),
      POPULATION_ESTIMATE = sample(5000:50000, n(), replace = TRUE),
      TOTAL = runif(n(), 30, 70),
      CONTRIBUTION_VALUE = runif(n(), -5, 5),
      REGION_LEVEL = "CHSA",
      INDEX_LABEL = case_when(
        INDEX_TYPE == "COMMUNITY_0_100" ~ "Community",
        INDEX_TYPE == "ECON_0_100" ~ "Economy",
        INDEX_TYPE == "EDUC_0_100" ~ "Education",
        INDEX_TYPE == "HEALTH_0_100" ~ "Health",
        INDEX_TYPE == "SEI_INDEX_0_100" ~ "SEI Index",
        INDEX_TYPE == "TOTAL_INDEX_0_100" ~ "Total Index"
      ),
      FACTOR_LABEL = case_when(
        FACTOR == "COMMUNITY_SING_PARENT_RATIO" ~
          "Ratio of total one-parent families among census families",
        FACTOR == "COMMUNITY_AVG_LONE_PARENT_AT_BIRTH" ~
          "Ratio of parents who were single when mothers gave birth to children",
        FACTOR == "COMMUNITY_CYIC_PER_1000" ~
          "Count of children and youth in care per thousand population",
        FACTOR == "COMMUNITY_TA_PROP" ~
          "Ratio of residents who were in temporary assistance within DA",
        FACTOR == "COMMUNITY_DA_PROP" ~
          "Ratio of residents who were in disability assistance within DA",
        FACTOR == "ECON_MEDIAN_INCOME" ~ "Median household income",
        FACTOR == "ECON_UNEMPLOYMENT_RATE" ~ "Unemployment rate",
        FACTOR == "ECON_LOW_INCOME" ~ "Low income rate",
        FACTOR == "EDUC_NO_CERTIFICATE" ~ "No certificate, diploma or degree",
        FACTOR == "EDUC_UNIVERSITY_DEGREE" ~ "University degree",
        FACTOR == "HEALTH_LIFE_EXPECTANCY" ~ "Life expectancy",
        TRUE ~ FACTOR
      )
    )

  return(list(index_data = sample_data, contribution_data = contribution_data))
}

# Load or create data
data_list <- create_sample_data()
index_data <- data_list$index_data
contribution_data <- data_list$contribution_data

# UI
ui <- dashboardPage(
  dashboardHeader(title = "BC Socio-Economic Index Dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Time Series Analysis",
        tabName = "timeseries",
        icon = icon("line-chart")
      ),
      menuItem(
        "Regional Comparison",
        tabName = "comparison",
        icon = icon("bar-chart")
      ),
      menuItem("Geographic View", tabName = "geographic", icon = icon("map")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML(
        "
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "
      ))
    ),

    tabItems(
      # Time Series Tab
      tabItem(
        tabName = "timeseries",
        fluidRow(
          box(
            title = "Filters",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            selectInput(
              "ts_region",
              "Select Region:",
              choices = unique(index_data$REGION_NAME),
              selected = unique(index_data$REGION_NAME)[1]
            ),
            selectInput(
              "ts_model",
              "Model Type:",
              choices = unique(index_data$MODEL_TYPE),
              selected = "Longitudinal"
            ),
            checkboxGroupInput(
              "ts_indices",
              "Select Indices:",
              choices = unique(index_data$INDEX_LABEL),
              selected = unique(index_data$INDEX_LABEL)
            )
          ),
          box(
            title = "Evolution of Key Index Values Over Time",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            plotlyOutput("timeseries_plot", height = "500px")
          )
        )
      ),

      # Regional Comparison Tab
      tabItem(
        tabName = "comparison",
        fluidRow(
          box(
            title = "Filters",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            selectInput(
              "comp_model",
              "Model Type:",
              choices = unique(index_data$MODEL_TYPE),
              selected = "Longitudinal"
            ),
            selectInput(
              "comp_year",
              "Calendar Year:",
              choices = sort(
                unique(index_data$CALENDAR_YEAR),
                decreasing = TRUE
              ),
              selected = max(index_data$CALENDAR_YEAR)
            ),
            selectInput(
              "comp_region_a",
              "Region A:",
              choices = unique(index_data$REGION_NAME),
              selected = unique(index_data$REGION_NAME)[1]
            ),
            selectInput(
              "comp_region_b",
              "Region B:",
              choices = unique(index_data$REGION_NAME),
              selected = unique(index_data$REGION_NAME)[2]
            ),
            selectInput(
              "comp_index",
              "Index Type:",
              choices = unique(index_data$INDEX_LABEL),
              selected = "SEI Index"
            )
          ),
          box(
            title = "Regional Index Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            plotlyOutput("comparison_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Factor Contributions",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("contribution_plot", height = "400px")
          )
        )
      ),

      # Geographic Tab
      tabItem(
        tabName = "geographic",
        fluidRow(
          box(
            title = "Filters",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            selectInput(
              "geo_model",
              "Model Type:",
              choices = unique(index_data$MODEL_TYPE),
              selected = "Longitudinal"
            ),
            selectInput(
              "geo_year",
              "Calendar Year:",
              choices = sort(
                unique(index_data$CALENDAR_YEAR),
                decreasing = TRUE
              ),
              selected = max(index_data$CALENDAR_YEAR)
            ),
            selectInput(
              "geo_index",
              "Index Type:",
              choices = unique(index_data$INDEX_LABEL),
              selected = "SEI Index"
            )
          ),
          box(
            title = "Geographic Distribution of Indices",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            leafletOutput("map", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Regional Data Table",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("geo_table")
          )
        )
      ),

      # Data Explorer Tab
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(
            title = "Data Filters",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(
                3,
                selectInput(
                  "exp_model",
                  "Model Type:",
                  choices = c("All", unique(index_data$MODEL_TYPE)),
                  selected = "All"
                )
              ),
              column(
                3,
                selectInput(
                  "exp_region",
                  "Region:",
                  choices = c("All", unique(index_data$REGION_NAME)),
                  selected = "All"
                )
              ),
              column(
                3,
                selectInput(
                  "exp_year",
                  "Year:",
                  choices = c("All", sort(unique(index_data$CALENDAR_YEAR))),
                  selected = "All"
                )
              ),
              column(
                3,
                selectInput(
                  "exp_index",
                  "Index Type:",
                  choices = c("All", unique(index_data$INDEX_LABEL)),
                  selected = "All"
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Index Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("explorer_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Time Series Plot
  output$timeseries_plot <- renderPlotly({
    filtered_data <- index_data %>%
      filter(
        REGION_NAME == input$ts_region,
        MODEL_TYPE == input$ts_model,
        INDEX_LABEL %in% input$ts_indices
      )

    p <- ggplot(
      filtered_data,
      aes(x = CALENDAR_YEAR, y = TOTAL, color = INDEX_LABEL)
    ) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = paste(
          "Evolution of Key Index Values Over Time in",
          input$ts_region
        ),
        subtitle = paste("Model:", input$ts_model),
        x = "Year",
        y = "Average Index Value",
        color = "Index Type"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p, tooltip = c("x", "y", "colour"))
  })

  # Regional Comparison Plot
  output$comparison_plot <- renderPlotly({
    filtered_data <- index_data %>%
      filter(
        MODEL_TYPE == input$comp_model,
        CALENDAR_YEAR == input$comp_year,
        REGION_NAME %in% c(input$comp_region_a, input$comp_region_b),
        INDEX_LABEL == input$comp_index
      )

    p <- ggplot(
      filtered_data,
      aes(x = REGION_NAME, y = TOTAL, fill = REGION_NAME)
    ) +
      geom_col(width = 0.6) +
      geom_text(aes(label = round(TOTAL, 1)), vjust = -0.5) +
      labs(
        title = paste("Index Comparison:", input$comp_index),
        subtitle = paste(
          "Year:",
          input$comp_year,
          "| Model:",
          input$comp_model
        ),
        x = "Region",
        y = "Index Value"
      ) +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p, tooltip = c("x", "y"))
  })

  # Factor Contribution Plot
  output$contribution_plot <- renderPlotly({
    filtered_contrib <- contribution_data %>%
      filter(
        MODEL_TYPE == input$comp_model,
        CALENDAR_YEAR == input$comp_year,
        REGION_NAME == input$comp_region_a,
        INDEX_LABEL == input$comp_index
      ) %>%
      arrange(desc(abs(CONTRIBUTION_VALUE))) %>%
      head(10)

    p <- ggplot(
      filtered_contrib,
      aes(
        x = reorder(FACTOR_LABEL, CONTRIBUTION_VALUE),
        y = CONTRIBUTION_VALUE,
        fill = ifelse(CONTRIBUTION_VALUE > 0, "Positive", "Negative")
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(
        values = c("Positive" = "#2E8B57", "Negative" = "#DC143C")
      ) +
      labs(
        title = paste("Factor Contributions for", input$comp_region_a),
        subtitle = paste(input$comp_index, "- Top 10 Factors"),
        x = "Factor",
        y = "Contribution Value",
        fill = "Impact"
      ) +
      theme_minimal()

    ggplotly(p, tooltip = c("x", "y"))
  })

  # Map
  output$map <- renderLeaflet({
    # Create sample coordinates for regions
    sample_coords <- data.frame(
      REGION_NAME = unique(index_data$REGION_NAME),
      lat = c(49.5, 49.1, 49.2, 49.3, 49.1, 49.2, 49.3, 49.4),
      lng = c(-115.1, -117.8, -122.9, -123.1, -122.8, -123.1, -123.0, -123.2)
    )

    filtered_data <- index_data %>%
      filter(
        MODEL_TYPE == input$geo_model,
        CALENDAR_YEAR == input$geo_year,
        INDEX_LABEL == input$geo_index
      ) %>%
      left_join(sample_coords, by = "REGION_NAME")

    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = ~ sqrt(TOTAL),
        popup = ~ paste(
          "<b>",
          REGION_NAME,
          "</b><br>",
          "Index Value:",
          round(TOTAL, 2),
          "<br>",
          "Population:",
          scales::comma(POPULATION_ESTIMATE)
        ),
        color = ~ colorNumeric("RdYlBu", TOTAL)(TOTAL),
        fillOpacity = 0.7
      ) %>%
      addLegend(
        "bottomright",
        pal = colorNumeric("RdYlBu", filtered_data$TOTAL),
        values = ~TOTAL,
        title = "Index Value"
      )
  })

  # Geographic Table
  output$geo_table <- DT::renderDataTable({
    filtered_data <- index_data %>%
      filter(
        MODEL_TYPE == input$geo_model,
        CALENDAR_YEAR == input$geo_year,
        INDEX_LABEL == input$geo_index
      ) %>%
      select(REGION_NAME, TOTAL, POPULATION_ESTIMATE, AVERAGE_AGE) %>%
      arrange(desc(TOTAL))

    DT::datatable(
      filtered_data,
      options = list(pageLength = 10, scrollX = TRUE),
      colnames = c("Region", "Index Value", "Population", "Average Age")
    ) %>%
      formatRound(c("TOTAL", "AVERAGE_AGE"), 2) %>%
      formatCurrency("POPULATION_ESTIMATE", currency = "", digits = 0)
  })

  # Explorer Table
  output$explorer_table <- DT::renderDataTable({
    filtered_data <- index_data

    if (input$exp_model != "All") {
      filtered_data <- filtered_data %>% filter(MODEL_TYPE == input$exp_model)
    }
    if (input$exp_region != "All") {
      filtered_data <- filtered_data %>% filter(REGION_NAME == input$exp_region)
    }
    if (input$exp_year != "All") {
      filtered_data <- filtered_data %>%
        filter(CALENDAR_YEAR == as.numeric(input$exp_year))
    }
    if (input$exp_index != "All") {
      filtered_data <- filtered_data %>% filter(INDEX_LABEL == input$exp_index)
    }

    filtered_data <- filtered_data %>%
      select(
        UID,
        REGION_NAME,
        CALENDAR_YEAR,
        INDEX_LABEL,
        TOTAL,
        POPULATION_ESTIMATE,
        AVERAGE_AGE,
        MODEL_TYPE,
        REGION_LEVEL
      )

    DT::datatable(
      filtered_data,
      options = list(pageLength = 15, scrollX = TRUE),
      filter = "top"
    ) %>%
      formatRound(c("TOTAL", "AVERAGE_AGE"), 2) %>%
      formatCurrency("POPULATION_ESTIMATE", currency = "", digits = 0)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
