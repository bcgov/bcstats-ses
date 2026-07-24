# Enhanced BC Socio-Economic Index Shiny App
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
library(shinyWidgets)

# Enhanced sample data creation with more realistic patterns
create_enhanced_sample_data <- function() {
  set.seed(123)
  
  regions <- c("100 Mile House", "Abbotsford Rural", "Agassiz/Harrison", "Alberni Valley/Bamfield",
               "Aldergrove/Otter", "Armstrong/Spallumcheen", "Arrow Lakes", "Bella Coola Valley",
               "Blundell", "Bowen Island/Lions Bay", "Brentwood/Willingdon/Parkcrest", 
               "Broadmoor", "Brookswood/Murrayville", "Buckingham/Lakeview/Cariboo/Second Street",
               "Burnaby Heights/Capital Hill", "Burnaby Mountain/Lougheed", "Burns Lake North",
               "Burns Lake South", "Burns Lake Town Centre", "Campbell River", "Campbell River Rural")
  
  years <- 2016:2023
  models <- c("Longitudinal", "Detail")
  indices <- c("COMMUNITY_0_100", "ECON_0_100", "EDUC_0_100", "HEALTH_0_100", "SEI_INDEX_0_100", "TOTAL_INDEX_0_100")
  
  # Create more realistic index values with trends
  sample_data <- expand.grid(
    REGION_NAME = regions,
    CALENDAR_YEAR = years,
    MODEL_TYPE = models,
    INDEX_TYPE = indices,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      UID = as.numeric(as.factor(paste(REGION_NAME, MODEL_TYPE))),
      AVERAGE_AGE = runif(n(), 35, 45),
      POPULATION_ESTIMATE = sample(5000:50000, n(), replace = TRUE),
      # Create more realistic index patterns
      base_value = case_when(
        INDEX_TYPE == "COMMUNITY_0_100" ~ runif(n(), 45, 65),
        INDEX_TYPE == "ECON_0_100" ~ runif(n(), 40, 60),
        INDEX_TYPE == "EDUC_0_100" ~ runif(n(), 35, 55),
        INDEX_TYPE == "HEALTH_0_100" ~ runif(n(), 50, 70),
        INDEX_TYPE == "SEI_INDEX_0_100" ~ runif(n(), 45, 65),
        INDEX_TYPE == "TOTAL_INDEX_0_100" ~ runif(n(), 40, 60)
      ),
      # Add year trend
      year_effect = (CALENDAR_YEAR - 2016) * runif(n(), -0.5, 1.5),
      TOTAL = pmax(0, pmin(100, base_value + year_effect + rnorm(n(), 0, 2))),
      REGION_LEVEL = "CHSA",
      INDEX_LABEL = case_when(
        INDEX_TYPE == "COMMUNITY_0_100" ~ "Community",
        INDEX_TYPE == "ECON_0_100" ~ "Economy", 
        INDEX_TYPE == "EDUC_0_100" ~ "Education",
        INDEX_TYPE == "HEALTH_0_100" ~ "Health",
        INDEX_TYPE == "SEI_INDEX_0_100" ~ "SEI Index",
        INDEX_TYPE == "TOTAL_INDEX_0_100" ~ "Total Index"
      )
    ) %>%
    select(-base_value, -year_effect)
  
  # Enhanced contribution data
  factors <- c("COMMUNITY_SING_PARENT_RATIO", "COMMUNITY_AVG_LONE_PARENT_AT_BIRTH", 
               "COMMUNITY_CYIC_PER_1000", "COMMUNITY_TA_PROP", "COMMUNITY_DA_PROP",
               "ECON_MEDIAN_INCOME", "ECON_UNEMPLOYMENT_RATE", "ECON_LOW_INCOME",
               "EDUC_NO_CERTIFICATE", "EDUC_UNIVERSITY_DEGREE", "HEALTH_LIFE_EXPECTANCY",
               "BENCHMARK")
  
  contribution_data <- expand.grid(
    REGION_NAME = regions,
    CALENDAR_YEAR = years,
    MODEL_TYPE = models,
    INDEX_TYPE = indices,
    FACTOR = factors,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      UID = as.numeric(as.factor(paste(REGION_NAME, MODEL_TYPE))),
      AVERAGE_AGE = runif(n(), 35, 45),
      POPULATION_ESTIMATE = sample(5000:50000, n(), replace = TRUE),
      TOTAL = runif(n(), 30, 70),
      CONTRIBUTION_VALUE = case_when(
        FACTOR == "BENCHMARK" ~ 50,
        TRUE ~ runif(n(), -10, 10)
      ),
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
        FACTOR == "COMMUNITY_SING_PARENT_RATIO" ~ "Ratio of total one-parent families among census families",
        FACTOR == "COMMUNITY_AVG_LONE_PARENT_AT_BIRTH" ~ "Ratio of parents who were single when mothers gave birth to children",
        FACTOR == "COMMUNITY_CYIC_PER_1000" ~ "Count of children and youth in care per thousand population with age between zero and eighteen",
        FACTOR == "COMMUNITY_TA_PROP" ~ "Ratio of residents who were in temporary assistance within DA",
        FACTOR == "COMMUNITY_DA_PROP" ~ "Ratio of residents who were in disability assistance within DA",
        FACTOR == "ECON_MEDIAN_INCOME" ~ "Median household income",
        FACTOR == "ECON_UNEMPLOYMENT_RATE" ~ "Unemployment rate",
        FACTOR == "ECON_LOW_INCOME" ~ "Low income rate",
        FACTOR == "EDUC_NO_CERTIFICATE" ~ "No certificate, diploma or degree",
        FACTOR == "EDUC_UNIVERSITY_DEGREE" ~ "University degree",
        FACTOR == "HEALTH_LIFE_EXPECTANCY" ~ "Life expectancy",
        FACTOR == "BENCHMARK" ~ "BC Benchmark",
        TRUE ~ FACTOR
      )
    )
  
  return(list(index_data = sample_data, contribution_data = contribution_data))
}

# Load data
data_list <- create_enhanced_sample_data()
index_data <- data_list$index_data
contribution_data <- data_list$contribution_data

# Custom CSS
custom_css <- "
.content-wrapper, .right-side {
  background-color: #f8f9fa;
}
.box {
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}
.main-header .navbar {
  background-color: #003366 !important;
}
.main-header .logo {
  background-color: #003366 !important;
}
.sidebar-menu > li.active > a {
  background-color: #003366 !important;
}
.filter-section {
  background-color: white;
  padding: 15px;
  border-radius: 8px;
  margin-bottom: 10px;
}
"

# Enhanced UI
ui <- dashboardPage(
  dashboardHeader(title = "BC Socio-Economic Index Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series Analysis", tabName = "timeseries", icon = icon("line-chart")),
      menuItem("Regional Comparison", tabName = "comparison", icon = icon("bar-chart")),
      menuItem("Geographic Explorer", tabName = "geographic", icon = icon("map")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      # Enhanced Time Series Tab
      tabItem(tabName = "timeseries",
        fluidRow(
          box(
            title = "Filters", status = "primary", solidHeader = TRUE, width = 3,
            div(class = "filter-section",
              h5("Region Selection"),
              pickerInput("ts_region", "Select Region:",
                         choices = sort(unique(index_data$REGION_NAME)),
                         selected = "Burnaby Heights/Capital Hill",
                         options = pickerOptions(liveSearch = TRUE)),
              
              h5("Model Configuration"),
              radioButtons("ts_model", "Model Type:",
                          choices = unique(index_data$MODEL_TYPE),
                          selected = "Longitudinal"),
              
              h5("Index Selection"),
              checkboxGroupInput("ts_indices", "Select Indices:",
                                choices = setNames(unique(index_data$INDEX_LABEL), 
                                                 unique(index_data$INDEX_LABEL)),
                                selected = c("Community", "Economy", "Education", "Health", "SEI Index", "Total Index"))
            )
          ),
          box(
            title = "Evolution of Key Index Values Over Time", status = "primary", 
            solidHeader = TRUE, width = 9,
            div(style = "margin-bottom: 10px;",
                textOutput("ts_subtitle")
            ),
            plotlyOutput("timeseries_plot", height = "500px")
          )
        )
      ),
      
      # Enhanced Regional Comparison Tab  
      tabItem(tabName = "comparison",
        fluidRow(
          box(
            title = "Comparison Settings", status = "primary", solidHeader = TRUE, width = 4,
            div(class = "filter-section",
              h5("Model & Time"),
              radioButtons("comp_model", "Model Type:",
                          choices = unique(index_data$MODEL_TYPE),
                          selected = "Longitudinal"),
              selectInput("comp_year", "Calendar Year:",
                         choices = sort(unique(index_data$CALENDAR_YEAR), decreasing = TRUE),
                         selected = 2023),
              
              h5("Region Selection"),
              pickerInput("comp_region_a", "Region A:",
                         choices = sort(unique(index_data$REGION_NAME)),
                         selected = "Arrow Lakes",
                         options = pickerOptions(liveSearch = TRUE)),
              pickerInput("comp_region_b", "Region B:",
                         choices = sort(unique(index_data$REGION_NAME)),
                         selected = "Burnaby Heights/Capital Hill",
                         options = pickerOptions(liveSearch = TRUE)),
              
              h5("Index Type"),
              selectInput("comp_index", "Index Type:",
                         choices = unique(index_data$INDEX_LABEL),
                         selected = "SEI Index")
            )
          ),
          box(
            title = "Index Comparison", status = "primary", 
            solidHeader = TRUE, width = 8,
            plotlyOutput("comparison_plot", height = "300px"),
            br(),
            DT::dataTableOutput("comparison_table")
          )
        ),
        fluidRow(
          box(
            title = "Factor Contributions Analysis", status = "info", 
            solidHeader = TRUE, width = 12,
            fluidRow(
              column(6, plotlyOutput("contribution_plot_a", height = "400px")),
              column(6, plotlyOutput("contribution_plot_b", height = "400px"))
            )
          )
        )
      ),
      
      # Enhanced Geographic Tab
      tabItem(tabName = "geographic",
        fluidRow(
          box(
            title = "Geographic Filters", status = "primary", solidHeader = TRUE, width = 3,
            div(class = "filter-section",
              h5("Model & Time Settings"),
              radioButtons("geo_model", "Model Type:",
                          choices = unique(index_data$MODEL_TYPE),
                          selected = "Longitudinal"),
              selectInput("geo_year", "Calendar Year:",
                         choices = sort(unique(index_data$CALENDAR_YEAR), decreasing = TRUE),
                         selected = 2023),
              
              h5("Index Selection"),
              selectInput("geo_index", "Index Type:",
                         choices = unique(index_data$INDEX_LABEL),
                         selected = "SEI Index"),
              
              h5("Display Options"),
              checkboxInput("geo_show_labels", "Show Region Labels", value = FALSE),
              sliderInput("geo_circle_size", "Circle Size Multiplier:", 
                         min = 0.5, max = 3, value = 1, step = 0.1)
            )
          ),
          box(
            title = "Index by CHSA: All Regions", status = "primary", 
            solidHeader = TRUE, width = 9,
            leafletOutput("map", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Regional Index Rankings", status = "info", 
            solidHeader = TRUE, width = 12,
            DT::dataTableOutput("geo_table")
          )
        )
      ),
      
      # Enhanced Data Explorer Tab
      tabItem(tabName = "explorer",
        fluidRow(
          box(
            title = "Data Filters", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(2, selectInput("exp_model", "Model Type:",
                                   choices = c("All", unique(index_data$MODEL_TYPE)),
                                   selected = "All")),
              column(3, pickerInput("exp_region", "Region:",
                                   choices = c("All", sort(unique(index_data$REGION_NAME))),
                                   selected = "All",
                                   options = pickerOptions(liveSearch = TRUE))),
              column(2, selectInput("exp_year", "Year:",
                                   choices = c("All", sort(unique(index_data$CALENDAR_YEAR))),
                                   selected = "All")),
              column(3, selectInput("exp_index", "Index Type:",
                                   choices = c("All", unique(index_data$INDEX_LABEL)),
                                   selected = "All")),
              column(2, br(), downloadButton("download_data", "Download CSV", 
                                           class = "btn-primary"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Socio-Economic Index Data", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("explorer_table")
          )
        )
      )
    )
  )
)

# Enhanced Server
server <- function(input, output, session) {
  
  # Time Series subtitle
  output$ts_subtitle <- renderText({
    paste("This chart illustrates the average index values from 2016 to 2023 across six key areas:",
           "Community, Economy, Education, Health, SEI Index, and Total Index.",
           "Each line represents one index, allowing users to explore how these metrics have evolved over time",
           "and compare trends across different domains within the broader socio-economic landscape.")
  })
  
  # Enhanced Time Series Plot
  output$timeseries_plot <- renderPlotly({
    filtered_data <- index_data %>%
      filter(REGION_NAME == input$ts_region,
             MODEL_TYPE == input$ts_model,
             INDEX_LABEL %in% input$ts_indices)
    
    # Define colors similar to PowerBI
    colors <- c("Community" = "#1f77b4", "Economy" = "#ff7f0e", "Education" = "#2ca02c", 
                "Health" = "#d62728", "SEI Index" = "#9467bd", "Total Index" = "#8c564b")
    
    p <- ggplot(filtered_data, aes(x = CALENDAR_YEAR, y = TOTAL, color = INDEX_LABEL)) +
      geom_line(size = 1.5, alpha = 0.8) +
      geom_point(size = 3, alpha = 0.9) +
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = unique(filtered_data$CALENDAR_YEAR)) +
      labs(title = paste("Evolution of Key Index Values Over Time in", input$ts_region),
           subtitle = paste("Model:", input$ts_model),
           x = "Year", y = "Average Index Value", color = "INDEX LABEL") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.1))
  })
  
  # Enhanced Regional Comparison Plot
  output$comparison_plot <- renderPlotly({
    filtered_data <- index_data %>%
      filter(MODEL_TYPE == input$comp_model,
             CALENDAR_YEAR == input$comp_year,
             REGION_NAME %in% c(input$comp_region_a, input$comp_region_b),
             INDEX_LABEL == input$comp_index)
    
    p <- ggplot(filtered_data, aes(x = REGION_NAME, y = TOTAL, fill = REGION_NAME)) +
      geom_col(width = 0.6, alpha = 0.8) +
      geom_text(aes(label = round(TOTAL, 1)), vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_brewer(type = "qual", palette = "Set2") +
      labs(title = paste("Index type:", input$comp_index, "index for CHSA:", 
                        paste(c(input$comp_region_a, input$comp_region_b), collapse = " and "), 
                        "in", input$comp_year, ". Model:", input$comp_model),
           x = "", y = "Index Value") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Comparison table
  output$comparison_table <- DT::renderDataTable({
    filtered_data <- index_data %>%
      filter(MODEL_TYPE == input$comp_model,
             CALENDAR_YEAR == input$comp_year,
             REGION_NAME %in% c(input$comp_region_a, input$comp_region_b),
             INDEX_LABEL == input$comp_index) %>%
      select(REGION_NAME, INDEX_LABEL, TOTAL, POPULATION_ESTIMATE) %>%
      mutate(CONTRIBUTION = round(TOTAL, 2))
    
    DT::datatable(filtered_data, 
                  options = list(pageLength = 5, dom = 't'),
                  colnames = c("Region", "Index", "Value", "Population", "Contribution")) %>%
      formatRound("TOTAL", 2) %>%
      formatCurrency("POPULATION_ESTIMATE", currency = "", digits = 0)
  })
  
  # Factor Contribution Plots
  output$contribution_plot_a <- renderPlotly({
    create_contribution_plot(input$comp_region_a, input$comp_model, input$comp_year, input$comp_index)
  })
  
  output$contribution_plot_b <- renderPlotly({
    create_contribution_plot(input$comp_region_b, input$comp_model, input$comp_year, input$comp_index)
  })
  
  # Helper function for contribution plots
  create_contribution_plot <- function(region, model, year, index) {
    filtered_contrib <- contribution_data %>%
      filter(MODEL_TYPE == model,
             CALENDAR_YEAR == year,
             REGION_NAME == region,
             INDEX_LABEL == index,
             FACTOR != "BENCHMARK") %>%
      arrange(desc(abs(CONTRIBUTION_VALUE))) %>%
      head(8)
    
    p <- ggplot(filtered_contrib, aes(x = reorder(str_wrap(FACTOR_LABEL, 30), CONTRIBUTION_VALUE), 
                                     y = CONTRIBUTION_VALUE,
                                     fill = ifelse(CONTRIBUTION_VALUE > 0, "Increase", "Decrease"))) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      scale_fill_manual(values = c("Increase" = "#2E8B57", "Decrease" = "#DC143C")) +
      labs(title = paste("Index type:", index, "index for CHSA:", region, "in", year, ". Model:", model),
           x = "FACTOR_LABEL", y = "Average of CONTRIBUTION", fill = "") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 10))
    
    ggplotly(p, tooltip = c("x", "y"))
  }
  
  # Enhanced Map
  output$map <- renderLeaflet({
    # Create sample coordinates for BC regions
    sample_coords <- data.frame(
      REGION_NAME = unique(index_data$REGION_NAME),
      lat = runif(length(unique(index_data$REGION_NAME)), 49, 60),
      lng = runif(length(unique(index_data$REGION_NAME)), -139, -114)
    )
    
    filtered_data <- index_data %>%
      filter(MODEL_TYPE == input$geo_model,
             CALENDAR_YEAR == input$geo_year,
             INDEX_LABEL == input$geo_index) %>%
      left_join(sample_coords, by = "REGION_NAME")
    
    # Create color palette
    pal <- colorNumeric("RdYlBu", filtered_data$TOTAL, reverse = TRUE)
    
    map <- leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~sqrt(TOTAL) * input$geo_circle_size,
        popup = ~paste("<b>", REGION_NAME, "</b><br>",
                      "Index Value:", round(TOTAL, 2), "<br>",
                      "Population:", scales::comma(POPULATION_ESTIMATE), "<br>",
                      "Average Age:", round(AVERAGE_AGE, 1)),
        color = ~pal(TOTAL),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1
      ) %>%
      addLegend("bottomright", 
                pal = pal,
                values = ~TOTAL,
                title = paste(input$geo_index, "Value"),
                opacity = 1)
    
    if(input$geo_show_labels) {
      map <- map %>%
        addLabelOnlyMarkers(
          lng = ~lng, lat = ~lat,
          label = ~REGION_NAME,
          labelOptions = labelOptions(noHide = TRUE, textsize = "10px")
        )
    }
    
    map
  })
  
  # Enhanced Geographic Table
  output$geo_table <- DT::renderDataTable({
    filtered_data <- index_data %>%
      filter(MODEL_TYPE == input$geo_model,
             CALENDAR_YEAR == input$geo_year,
             INDEX_LABEL == input$geo_index) %>%
      select(REGION_NAME, INDEX_LABEL, TOTAL, POPULATION_ESTIMATE, AVERAGE_AGE) %>%
      arrange(desc(TOTAL)) %>%
      mutate(Rank = row_number())
    
    DT::datatable(filtered_data, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  colnames = c("Region", "Index", "Value", "Population", "Avg Age", "Rank")) %>%
      formatRound(c("TOTAL", "AVERAGE_AGE"), 2) %>%
      formatCurrency("POPULATION_ESTIMATE", currency = "", digits = 0) %>%
      formatStyle("Rank", backgroundColor = styleInterval(c(5, 10), c("#e8f5e8", "#fff2cc", "#ffcccc")))
  })
  
  # Enhanced Explorer Table
  output$explorer_table <- DT::renderDataTable({
    filtered_data <- index_data
    
    if(input$exp_model != "All") {
      filtered_data <- filtered_data %>% filter(MODEL_TYPE == input$exp_model)
    }
    if(input$exp_region != "All") {
      filtered_data <- filtered_data %>% filter(REGION_NAME == input$exp_region)
    }
    if(input$exp_year != "All") {
      filtered_data <- filtered_data %>% filter(CALENDAR_YEAR == as.numeric(input$exp_year))
    }
    if(input$exp_index != "All") {
      filtered_data <- filtered_data %>% filter(INDEX_LABEL == input$exp_index)
    }
    
    filtered_data <- filtered_data %>%
      select(UID, REGION_NAME, CALENDAR_YEAR, INDEX_LABEL, TOTAL, 
             POPULATION_ESTIMATE, AVERAGE_AGE, MODEL_TYPE, REGION_LEVEL) %>%
      arrange(desc(TOTAL))
    
    DT::datatable(filtered_data, 
                  options = list(pageLength = 20, scrollX = TRUE, dom = 'Bfrtip'),
                  filter = "top",
                  extensions = 'Buttons') %>%
      formatRound(c("TOTAL", "AVERAGE_AGE"), 2) %>%
      formatCurrency("POPULATION_ESTIMATE", currency = "", digits = 0)
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("ses_index_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      filtered_data <- index_data
      
      if(input$exp_model != "All") {
        filtered_data <- filtered_data %>% filter(MODEL_TYPE == input$exp_model)
      }
      if(input$exp_region != "All") {
        filtered_data <- filtered_data %>% filter(REGION_NAME == input$exp_region)
      }
      if(input$exp_year != "All") {
        filtered_data <- filtered_data %>% filter(CALENDAR_YEAR == as.numeric(input$exp_year))
      }
      if(input$exp_index != "All") {
        filtered_data <- filtered_data %>% filter(INDEX_LABEL == input$exp_index)
      }
      
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)