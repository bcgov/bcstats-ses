

# BC community Socio-economic status index
# app.R

library(shiny)
library(leaflet)
library(sf)

# Define UI
ui <- navbarPage("BC Socio-economic Status (SES) Index",
                 tabPanel("Home",
                          h3("About this App"),
                          p("This Shiny app provides a user-friendly interface to explore the BC SES Index project data. The app has the following sections:"),
                          tags$ul(
                            tags$li(tags$b("Data Cleaning:"), "Run the data cleaning scripts to process the raw data."),
                            tags$li(tags$b("Economic Indicators:"), "Visualize the trends and geographic distribution of various economic indicators.")
                          ),
                          p("For more details on the project, please refer to the README.md file.")
                 ),
                 tabPanel("Data Cleaning",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Workflow Control"),
                              p("Click the button below to start the data cleaning process."),
                              actionButton("run_scripts", "Run Data Cleaning Scripts"),
                              hr(),
                              h4("Process Status"),
                              verbatimTextOutput("status_log")
                            ),
                            mainPanel(
                              h3("Data Cleaning Process"),
                              p("This section allows you to run the data cleaning scripts located in the 'src' directory. The scripts are executed in a predefined order to ensure data integrity."),
                              p("The output and any potential errors from the scripts will be displayed in the 'Process Status' log in the sidebar.")
                            )
                          )
                 ),
                 tabPanel("Economic Indicators",
                          tabsetPanel(
                            tabPanel("Variable Trends",
                                     h3("Economic Variable Trends"),
                                     p("This section will display trends of various economic variables over time."),
                                     # Add inputs for selecting variable and time range
                                     plotOutput("trend_plot")
                            ),
                            tabPanel("Geographic Distribution",
                                     h3("Geographic Distribution of Economic Variables"),
                                     p("This section will display the geographic distribution of selected economic variables on a map."),
                                     # Add inputs for selecting variable
                                     leafletOutput("map")
                            )
                          )
                 )
)

# Define server logic
server <- function(input, output) {

    # Reactive value to store log messages
    log_messages <- reactiveVal("")

    observeEvent(input$run_scripts, {
        log_messages("") # Clear previous logs
        
        # List of scripts to run in order
        scripts_to_run <- c(
            "01_output_statscan_census.R",
            "03_output_crime_rate.R",
            "04_output_TMF.R",
            "05_output_LFS.R",
            "06_output_wildfire.R",
            "07_SLA.R",
            "08_BC_population_estimates.R",
            "09_output_remoteness.R",
            "10_output_housing_value.R",
            "12_output_CHSA_DA_lookup.R",
            "13_BC_DA_population_estimates.r"
        )

        # Function to run a single script and capture output
        run_script <- function(script_name) {
            log_messages(paste(log_messages(), "\n--- Running:", script_name, "---"))
            output <- capture.output({
                tryCatch({
                    source(file.path("src", script_name), local = TRUE)
                    log_messages(paste(log_messages(), "\n", script_name, "completed successfully."))
                }, error = function(e) {
                    log_messages(paste(log_messages(), "\nError in", script_name, ":", e$message))
                })
            }, type = "message")
            log_messages(paste(log_messages(), "\n", paste(output, collapse = "\n")))
        }

        # Run all scripts sequentially
        for (script in scripts_to_run) {
            run_script(script)
        }
        
        log_messages(paste(log_messages(), "\n--- All scripts have been executed. ---"))
    })

    # Render the log messages
    output$status_log <- renderText({
        log_messages()
    })

    # Placeholder for trend plot
    output$trend_plot <- renderPlot({
        plot(1:10, main = "Placeholder Trend Plot")
    })

    # Placeholder for map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = -122.7, lat = 51.5, zoom = 5) %>%
            addMarkers(lng = -123.12, lat = 49.28, popup = "Vancouver")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

