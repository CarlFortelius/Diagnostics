#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Load necessary libraries
library(shiny)
library(here)
library("harp")
library(tidyverse)
library(purrr)
library(zoo)
library(scales)      # Needed for function pretty_breaks()


source(here("R/fn_select_parameter.R"))
source(here("R/fn_return_plot_of_data_at_mast.R"))

rds_path <- "/perm/fnm/Site_data/"

# Define the user interface
ui <- fluidPage(
  #titlePanel("RDS File Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("rds_path", "Give a directory:", value = "/perm/fnm/Site_data/"),
      uiOutput("fileSelectorUI"),  # Placeholder for selectInput
      #selectInput("rds_file", "Choose the period", choices = list.files(path = rds_path, 
                                                  #pattern = "\\.rds$", full.names = FALSE)),
      actionButton("load_data", "Load Data"),
      uiOutput("site_ui"),
      uiOutput("parameter_ui"),
      selectInput("plot_type", "Plot Type", choices = c("valid_dttm", "valid_hour", "hexbin", 
                                                        "histogram", "cumhisto", "A_vs_B", 
                                                        "compare_params")),
      selectInput("show_zero", "Show Zero", choices = c("Neither", "Both", "X", "Y")),
      uiOutput("parameter2_ui")
    ),
    
    mainPanel(
      plotOutput("dataPlot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  indata <- reactiveVal(NULL)
  
  # Reactive expression to get list of files in the directory
  filesInDir <- reactive({
    rds_path <- input$rds_path
    if (dir.exists(rds_path)) {
      list.files(path = rds_path, pattern = "\\.rds$", full.names = FALSE)
    } else {
      character(0)  # Return empty character vector if directory does not exist
    }
  })
  
  # Dynamically create selectInput based on the files in the directory
  output$fileSelectorUI <- renderUI({
    selectInput("rds_file", "Select a file:", choices = filesInDir())
  })
  
  observeEvent(input$load_data, {
    req(input$rds_file)
    
    # Read the selected RDS file
    indata(readRDS(paste(input$rds_path,input$rds_file,sep="/")))
    
    # Update UI elements for site and parameter selection
    output$site_ui <- renderUI({
      req(indata())
      selectInput("site", "Choose Site", choices = names(indata()$metadata$Sites))
    })
    
    output$parameter_ui <- renderUI({
      req(indata())
      selectInput("parameter", "Choose Parameter", choices = names( indata()$params) )
    })
    output$parameter2_ui <- renderUI({
      req(indata())
      selectInput("parameter2", paste0("Choose Reference Parameter \n
                                       (for A_vs_B and compare_params)"), 
                                       choices = names( indata()$params) )
    })
  })
  
  output$dataPlot <- renderPlot({
    req(indata(), input$site, input$parameter, input$plot_type)
    
    
    data       <- indata()
    data$all_obs <- data$all_obs |> 
      mutate(MOMF = if_else(SID == 3, -MOMF, MOMF)) #change the sign of MOMF for Lindenberg

    site       <- input$site
    parameter  <- input$parameter
    parameter2 <- input$parameter2
    plot_type  <- input$plot_type
    show_zero  <- input$show_zero
    
    object <- fn_select_parameter(data$all_obs, 
                                  data$all_mods, 
                                  parameter, 
                                  data$params[[{{parameter}}]]$units,
                                  SID=data$metadata$Sites[[{{site}}]]$SID)
    if(!is.null(object)) {
      object <- check_obs_against_fcst(object, 
                                      {{parameter}},
                                      num_sd_allowed = 6,
                                      stratification=c("quarter_day"))|>
        expand_date(valid_dttm)
      
      tester <- do.call(rbind, 
                        lapply(object, function(model){
                          model |>
                            select( valid_dttm, 
                                    fcst, 
                                    all_of({{parameter}}), 
                                    fcst_model, SID)
                        })) |>
        filter(SID==data$metadata$Sites[[{{site}}]]$SID)
    }
    if (length( tester[[{{parameter}}]]) == 0) {return()}
    
    if (!is.null(parameter2)){
      object2 <- fn_select_parameter(data$all_obs, 
                                    data$all_mods, 
                                    parameter2, 
                                    data$params[[{{parameter2}}]]$units,
                                    SID=data$metadata$Sites[[{{site}}]]$SID)
      if(!is.null(object2)) {
        object2 <- as_harp_list(
          lapply(
            object2,
            check_obs_against_fcst, 
            {{parameter2}},
            num_sd_allowed = 6)
        ) |>
          expand_date(valid_dttm)
        
        tester <- do.call(rbind, 
                          lapply(object2, function(model){
                            model |>
                              select( valid_dttm, 
                                      fcst, 
                                      all_of({{parameter2}}), 
                                      fcst_model, SID)
                          })) |>
          filter(SID==data$metadata$Sites[[{{site}}]]$SID)
      }
    }
    #if (length( tester[[{{parameter}}]]) == 0) {return()}

      fn_return_plot_of_data_at_mast(
        plot_type           = plot_type, #plot_type, # possible choices: valid_dttm, 
        fc_object           = object, #a harp forecast object (list of data frames)
        secondary_object    = object2, # used for certain plot_types ("compare_params")
        #plotvar             = parameter, #string, name of parameter to be plotted
        #secondary_var       = "SLH",
        SID                 = data$metadata$Sites[[{{site}}]]$SID, # station identifier (1=SODA, 2=CABA, 3=LIND)
        site                = site, # name of station
        #valid_hour, lead_time, histo, cumhisto, scatter
        rolling_mean_window = 3*24, #integer, assumed hourly
        show_zero           = show_zero)
      
  }
  )
}
# Run the application
shinyApp(ui = ui, server = server)
