# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Manual Calibration Verification"),
  
  # Use fluidRow and column instead of sidebarLayout for better control
  fluidRow(
    # Main content area (left side, full width minus sidebar)
    column(
      width = 9,
      
      # Plot output
      plotlyOutput("cal_plot", height = "500px"),
      
      br(),
      
      # Information tables output
      tabsetPanel(
        id = "main_tabs",  
        tabPanel("Site Calibration Data", 
                 DT::dataTableOutput("site_calibration_df")),
        tabPanel("Sensor Calibration Data", 
                 DT::dataTableOutput("sensor_calibration_df")),
        tabPanel("Field Note Information",
                 DT::dataTableOutput("field_notes_df"))  # Uncomment and give it a unique output ID
      )
    ),
    
    # Sidebar on the right
    column(
      width = 3,
      
      # Options for exploring calibration decisions
      # These options are dynamic and are established in `server.R`
      wellPanel(
        
        h4("Options Panel"),
        
        div(style = "max-height: 500px; overflow-y: auto; overflow-x: hidden; padding-right: 10px;",
            uiOutput("dynamic_controls")
        ),
        
        br(),
        
        actionButton("previewPlot", "Preview Plot")
      ),
      
      fluidRow(
        # Left column - Action buttons
        column(
          6,
          wellPanel(
            actionButton("acceptOriginal", "Accept Original Data"), br(),
            actionButton("acceptRecalibrated", "Accept Re-calibrated Data"), br(),
            actionButton("acceptUpdates", "Accept Updated Calibration Decisions")
          )
        ),
        
        # Right column - Select inputs
        column(
          6,
          selectInput(
            inputId = "year_choice",
            label = "Year Choice",
            choices = names(calibrated_data_tracking),
            selected = NULL,
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          
          selectInput(
            inputId = "site_param_choice",
            label = "Site Parameter Choice",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          )
        )
      )
    )
  )
)