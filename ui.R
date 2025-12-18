# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Manual Calibration Verification"),
  
  # Use fluidRow and column instead of sidebarLayout for better control
  fluidRow(
    # Main content area (left side, full width minus sidebar)
    column(width = 9,
           plotlyOutput("cal_plot", height = "500px"),
           
           br(),
           
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
    column(width = 3,
           wellPanel(
             h4("Options Panel"),
             div(style = "max-height: 400px; overflow-y: auto; overflow-x: hidden; padding-right: 10px;",
                 uiOutput("dynamic_controls")
             ),
             br(),
             actionButton("previewPlot", "Preview Plot")
           ),
           wellPanel(
             actionButton("acceptButton", "Accept all original data", class = "btn-accept"),
             actionButton("rejectButton", "Accept all re-calibrated data", class = "btn-reject"),
             actionButton("acceptDecisionButton", "Accept Decisions", class = "btn-decision-accept")
           ),
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