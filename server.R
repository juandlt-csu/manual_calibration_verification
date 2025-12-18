# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Set up reactive values
  # At the beginning of your server function
  values <- reactiveValues(
    selected_sensor_row = NULL,
    selected_sensor_data = NULL,
    selected_sensor_key = NULL,
    preview_final_df = NULL  # Store preview results
  )
  
  # Update year choice
  site_param_options <- reactive({
    req(input$year_choice)
    calibrated_data_tracking[[input$year_choice]]
  })
  
  sensor_options <- reactive({
    req(input$year_choice)
    sensor_calibration_data[[input$year_choice]]
  })
  
  # Update site param choice when year choice changes
  observeEvent(input$year_choice, {
    updateSelectInput(session, "site_param_choice",
                      choices = names(site_param_options()),
                      selected = names(site_param_options())[1])
    # Clear preview when changing year
    values$preview_final_df <- NULL
  })
  
  # Clear preview when site param changes
  observeEvent(input$site_param_choice, {
    values$preview_final_df <- NULL
  })
  
  # Update calibration plot df selected when site param choice updates
  cal_plot_df <- reactive({
    req(input$site_param_choice)
    calibrated_data_tracking[[input$year_choice]][[input$site_param_choice]] 
  })
  
  site_calibration_data_df <- reactive({
    req(cal_plot_df())
    cal_plot_df() %>% 
      group_by(sensor_date) %>% 
      slice_min(DT_round, n = 1) %>%
      arrange(DT_round)
  })
  
  sensor_calibration_data_df <- reactive({
    req(values$selected_sensor_key)
    
    sensor_calibration_data[[input$year_choice]][[values$selected_sensor_key]]
  })
  
  field_notes_data_df <- reactive({
    req(cal_plot_df())
    cal_plot_df() %>%
      filter(
        !is.na(visit_comments) |
          !is.na(cals_performed) |
          !is.na(mal_flag)
      ) %>%
      select(DT_round, last_site_visit, visit_comments, sensor_malfunction, cals_performed,
             mal_flag, sonde_moved, sonde_employed) %>%
      arrange(DT_round)
  })
  
  output$cal_plot <- renderPlotly({
    
    # Use preview data if available, otherwise use original data
    if (!is.null(values$preview_final_df)) {
      cal_plot(values$preview_final_df)
    } else {
      cal_plot(cal_plot_df())
    }
    
  })
  
  output$site_calibration_df <- DT::renderDataTable({
    DT::datatable(site_calibration_data_df(), 
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE
                  )
    )
  })
  
  output$sensor_calibration_df <- DT::renderDataTable({
    DT::datatable(sensor_calibration_data_df(), 
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE
                  )
    )
  })
  
  output$field_notes_df <- DT::renderDataTable({
    
    # Debug: check what we're trying to render
    data <- field_notes_data_df()
    print("Field notes data structure:")
    print(str(data))
    print("Field notes data preview:")
    print(head(data))
    
    if(nrow(data) == 0) {
      # Return a simple message if no data
      DT::datatable(
        data.frame(Message = "No field notes available"),
        options = list(dom = 't')
      )
    } else {
      DT::datatable(data, 
                    options = list(
                      pageLength = 10,
                      scrollX = TRUE
                    )
      )
    }
  })
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # DECISION SECTION ====
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Reactive to generate dynamic UI based on datatable
  # In your renderUI
  output$dynamic_controls <- renderUI({
    site_cal_data <- site_calibration_data_df()
    
    if(is.null(site_cal_data) || nrow(site_cal_data) == 0) {
      return(p("No data available"))
    }
    
    ui_elements <- lapply(1:nrow(site_cal_data), function(i) {
      
      row_data <- site_cal_data[i, ]
      
      cal_DT <- as.character(row_data$DT_round)
      
      sensor_calibration_data_choice <- as.character(row_data$sensor_serial)
      
      # Initialize the possible choices with what is currently available for calibrations
      lag_present <- (!is.na(row_data$slope_lag) & !is.na(row_data$offset_lag))
      
      lead_present <- (!is.na(row_data$slope_lead) & !is.na(row_data$offset_lead))

      default_choices <- case_when(
        !lead_present & !lag_present ~ c(NA, "Original", NA),
        !lead_present ~ c("Lag", "Original", NA),
        !lag_present ~ c(NA, "Original", "Lead"),
        .default = c("Lag", "Original", "Lead")
      ) %>% 
        discard(is.na)
      
      # Initialize the choice with what is currently selected
      from_default <- case_when(
        row_data$slope_final == row_data$slope & 
          row_data$offset_final == row_data$offset ~ "Original",
        row_data$slope_final == row_data$slope_lag & 
          row_data$offset_final == row_data$offset_lag ~ "Lag",
        row_data$slope_final == row_data$slope_lead & 
          row_data$offset_final == row_data$offset_lead ~ "Lead",
        .default = "Original"
      )
      
      to_default <- case_when(
        row_data$slope == row_data$slope_lead & 
          row_data$offset == row_data$offset_lead ~ "Original",
        row_data$slope != row_data$slope_lead | 
          row_data$offset != row_data$offset_lead ~ "Lead",
        .default = "Original"
      )
      
      div(
        style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0;",
        h6(paste0("Calibration: ", cal_DT, "; Sensor: ", sensor_calibration_data_choice)),
        
        # Two select inputs side by side for each row
        fluidRow(
          column(width = 6,
                 selectInput(
                   inputId = paste0("from_decision_", i),
                   label = "From:",
                   choices = default_choices,
                   selected = from_default
                 )
          ),
          column(width = 6,
                 selectInput(
                   inputId = paste0("to_decision_", i),
                   label = "To:",
                   choices = default_choices,
                   selected = to_default
                 )
          )
        ),
        
        actionButton(
          inputId = paste0("sensorViewButton_", i), 
          label = "View Sensor Calibration Data", 
          class = "btn-snsr-view"
        ),
      )
    })
    
    do.call(tagList, ui_elements)
  })   
  
  # Create a reactive to collect all the dynamic input values
  dynamic_decisions <- reactive({
    
    req(site_calibration_data_df())
    
    site_cal_data <- site_calibration_data_df()
    
    if(is.null(site_cal_data) || nrow(site_cal_data) == 0) {
      return(NULL)
    }
    
    decisions <- data.frame(
      row = 1:nrow(site_cal_data),
      from = sapply(1:nrow(site_cal_data), function(i) {
        input[[paste0("from_decision_", i)]] %||% "Original"
      }),
      to = sapply(1:nrow(site_cal_data), function(i) {
        input[[paste0("to_decision_", i)]] %||% "Original"
      }),
      stringsAsFactors = FALSE
    )
    
    return(decisions)
  })
  
  # Use the decisions
  observeEvent(input$previewPlot, {
    
    req(site_calibration_data_df())
    req(cal_plot_df())
    
    cal_data <- cal_plot_df()
    site_cal_data <- site_calibration_data_df()
    decisions <- dynamic_decisions()
    
    # Generate and store preview
    values$preview_final_df <- generate_final_df(cal_data, site_cal_data, decisions)
    
    print("All decisions:")
    print(decisions)
    print("Preview data updated - plot will refresh")
  })
  
  observe({
    site_cal_data <- site_calibration_data_df()
    
    if(!is.null(site_cal_data) && nrow(site_cal_data) > 0) {
      lapply(1:nrow(site_cal_data), function(i) {
        button_id <- paste0("sensorViewButton_", i)
        
        observeEvent(input[[button_id]], {
          updateTabsetPanel(session, "main_tabs", selected = "Sensor Calibration Data")
          
          # Store the row and data
          values$selected_sensor_row <- i
          values$selected_sensor_data <- site_cal_data[i, ]
          
          # Pre-calculate the sensor key
          parameter <- site_cal_data[i, ]$parameter  # Adjust column name
          sensor_serial <- site_cal_data[i, ]$sensor_serial  # Adjust column name
          values$selected_sensor_key <- paste0(parameter, "-", sensor_serial)
        })
      })
    }
  })
  
  # Accept decisions and finalize calibration
  observeEvent(input$acceptDecisionButton, {
    req(input$year_choice)
    req(input$site_param_choice)
    
    # Get current selections
    current_year <- input$year_choice
    current_site_param <- input$site_param_choice
    
    # Generate final_df (use preview if available, otherwise generate with current decisions)
    if (!is.null(values$preview_final_df)) {
      final_df <- values$preview_final_df
    } else {
      # Generate with current decisions
      cal_data <- cal_plot_df()
      site_cal_data <- site_calibration_data_df()
      decisions <- dynamic_decisions()
      final_df <- generate_final_df(cal_data, site_cal_data, decisions)
    }
    
    # Add to finalized data
    if (is.null(final_calibrated_data[[current_year]])) {
      final_calibrated_data[[current_year]] <<- list()
    }
    final_calibrated_data[[current_year]][[current_site_param]] <<- final_df
    
    # Remove from tracking data
    calibrated_data_tracking[[current_year]][[current_site_param]] <<- NULL
    
    # Save both files
    readr::write_rds(calibrated_data_tracking, tracking_file)
    readr::write_rds(final_calibrated_data, finalized_file)
    
    # Clear preview
    values$preview_final_df <- NULL
    
    # Update UI choices
    remaining_choices <- names(calibrated_data_tracking[[current_year]])
    
    if (length(remaining_choices) > 0) {
      updateSelectInput(session, "site_param_choice",
                        choices = remaining_choices,
                        selected = remaining_choices[1])
      
      showNotification(
        paste0("Calibration verified and saved for: ", current_site_param, 
               ". Remaining: ", length(remaining_choices)),
        type = "message",
        duration = 5
      )
    } else {
      showNotification(
        paste0("All calibrations verified for year ", current_year, "!"),
        type = "message",
        duration = 10
      )
      
      # Find next year with data
      all_years <- names(calibrated_data_tracking)
      current_year_idx <- which(all_years == current_year)
      
      if (current_year_idx < length(all_years)) {
        next_year <- all_years[current_year_idx + 1]
        updateSelectInput(session, "year_choice", selected = next_year)
      }
    }
  })
  
}
