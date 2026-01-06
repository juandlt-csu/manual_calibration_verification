update_backend <- function(df, year, site_param, session) {

  # Add to finalized data
  if (is.null(final_calibrated_data[[year]])) {
    final_calibrated_data[[year]] <<- list()
  }
  final_calibrated_data[[year]][[site_param]] <<- df
  
  # Remove from tracking data
  calibrated_data_tracking[[year]][[site_param]] <<- NULL
  
  # Update UI choices
  remaining_choices <- names(calibrated_data_tracking[[year]])
  
  if (length(remaining_choices) > 0) {
    updateSelectInput(session, "site_param_choice",
                      choices = remaining_choices,
                      selected = remaining_choices[1])
    
    showNotification(
      paste0("Calibration verified and saved for: ", site_param, 
             ". Remaining: ", length(remaining_choices)),
      type = "message",
      duration = 5
    )
    
  } else {
    showNotification(
      paste0("All calibrations verified for year ", year, "!"),
      type = "message",
      duration = 10
    )
    
    # Find next year with data
    all_years <- names(calibrated_data_tracking)
    year_idx <- which(all_years == year)
    
    if (year_idx < length(all_years)) {
      next_year <- all_years[year_idx + 1]
      updateSelectInput(session, "year_choice", selected = next_year)
    }
  }
  
  # Save both files (at the end so the app feels faster)
  readr::write_rds(calibrated_data_tracking, tracking_file)
  readr::write_rds(final_calibrated_data, finalized_file)
  
}