# Helper function to generate final_df with updated calibrations
# Suppress R CMD check notes for NSE used in tidyverse functions
# These warnings are expected and not actual errors
generate_final_df <- function(cal_data, site_cal_data, decisions) {
  browser()
  parameter <- unique(cal_data$parameter)
  
  updated_calibrations <- site_cal_data %>% 
    bind_cols(decisions) %>% 
    mutate(
      updated_slope_from = case_when(
        from == "Original" ~ slope,
        from == "Lag" ~ slope_lag,
        from == "Lead" ~ slope_lead,
        .default = slope_final
      ),
      updated_offset_from = case_when(
        from == "Original" ~ offset,
        from == "Lag" ~ offset_lag,
        from == "Lead" ~ offset_lead,
        .default = offset_final
      ),
      updated_slope_to = case_when(
        to == "Original" ~ slope,
        to == "Lag" ~ slope_lag,
        to == "Lead" ~ slope_lead,
        .default = slope_lead
      ),
      updated_offset_to = case_when(
        to == "Original" ~ offset,
        to == "Lag" ~ offset_lag,
        to == "Lead" ~ offset_lead,
        .default = offset_lead
      )
    ) %>% 
    select(DT_round, 
           updated_slope_from, updated_slope_to,
           updated_offset_from, updated_offset_to)
  
  # Join decisions to cal_plot_df()
  updated_cal_plot_df <- cal_data %>% 
    dplyr::left_join(updated_calibrations, by = "DT_round") %>% 
    # Forward fill with updated decisions %>% 
    tidyr::fill(updated_slope_from, updated_slope_to,
                updated_offset_from, updated_offset_to, 
                .direction = "down")
  
  # Back calibrate with updated decisions
  if (parameter %in% c("Chl-a Fluorescence", "FDOM Fluorescence", "ORP", 
                       "Pressure", "Specific Conductivity", "DO", "Turbidity")) {
    updated_cal_plot_df <- updated_cal_plot_df %>%
      cal_lin_trans_lm(
        df = ., 
        raw_col = "mean_cleaned_raw",
        slope_from_col = "updated_slope_from", offset_from_col = "updated_offset_from",
        slope_to_col = "updated_slope_to", offset_to_col = "updated_offset_to",
        wt_col = "wt"
      )
  }
  
  if (parameter == "pH"){
    updated_cal_plot_df <- updated_cal_plot_df %>%
      cal_lin_trans_inv_lm_pH(
        df = ., 
        mv_col = "mean_cleaned_raw",
        slope_from_col = "updated_slope_from", offset_from_col = "updated_offset_from",
        slope_to_col = "updated_slope_to", offset_to_col = "updated_offset_to",
        wt_col = "wt"
      ) 
  }
  
  # Validate calibration results and create final calibrated values 
  checked_df <- updated_cal_plot_df  %>%
    cal_check(df = ., obs_col = "mean_cleaned", lm_trans_col = "mean_lm_trans")
  
  # Reorder the final columns 
  final_df <- checked_df %>%
    dplyr::select(
      # DT sensor reading columns
      DT_round,
      # Field ID columns
      site, sonde_serial, parameter,
      # Sensor reading transformation columns
      mean_cleaned, mean_cleaned_raw, mean_lm_trans, mean_cleaned_cal, cal_check,
      # Sensor information
      sensor_serial,
      # DT calibration information columns
      file_date, sonde_date, sensor_date_lag, sensor_date, sensor_date_lead,
      # Calibration information columns
      correct_calibration, slope_lag, offset_lag, slope, offset, slope_final, offset_final, slope_lead, offset_lead,
      updated_slope_from, updated_offset_from, updated_slope_to, updated_offset_to
      # Remove everything else
    )
  
  return(final_df)
}
