# Function to make example plots
cal_plot <- function(df) {
  parameter <- unique(df$parameter)
  site <- unique(df$site)
  year <- lubridate::year(df$DT_round)[1]
  
  # Combine all data into one data frame with descriptive categories
  combined_data <- bind_rows(
    # Original data
    df %>% 
      select(DT_round, value = mean_cleaned, correct_calibration) %>%
      mutate(data_type = ifelse(correct_calibration, 
                                "Original Data (Good Calibration)", 
                                "Original Data (Bad Calibration)")),
    # Calibrated data
    df %>%
      select(DT_round, value = mean_cleaned_cal) %>%
      mutate(data_type = "Calibrated Data")
  )
  
  # Create vline dataframe
  vline_df <- df %>% 
    group_by(sensor_date) %>% 
    slice_min(DT_round, n = 1) %>%
    arrange(DT_round)
  
  # Single plot with one geom_line
  p <- ggplot(combined_data, aes(x = DT_round, y = value, color = data_type)) +
    geom_line(linewidth = 0.3, alpha = 0.8) +
    scale_color_manual(
      values = c(
        "Original Data (Good Calibration)" = "springgreen4", 
        "Original Data (Bad Calibration)" = "tomato",
        "Back-calibrated Data" = "steelblue"
      ),
      name = "Data Type"
    ) +
    geom_vline(xintercept = vline_df$DT_round) +
    labs(
      title = paste(site, parameter, year, "Calibration"),
      x = NULL,
      y = paste(parameter, "(units)")
    ) +
    theme_minimal()
  
  p <- ggplotly(p) %>%
    layout(legend = list(
      orientation = "h",    # horizontal
      x = 0.5,             # center horizontally
      xanchor = 'center',  # anchor at center
      y = -0.1             # position below plot
    ))
  
  return(p)
}