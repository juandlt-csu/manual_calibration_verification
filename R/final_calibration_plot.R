# Function to make example plots
final_calibration_plot <- function(df) {
  
  parameter <- unique(df$parameter)
  site <- unique(df$site)
  year <- lubridate::year(df$DT_round)[1]
  
  final_data <- df %>%
    select(DT_round, value = mean_cleaned_cal) %>%
    mutate(data_type = "Final Data")
  
  # Create vline dataframe
  vline_df <- df %>% 
    group_by(sensor_date) %>% 
    slice_min(DT_round, n = 1) %>%
    arrange(DT_round)
  
  # Single plot with one geom_line
  p <- ggplot(final_data, aes(x = DT_round, y = value, color = data_type)) +
    geom_line(linewidth = 0.3, alpha = 0.8) +
    scale_color_manual(values = c("Final Data" = "black"), name = "Data Type") +
    geom_vline(xintercept = vline_df$DT_round) +
    labs(
      title = paste(site, parameter, year, "Final Data"),
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