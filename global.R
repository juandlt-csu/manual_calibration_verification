# global.R
library(shiny)
library(ross.wq.tools)
library(DT)

#Installing and loading all packages
invisible(
  lapply(c(
    "tidyverse", # Data manipulation
    "janitor", # Clean dirty data
    "lubridate", # Date-Time Manipulation
    "rvest", # HTML Retrieval and Manipulation
    "readxl", # Reading excel files
    "here", # Easy, cross platform file referencing
    "ggplot2", # Plotting libraries
    "ggpubr",
    "plotly",
    "devtools", # For downloading GitHub packages
    "remotes",
    "yaml",
    "arrow", # reading parquet files
    "groupdata2"
  ),
  function(x) {
    if (x %in% installed.packages()) {
      suppressMessages({
        library(x, character.only = TRUE)
      })
    } else {
      suppressMessages({
        install.packages(x)
        library(x, character.only = TRUE)
      })
    }
  })
)

# Load data once when app starts

# Define file paths for tracking system
data_dir <- here::here("data", "raw", "sensor", "manual_data_verification", "complete_dataset")
tracking_file <- file.path(data_dir, "calibrated_sensor_field_data_tracking.rds")
finalized_file <- file.path(data_dir, "calibrated_sensor_field_data_finalized.rds")
original_file <- here::here("data", "raw", "sensor", "manual_data_verification", 
                            "complete_dataset", "calibrated_sensor_field_data.rds")

# Initialize tracking data (unverified calibrations)
if (file.exists(tracking_file)) {
  calibrated_data_tracking <- readr::read_rds(tracking_file)
} else {
  # First time running - copy original to tracking
  calibrated_data_tracking <- readr::read_rds(original_file)
  readr::write_rds(calibrated_data_tracking, tracking_file)
}

# Initialize finalized data (verified calibrations)
if (file.exists(finalized_file)) {
  final_calibrated_data <- readr::read_rds(finalized_file)
} else {
  # First time running - create empty nested list structure matching tracking data
  final_calibrated_data <- lapply(calibrated_data_tracking, function(year_data) {
    list()  # Empty list for each year
  })
  readr::write_rds(final_calibrated_data, finalized_file)
}

# Read in the sensor specific calibration data
sensor_calibration_data <- readr::read_rds(here::here("data", "collated", "sensor", "cal_reports",
                                                      "sensor_calibration_data.RDS"))

ross.wq.tools::load_calibration_data(
  cal_data_file_path = here::here("data", "collated", "sensor", "cal_reports",
                                  "munged_calibration_data.RDS")
)

# Source helper functions
source(here::here("R", "generate_final_df.R"))

