#!/usr/bin/env Rscript
#
# Download and process measles school data from the TACC measles dashboard
# Data source: https://github.com/TACC/measles-dashboard
#
# This script downloads individual state CSV files, combines them, and extracts
# the required columns for the epiworldRShiny school selector feature.

# Base URL for the raw data files
base_url <- "https://github.com/TACC/measles-dashboard/archive/refs/heads/main.zip"

# Download and unzip the data to a temporary directory
temp_zip <- tempfile(fileext = ".zip")

message("Downloading measles school data from TACC repository...\n")
download.file(base_url, temp_zip, mode = "wb")
temp_dir <- tempdir()
unzip(temp_zip, exdir = temp_dir)

# Listing files from the state data
data_dir <- file.path(temp_dir, "measles-dashboard-main", "state_data")
data_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

# US State codes
library(data.table)
data_ <- lapply(data_files, \(f) {
  state_f <- gsub(".+state_data/([^_]+).+\\.csv$", "\\1", f)

  # state,county,school_name,school_id,vaccination_rate,num_students
  fread(f)[, .(
    state = state_f,
    county = County,
    school_name = `School District or Name`,
    school_id = sprintf("%s-%05d", state_f, seq_len(.N)),
    vaccination_rate = `MMR Vaccination Rate` / 100,
    num_students = 500 # Fixing since not provided in source data
    )]
}) |> rbindlist()


message("Total schools:", nrow(data_), "\n")

# Save to CSV
output_file <- "inst/data/schools_measles.csv"
message("Saving to:", output_file, "\n")
fwrite(data_, output_file, row.names = FALSE)

message("Done!\n")
message("\nData summary:\n")
message("  States:", length(unique(data_$state)), "\n")
message("  Counties:", length(unique(data_$county)), "\n")
message("  Schools:", nrow(data_), "\n")
message("  Vaccination rate range:", 
    sprintf("%.1f%% - %.1f%%", 
            min(data_$vaccination_rate, na.rm = TRUE) * 100,
            max(data_$vaccination_rate, na.rm = TRUE) * 100), "\n")
