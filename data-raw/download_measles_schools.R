#!/usr/bin/env Rscript
#
# Download and process measles school data from the TACC measles dashboard
# Data source: https://github.com/TACC/measles-dashboard
#
# This script downloads individual state CSV files, combines them, and extracts
# the required columns for the epiworldRShiny school selector feature.

library(utils)

# Base URL for the raw data files
base_url <- "https://raw.githubusercontent.com/TACC/measles-dashboard/main/state_data/"

# List of state files to download (all US states)
states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
  "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
  "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
  "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
  "New_Hampshire", "New_Jersey", "New_Mexico", "New_York",
  "North_Carolina", "North_Dakota", "Ohio", "Oklahoma", "Oregon",
  "Pennsylvania", "Rhode_Island", "South_Carolina", "South_Dakota",
  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
  "West_Virginia", "Wisconsin", "Wyoming"
)

# Initialize list to store data frames
all_data <- list()

cat("Downloading measles school data from TACC repository...\n")

# Download and read each state file
for (state in states) {
  state_file <- paste0(state, ".csv")
  url <- paste0(base_url, state_file)
  
  cat("  Processing:", state, "... ")
  
  tryCatch({
    # Download the file
    data <- read.csv(url, stringsAsFactors = FALSE)
    
    # Check if required columns exist
    if (all(c("name", "county", "enroll", "mmr") %in% colnames(data))) {
      # Extract and rename columns
      # name -> school_name
      # county -> county
      # enroll -> num_students
      # mmr -> vaccination_rate (need to convert from percentage to decimal)
      
      state_data <- data.frame(
        state = gsub("_", " ", state),  # Replace underscores with spaces
        county = data$county,
        school_name = data$name,
        school_id = paste0(
          toupper(substr(gsub("_", "", state), 1, 2)),
          "-",
          sprintf("%05d", seq_len(nrow(data)))
        ),
        vaccination_rate = ifelse(
          !is.na(data$mmr) & data$mmr >= 0 & data$mmr <= 100,
          data$mmr / 100,  # Convert percentage to decimal
          NA
        ),
        num_students = data$enroll,
        stringsAsFactors = FALSE
      )
      
      # Remove rows with missing critical data
      state_data <- state_data[
        !is.na(state_data$vaccination_rate) & 
        !is.na(state_data$num_students) &
        state_data$num_students > 0,
      ]
      
      all_data[[state]] <- state_data
      cat("OK (", nrow(state_data), "schools)\n", sep = "")
    } else {
      cat("SKIP (missing columns)\n")
    }
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  })
}

# Combine all state data
cat("\nCombining data from all states...\n")
combined_data <- do.call(rbind, all_data)
rownames(combined_data) <- NULL

cat("Total schools:", nrow(combined_data), "\n")

# Save to CSV
output_file <- "../inst/data/schools_measles.csv"
cat("Saving to:", output_file, "\n")
write.csv(combined_data, output_file, row.names = FALSE)

cat("Done!\n")
cat("\nData summary:\n")
cat("  States:", length(unique(combined_data$state)), "\n")
cat("  Counties:", length(unique(combined_data$county)), "\n")
cat("  Schools:", nrow(combined_data), "\n")
cat("  Vaccination rate range:", 
    sprintf("%.1f%% - %.1f%%", 
            min(combined_data$vaccination_rate, na.rm = TRUE) * 100,
            max(combined_data$vaccination_rate, na.rm = TRUE) * 100), "\n")
cat("  School size range:", 
    min(combined_data$num_students, na.rm = TRUE), "-",
    max(combined_data$num_students, na.rm = TRUE), "students\n")
