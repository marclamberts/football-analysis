# Load necessary libraries
library(jsonlite)
library(dplyr)
library(data.table)
library(glue)

# Define folder paths
csv_folder <- "~/Python/BM/xgCSV/"   # Folder containing CSV files
json_folder <- "~/Python/BM/"        # Folder containing JSON files
output_folder <- "~/Python/BM/mergedCSV/"  # Folder to save merged CSV files

# Get list of files
csv_files <- list.files(csv_folder, pattern = "\\.csv$", full.names = TRUE)
json_files <- list.files(json_folder, pattern = "\\.json$", full.names = TRUE)

# Loop through each CSV and JSON pair
for (csv_file in csv_files) {
  
  # Extract match identifier from CSV filename (adjust the regex if needed)
  match_id <- gsub("\\.csv$", "", basename(csv_file))
  
  # Find corresponding JSON file
  json_file <- json_files[grepl(match_id, json_files)]
  
  # Skip if no matching JSON file found
  if (length(json_file) == 0) {
    print(glue("No matching JSON file found for: {csv_file}"))
    next
  }
  
  print(glue("Processing: {basename(csv_file)} with {basename(json_file)}"))
  
  # Read the CSV file
  csv_data <- read.csv(csv_file)
  
  # Read the JSON file and flatten it into a data frame
  json_data <- fromJSON(json_file, flatten = TRUE)
  json_events <- as.data.frame(json_data$event)
  
  # Filter JSON data to identify shots on target (typeId 15 or 16)
  json_events <- json_events %>%
    mutate(Shot_on_Target = ifelse(typeId %in% c(15, 16), TRUE, FALSE)) %>%
    select(x, y, Shot_on_Target) %>%
    distinct()  # Ensure unique combinations of x, y, and Shot_on_Target
  
  # Merge the CSV file with the JSON data using only x and y
  merged_data <- csv_data %>%
    left_join(json_events, by = c("x", "y"))
  
  # Replace NA values in "Shot_on_Target" with FALSE (if no match is found)
  merged_data$Shot_on_Target[is.na(merged_data$Shot_on_Target)] <- FALSE
  
  # Save the updated merged data to a new CSV file
  output_file <- glue("{output_folder}{match_id}_merged.csv")
  write.csv(merged_data, output_file, row.names = FALSE)
  
  print(glue("Saved merged data to: {output_file}"))
}

print("All files processed.")
