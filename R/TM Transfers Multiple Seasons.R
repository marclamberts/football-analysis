# Install necessary libraries (run only if not installed)
# install.packages("devtools")
# devtools::install_github("JaseZiv/worldfootballR")
# install.packages("writexl")
# install.packages("dplyr")
# install.packages("purrr")
library(writexl)
library(worldfootballR)
library(dplyr)
library(purrr)

# Base URL for VVV-Venlo
base_url <- "https://www.transfermarkt.com/atalanta-bergamo/startseite/verein/800/saison_id/"

# Create a vector with seasons from 2014 to 2024
seasons <- 2014:2024

# Function to get transfer data, save individual Excel files, and return the data frame
get_and_save_transfers <- function(season) {
  # Generate the team URL for each season
  team_url <- paste0(base_url, season)
  
  # Fetch the transfer data
  transfer_data <- tm_team_transfers(team_url = team_url, transfer_window = "all")
  
  # Create the filename for the season
  file_name <- paste0("atalanta_transfers_", season, ".xlsx")
  
  # Write the data to an individual Excel file
  write_xlsx(transfer_data, path = file_name)
  
  # Return the data frame
  return(transfer_data)
}

# Apply the function to all seasons and combine the data frames into one
combined_data <- map_df(seasons, get_and_save_transfers)

# Write the combined data into one Excel file
write_xlsx(combined_data, path = "atalanta_transfers_combined_2014_2024.xlsx")

# Glimpse of the combined data
dplyr::glimpse(combined_data)
