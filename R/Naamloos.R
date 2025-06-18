library(StatsBombR)
library(openxlsx)

# Credentials (both accounts)
username1 <- "Cem@fc32.com"
password1 <- "FxtA72Ux"

username2 <- "JACK71299@HOTMAIL.CO.UK"
password2 <- "J7rB7aP2"

# Load match IDs from your Excel file
match_df <- read.xlsx("match_ids_all.xlsx")

# Load match info with team names (for creating "Match" column)
matches_info <- read.xlsx("all_matches_two_accounts.xlsx")

# Create a lookup table: match_id -> "home_team - away_team"
matches_info$Match <- paste0(matches_info$home_team.home_team_name, " - ", matches_info$away_team.away_team_name)
match_lookup <- setNames(matches_info$Match, matches_info$match_id)

# Helper to safely fetch events for one match ID
safe_event_fetch <- function(username, password, match_id) {
  tryCatch({
    df <- allevents(username, password, match_id)
    df$match_id <- match_id  # add for reference
    return(df)
  }, error = function(e) {
    message(paste("Failed match ID:", match_id, "user:", username))
    return(NULL)
  })
}

# Function to align columns of all data.frames in a list
align_columns <- function(df_list) {
  all_cols <- unique(unlist(lapply(df_list, colnames)))
  
  df_list_aligned <- lapply(df_list, function(df) {
    missing_cols <- setdiff(all_cols, colnames(df))
    if(length(missing_cols) > 0) {
      df[missing_cols] <- NA  # add missing columns as NA
    }
    # reorder columns to the same order
    df <- df[all_cols]
    return(df)
  })
  
  return(df_list_aligned)
}

# Get unique match IDs, but only first 5
match_ids <- unique(match_df$match_id)[1:5]

# Initialize empty list to collect filtered data frames
filtered_events_list <- list()

# Loop through first 5 match IDs and collect filtered events
for (i in seq_along(match_ids)) {
  match_id <- match_ids[i]
  cat("Fetching events for match ID:", match_id, "\n")
  
  # Try first account
  ev1 <- safe_event_fetch(username1, password1, match_id)
  
  # If fail or empty, try second
  if (is.null(ev1) || nrow(ev1) == 0) {
    ev2 <- safe_event_fetch(username2, password2, match_id)
    ev <- ev2
  } else {
    ev <- ev1
  }
  
  if (!is.null(ev) && nrow(ev) > 0) {
    
    # Apply filters:
    ev_filtered <- subset(ev, shot.outcome.name == "Goal" &
                            play_pattern.name %in% c("From Corner", "From Throw In", "From Free Kick"))
    
    if (nrow(ev_filtered) > 0) {
      # Add the "Match" column using lookup (match_id -> "home - away")
      ev_filtered$Match <- match_lookup[as.character(match_id)]
      
      filtered_events_list[[length(filtered_events_list) + 1]] <- ev_filtered
      cat("✓ Found", nrow(ev_filtered), "filtered goals for match ID", match_id, "\n")
    } else {
      cat("✗ No matching goals for match ID", match_id, "\n")
    }
  } else {
    cat("✗ No events found for match ID", match_id, "\n")
  }
}

# Combine all filtered events into one data frame after aligning columns
if (length(filtered_events_list) > 0) {
  filtered_events_list <- align_columns(filtered_events_list)
  all_filtered_events <- do.call(rbind, filtered_events_list)
  
  # Save all filtered events into one Excel file
  write.xlsx(all_filtered_events, "filtered_goals_first5_matches.xlsx", na.string = "NA")
  cat("✓ Saved all filtered goals to filtered_goals_first5_matches.xlsx\n")
} else {
  cat("✗ No filtered events found for any of the first 5 matches.\n")
}

cat("✅ All done.\n")
