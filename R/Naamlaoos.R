# Load necessary libraries
library(StatsBombR)
library(openxlsx)

# Account credentials
username1 <- "Cem@fc32.com"
password1 <- "FxtA72Ux"

username2 <- "JACK71299@HOTMAIL.CO.UK"
password2 <- "J7rB7aP2"

# Competition and season IDs
competition_ids <- c(4, 5)  # Add more IDs if needed
season_ids <- c(317)     # Match order with competition_ids

# Safe wrapper for fetching matches
safe_get_matches <- function(username, password, season_id, comp_id) {
  tryCatch({
    get.matches(username, password, season_id, comp_id)
  }, error = function(e) {
    message(paste("Failed for comp:", comp_id, "season:", season_id, "user:", username))
    return(NULL)
  })
}

# Store all matches
all_matches_list <- list()

for (i in seq_along(competition_ids)) {
  comp_id <- competition_ids[i]
  season_id <- season_ids[1]  # Adjust if you have multiple season_ids
  
  cat("Trying competition:", comp_id, "season:", season_id, "\n")
  
  matches_df <- safe_get_matches(username1, password1, season_id, comp_id)
  
  if (is.null(matches_df) || nrow(matches_df) == 0) {
    matches_df <- safe_get_matches(username2, password2, season_id, comp_id)
  }
  
  if (!is.null(matches_df) && nrow(matches_df) > 0) {
    matches_df$competition_id <- comp_id
    matches_df$season_id <- season_id
    all_matches_list[[length(all_matches_list) + 1]] <- matches_df
    cat("✓ Fetched matches\n")
  } else {
    cat("✗ No matches found\n")
  }
}

# Combine and save matches
if (length(all_matches_list) > 0) {
  all_matches <- do.call(rbind, all_matches_list)
  write.xlsx(all_matches, file = "all_matches_two_accounts.xlsx", na.string = "NA")
  cat("✅ All matches saved to all_matches_two_accounts.xlsx\n")
} else {
  stop("⚠️ No matches found for any competition + season pair\n")
}

# Create match name lookup
all_matches$Match <- paste0(all_matches$home_team.home_team_name, " - ", all_matches$away_team.away_team_name)
match_lookup <- setNames(all_matches$Match, all_matches$match_id)

# Helper to safely fetch events
safe_event_fetch <- function(username, password, match_id) {
  tryCatch({
    df <- allevents(username, password, match_id)
    df$match_id <- match_id
    return(df)
  }, error = function(e) {
    message(paste("Failed match ID:", match_id, "user:", username))
    return(NULL)
  })
}

# Align columns across data frames
align_columns <- function(df_list) {
  all_cols <- unique(unlist(lapply(df_list, colnames)))
  df_list_aligned <- lapply(df_list, function(df) {
    missing_cols <- setdiff(all_cols, colnames(df))
    if (length(missing_cols) > 0) {
      df[missing_cols] <- NA
    }
    df <- df[all_cols]
    return(df)
  })
  return(df_list_aligned)
}

# Fetch all events without filtering
match_ids <- unique(all_matches$match_id)
all_events_list <- list()

for (i in seq_along(match_ids)) {
  match_id <- match_ids[i]
  cat("Fetching events for match ID:", match_id, "\n")
  
  ev1 <- safe_event_fetch(username1, password1, match_id)
  ev <- if (is.null(ev1) || nrow(ev1) == 0) safe_event_fetch(username2, password2, match_id) else ev1
  
  if (!is.null(ev) && nrow(ev) > 0) {
    ev$Match <- match_lookup[as.character(match_id)]
    all_events_list[[length(all_events_list) + 1]] <- ev
    cat("✓ Added", nrow(ev), "events for match ID", match_id, "\n")
  } else {
    cat("✗ No events found for match ID", match_id, "\n")
  }
}

# Save all events with metadata
if (length(all_events_list) > 0) {
  all_events_list <- align_columns(all_events_list)
  all_events <- do.call(rbind, all_events_list)
  
  # Prepare metadata columns
  meta_df <- all_matches[, c("match_id", 
                             "competition.country_name", 
                             "competition.competition_name", 
                             "season.season_name")]
  
  # Merge metadata
  all_events <- merge(all_events, meta_df, by = "match_id", all.x = TRUE)
  
  # Save to Excel
  write.xlsx(all_events, "all_events_all_matches.xlsx", na.string = "NA")
  cat("✅ All events saved to all_events_all_matches.xlsx\n")
} else {
  cat("✗ No events found for any match.\n")
}

cat("✅ All done.\n")
