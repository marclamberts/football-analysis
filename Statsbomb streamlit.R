library(StatsBombR)
library(openxlsx)

# Account credentials
username1 <- "Cem@fc32.com"
password1 <- "FxtA72Ux"

username2 <- "JACK71299@HOTMAIL.CO.UK"
password2 <- "J7rB7aP2"

# Replace with your actual competition and season IDs
competition_ids <- c(8)  # Fill in competition IDs here
season_ids <- c(317)    # Fill in corresponding season IDs here

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
  season_id <- season_ids[1]
  
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

# Create lookup for match names
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

# Extract and filter events
match_ids <- unique(all_matches$match_id)
filtered_events_list <- list()

for (i in seq_along(match_ids)) {
  match_id <- match_ids[i]
  cat("Fetching events for match ID:", match_id, "\n")
  
  ev1 <- safe_event_fetch(username1, password1, match_id)
  ev <- if (is.null(ev1) || nrow(ev1) == 0) safe_event_fetch(username2, password2, match_id) else ev1
  
  if (!is.null(ev) && nrow(ev) > 0) {
    ev_filtered <- subset(ev, shot.outcome.name == "Goal" &
                            play_pattern.name %in% c("From Corner", "From Throw In", "From Free Kick"))
    
    if (nrow(ev_filtered) > 0) {
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

# Save filtered events with added competition and season metadata
if (length(filtered_events_list) > 0) {
  filtered_events_list <- align_columns(filtered_events_list)
  all_filtered_events <- do.call(rbind, filtered_events_list)
  
  # Prepare metadata columns
  meta_df <- all_matches[, c("match_id", 
                             "competition.country_name", 
                             "competition.competition_name", 
                             "season.season_name")]
  
  # Merge metadata into filtered events by match_id
  all_filtered_events <- merge(all_filtered_events, meta_df, by = "match_id", all.x = TRUE)
  
  # Save to Excel
  write.xlsx(all_filtered_events, "test.xlsx", na.string = "NA")
  cat("✓ Saved all filtered goals to filtered_goals_all_matches.xlsx with metadata\n")
} else {
  cat("✗ No filtered events found for any match.\n")
}

cat("✅ All done.\n")
