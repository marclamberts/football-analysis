# Load necessary libraries
library(StatsBombR)
library(openxlsx)
library(furrr)
library(future)

# Account credentials
username1 <- "Cem@fc32.com"
password1 <- "FxtA72Ux"

username2 <- "JACK71299@HOTMAIL.CO.UK"
password2 <- "J7rB7aP2"

# Competition and season IDs
competition_ids <- c(1522, 194, 85, 
                     77,
                     124,
                     10,
                     3,
                     6,
                     46,
                     11,
                     42,
                     4,
                     5,
                     13,
                     7,
                     65,
                     2,
                     51,
                     12,
                     80)
season_ids <- c(317)

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

# Create match lookup
all_matches$Match <- paste0(all_matches$home_team.home_team_name, " - ", all_matches$away_team.away_team_name)
match_lookup <- setNames(all_matches$Match, all_matches$match_id)

# Safe event fetch wrapper
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

# Align columns across event dataframes
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

# Setup parallel processing
plan(multisession)

# Parallel-safe fetch wrapper
parallel_safe_fetch <- function(match_id) {
  cat("Fetching events for match ID:", match_id, "\n")
  
  ev1 <- safe_event_fetch(username1, password1, match_id)
  ev <- if (is.null(ev1) || nrow(ev1) == 0) safe_event_fetch(username2, password2, match_id) else ev1
  
  if (!is.null(ev) && nrow(ev) > 0) {
    ev_filtered <- subset(ev, shot.outcome.name == "Goal" &
                            play_pattern.name %in% c("From Corner", "From Throw In", "From Free Kick"))
    
    if (nrow(ev_filtered) > 0) {
      ev_filtered$Match <- match_lookup[as.character(match_id)]
      cat("✓ Found", nrow(ev_filtered), "filtered goals for match ID", match_id, "\n")
      return(ev_filtered)
    }
  }
  
  cat("✗ No matching events for match ID", match_id, "\n")
  return(NULL)
}

# Extract and filter events in parallel
match_ids <- unique(all_matches$match_id)

cat("🚀 Starting parallel event fetching...\n")
filtered_events_list <- future_map(match_ids, parallel_safe_fetch, .progress = TRUE)

# Remove NULLs
filtered_events_list <- filtered_events_list[!sapply(filtered_events_list, is.null)]

# Save filtered events
if (length(filtered_events_list) > 0) {
  filtered_events_list <- align_columns(filtered_events_list)
  all_filtered_events <- do.call(rbind, filtered_events_list)
  
  # Prepare metadata
  meta_df <- all_matches[, c("match_id", 
                             "competition.country_name", 
                             "competition.competition_name", 
                             "season.season_name")]
  
  all_filtered_events <- merge(all_filtered_events, meta_df, by = "match_id", all.x = TRUE)
  
  write.xlsx(all_filtered_events, "filtered_goals_all_matches.xlsx", na.string = "NA")
  cat("✓ Saved all filtered goals to filtered_goals_all_matches.xlsx with metadata\n")
} else {
  cat("✗ No filtered events found for any match.\n")
}

cat("✅ All done.\n")
