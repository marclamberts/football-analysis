# =========================
# Load necessary packages
# =========================
library(StatsBombR)
library(dplyr)
library(purrr)
library(openxlsx)

# =========================
# User credentials
# =========================
username <- "JACK71299@HOTMAIL.CO.UK"
password <- "J7rB7aP2"

# =========================
# Competitions and seasons
# =========================
competition_ids <- c(108)  # Example competitions
season_ids <- c(315)       # Example seasons

# =========================
# Fetch matches safely
# =========================
get_matches_safe <- function(username, password, comp_id, season_id) {
  tryCatch({
    get.matches(username, password, season_id, comp_id)
  }, error = function(e) {
    message(paste("Failed to fetch matches for comp:", comp_id, "season:", season_id))
    return(NULL)
  })
}

# =========================
# Fetch all matches
# =========================
all_matches <- purrr::map2_dfr(competition_ids, season_ids, function(comp_id, season_id) {
  get_matches_safe(username, password, comp_id, season_id)
})

if (nrow(all_matches) == 0) stop("No matches found.")

# =========================
# Create match lookup
# =========================
all_matches <- all_matches %>%
  mutate(Match = paste0(home_team.home_team_name, " - ", away_team.away_team_name))

match_lookup <- setNames(all_matches$Match, all_matches$match_id)

# =========================
# Fetch events safely
# =========================
fetch_events_safe <- function(username, password, match_id) {
  tryCatch({
    df <- allevents(username, password, match_id)
    if (!is.null(df) && nrow(df) > 0) {
      df$match_id <- match_id
      df$Match <- match_lookup[as.character(match_id)]
    }
    return(df)
  }, error = function(e) {
    message(paste("Failed to fetch events for match_id:", match_id))
    return(NULL)
  })
}

# =========================
# Loop over matches
# =========================
all_corners_shots_list <- list()

for (match_id in unique(all_matches$match_id)) {
  cat("Fetching events for match_id:", match_id, "\n")
  
  df <- fetch_events_safe(username, password, match_id)
  
  if (!is.null(df) && nrow(df) > 0) {
    # Filter corner passes
    corner_passes <- df %>%
      filter(type.name == "Pass",
             play_pattern.name == "From Corner",
             pass.type.name == "Corner") %>%
      select(match_id, Match, index, possession, possession_team.id,
             timestamp, location, pass.end_location, pass.height.name, pass.body_part.name,
             pass.outcome.name, pass.technique.name, team.name) %>%
      mutate(event_type = "CornerPass")
    
    if (nrow(corner_passes) > 0) {
      # Get unique possession IDs from the corner passes
      target_possessions <- unique(corner_passes$possession)
      
      # Filter shots within the same possessions
      shots_in_possession <- df %>%
        filter(type.name == "Shot", possession %in% target_possessions) %>%
        select(match_id, Match, index, possession, possession_team.id,
               timestamp, location, shot.outcome.name, shot.statsbomb_xg, team.name) %>%
        mutate(event_type = "Shot")
      
      combined_events <- bind_rows(corner_passes, shots_in_possession)
      all_corners_shots_list[[length(all_corners_shots_list) + 1]] <- combined_events
      
      cat("✓ Added", nrow(corner_passes), "corner passes and", nrow(shots_in_possession), "shots\n")
    } else {
      cat("✗ No corner passes in this match\n")
    }
  } else {
    cat("✗ No events found for this match\n")
  }
}

# =========================
# Prepare metadata columns from all_matches
# =========================
meta_df <- all_matches %>%
  select(match_id, 
         competition.country_name, 
         competition.competition_name, 
         season.season_name)

# =========================
# Combine and save results with metadata
# =========================
if (length(all_corners_shots_list) > 0) {
  all_corners_shots <- bind_rows(all_corners_shots_list)
  
  # Merge metadata to final df by match_id
  all_corners_shots <- merge(all_corners_shots, meta_df, by = "match_id", all.x = TRUE)
  
  write.xlsx(all_corners_shots, "corner_passes_and_shots_with_metadata.xlsx")
  cat("✅ Saved all corner passes and related shots with metadata to corner_passes_and_shots_with_metadata.xlsx\n")
} else {
  cat("⚠️ No corner passes found across matches.\n")
}
