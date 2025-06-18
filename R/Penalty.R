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
username <- "Cem@fc32.com"
password <- "FxtA72Ux"

# =========================
# Competitions and seasons
# =========================
competition_ids <- c(107)  # Example competitions
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
# Loop over matches to extract penalties only
# =========================
all_penalty_shots_list <- list()

for (match_id in unique(all_matches$match_id)) {
  cat("Fetching events for match_id:", match_id, "\n")
  
  df <- fetch_events_safe(username, password, match_id)
  
  if (!is.null(df) && nrow(df) > 0) {
    # Filter only penalty shots
    penalty_shots <- df %>%
      filter(type.name == "Shot", shot.type.name == "Penalty") %>%
      select(match_id, Match, index, possession, possession_team.id,
             timestamp, location, shot.outcome.name, shot.type.name, team.name) %>%
      mutate(event_type = "PenaltyShot")
    
    if (nrow(penalty_shots) > 0) {
      all_penalty_shots_list[[length(all_penalty_shots_list) + 1]] <- penalty_shots
      cat("✓ Found", nrow(penalty_shots), "penalty shots\n")
    } else {
      cat("✗ No penalty shots in this match\n")
    }
  } else {
    cat("✗ No events found for this match\n")
  }
}

# =========================
# Combine and save results
# =========================
if (length(all_penalty_shots_list) > 0) {
  all_penalty_shots <- bind_rows(all_penalty_shots_list)
  
  write.xlsx(all_penalty_shots, "penalty_shots.xlsx")
  cat("✅ Saved all penalty shots to penalty_shots.xlsx\n")
} else {
  cat("⚠️ No penalty shots found across matches.\n")
}
