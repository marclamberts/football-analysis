# =========================
# Load necessary packages
# =========================
library(StatsBombR)
library(dplyr)
library(purrr)
library(openxlsx)
library(lubridate)

# =========================
# User credentials
# =========================
username <- "JACK71299@HOTMAIL.CO.UK"
password <- "J7rB7aP2"

# =========================
# Competitions and seasons
# =========================
competition_ids <- c(108)
season_ids <- c(315)

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
# Fetch first 3 matches
# =========================
all_matches <- purrr::map2_dfr(competition_ids, season_ids, function(comp_id, season_id) {
  get_matches_safe(username, password, comp_id, season_id)
})

if (nrow(all_matches) == 0) stop("No matches found.")

all_matches <- all_matches %>%
  mutate(Match = paste0(home_team.home_team_name, " - ", away_team.away_team_name)) %>%
  slice_head(n = 3)

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
# Defensive Setup Function
# =========================
get_defensive_setup_flat <- function(freeze_frame) {
  if (is.null(freeze_frame) || length(freeze_frame) == 0 || !is.list(freeze_frame)) {
    return("")
  }
  
  if (length(freeze_frame) < 7) {
    return("")
  }
  
  x_positions <- freeze_frame[[1]]
  y_positions <- freeze_frame[[2]]
  teammates <- freeze_frame[[3]]
  
  if (length(x_positions) == 0 || length(y_positions) == 0 || length(teammates) == 0) {
    return("")
  }
  
  player_df <- data.frame(
    x = x_positions,
    y = y_positions,
    teammate = teammates,
    stringsAsFactors = FALSE
  )
  
  defenders <- player_df %>%
    filter(teammate == FALSE) %>%
    filter(x < 6, y >= 20, y <= 60)
  
  if (nrow(defenders) == 0) {
    return("")
  }
  
  defenders <- defenders %>%
    mutate(zone = cut(y, breaks = c(20, 33.33, 46.66, 60), labels = c("Front", "Middle", "Back"), right = FALSE))
  
  zone_counts <- table(defenders$zone)
  zone_counts <- zone_counts[zone_counts > 0]
  
  if (length(zone_counts) == 0) {
    return("")
  }
  
  result <- paste0(names(zone_counts), ": ", zone_counts, collapse = ", ")
  return(result)
}

# =========================
# Process matches
# =========================
all_corners_shots_list <- list()

for (match_id in unique(all_matches$match_id)) {
  cat("Fetching events for match_id:", match_id, "\n")
  
  df <- fetch_events_safe(username, password, match_id)
  
  if (!is.null(df) && nrow(df) > 0) {
    corner_passes <- df %>%
      filter(type.name == "Pass",
             play_pattern.name == "From Corner",
             pass.type.name == "Corner") %>%
      select(match_id, Match, possession, possession_team.id, index.pass = index,
             pass_timestamp = timestamp, location, pass.end_location,
             pass.height.name, pass.body_part.name, pass.outcome.name,
             pass.technique.name, team.name, player.name, position.name) %>%
      mutate(event_type = "CornerPass",
             Taker = player.name,
             pass_position = position.name) %>%
      select(-player.name, -position.name)
    
    if (nrow(corner_passes) > 0) {
      target_possessions <- unique(corner_passes$possession)
      
      shots_in_possession <- df %>%
        filter(type.name == "Shot", possession %in% target_possessions) %>%
        select(match_id, Match, possession, possession_team.id, index.shot = index,
               shot_timestamp = timestamp, location, shot.outcome.name,
               shot.statsbomb_xg, team.name, player.name, position.name,
               shot.body_part.name, shot.freeze_frame) %>%
        mutate(event_type = "Shot",
               Shooter = player.name,
               shot_position = position.name) %>%
        select(-player.name, -position.name)
      
      combined <- left_join(corner_passes, shots_in_possession, by = c("match_id", "Match", "possession", "possession_team.id"))
      
      if (nrow(combined) > 0) {
        combined <- combined %>%
          mutate(pass_location_x = purrr::map_dbl(location.x, ~ if (!is.null(.x)) .x[[1]] else NA_real_),
                 pass_location_y = purrr::map_dbl(location.x, ~ if (!is.null(.x)) .x[[2]] else NA_real_),
                 pass_end_location_x = purrr::map_dbl(pass.end_location, ~ if (!is.null(.x)) .x[[1]] else NA_real_),
                 pass_end_location_y = purrr::map_dbl(pass.end_location, ~ if (!is.null(.x)) .x[[2]] else NA_real_),
                 shot_location_x = purrr::map_dbl(location.y, ~ if (!is.null(.x)) .x[[1]] else NA_real_),
                 shot_location_y = purrr::map_dbl(location.y, ~ if (!is.null(.x)) .x[[2]] else NA_real_),
                 shot_location_z = purrr::map_dbl(location.y, ~ if (!is.null(.x) && length(.x) >= 3) .x[[3]] else NA_real_),
                 Defensive_setup = purrr::map_chr(shot.freeze_frame, ~ get_defensive_setup_flat(.x))) %>%
          select(match_id, Match, possession, pass_timestamp, Taker, pass_position, pass.height.name, pass.body_part.name,
                 pass.outcome.name, pass.technique.name, pass_location_x, pass_location_y,
                 pass_end_location_x, pass_end_location_y,
                 shot_timestamp, Shooter, shot_position, shot.body_part.name,
                 shot.outcome.name, shot.statsbomb_xg,
                 shot_location_x, shot_location_y, shot_location_z, Defensive_setup)
        
        all_corners_shots_list[[length(all_corners_shots_list) + 1]] <- combined
        cat("✓ Added", nrow(combined), "corner-pass-shot combinations\n")
      }
    } else {
      cat("✗ No corner passes in this match\n")
    }
  } else {
    cat("✗ No events found for this match\n")
  }
}

# =========================
# Final Processing and Save
# =========================
if (length(all_corners_shots_list) > 0) {
  final_df <- bind_rows(all_corners_shots_list)
  
  # Extract Minute and Second from pass_timestamp
  final_df <- final_df %>%
    mutate(
      pass_time = hms(pass_timestamp),
      Minute = minute(pass_time),
      Second = second(pass_time)
    )
  
  # Calculate SP_outcome based on time difference
  final_df <- final_df %>%
    group_by(match_id, possession) %>%
    mutate(
      pass_time_sec = as.numeric(hms(pass_timestamp)),
      shot_time_sec = as.numeric(hms(shot_timestamp)),
      time_diff = shot_time_sec - pass_time_sec,
      min_time_diff = suppressWarnings(min(time_diff[!is.na(time_diff)], na.rm = TRUE)),
      SP_outcome = case_when(
        all(is.na(shot_timestamp)) ~ 'No first contact - no shot',
        min_time_diff == 0 ~ 'First contact - direct shot',
        min_time_diff > 0 & min_time_diff <= 3 ~ 'First contact - shot within 3 seconds',
        min_time_diff > 3 ~ 'No first contact - shot',
        TRUE ~ 'First contact - no shot'
      )
    ) %>%
    ungroup() %>%
    select(-pass_time_sec, -shot_time_sec, -time_diff, -min_time_diff, -pass_time)
  
  write.xlsx(final_df, "corner_passes_and_shots_defensive_setup.xlsx")
  cat("✅ Data saved to corner_passes_and_shots_defensive_setup.xlsx with SP_outcome, Minute, and Second columns\n")
  
} else {
  cat("⚠️ No corner passes found across matches.\n")
}
