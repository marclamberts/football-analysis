library(StatsBombR)
library(openxlsx)

# Account 1
username1 <- "Cem@fc32.com"
password1 <- "FxtA72Ux"

# Account 2
username2 <- "JACK71299@HOTMAIL.CO.UK"
password2 <- "J7rB7aP2"

# Your competition and season ID pairs
competition_ids <- c(108)     # Example competitions
season_ids <- c(315)       # Corresponding seasons

# Function to safely get match IDs
safe_match_ids <- function(username, password, season_id, comp_id) {
  tryCatch({
    mv <- matchesvector(username, password, season_id, comp_id)
    return(mv)
  }, error = function(e) {
    message(paste("Failed:", comp_id, season_id, "with", username))
    return(NULL)
  })
}

# Collect match IDs
all_match_ids <- list()

for (i in seq_along(competition_ids)) {
  comp_id <- competition_ids[i]
  season_id <- season_ids[i]
  
  cat("Trying competition:", comp_id, "season:", season_id, "\n")
  
  # Try first account
  mv1 <- safe_match_ids(username1, password1, season_id, comp_id)
  
  # Try second account if first fails
  if (is.null(mv1) || length(mv1) == 0) {
    mv2 <- safe_match_ids(username2, password2, season_id, comp_id)
    mv <- mv2
  } else {
    mv <- mv1
  }
  
  # Save results
  if (!is.null(mv) && length(mv) > 0) {
    df <- data.frame(
      competition_id = comp_id,
      season_id = season_id,
      match_id = mv
    )
    all_match_ids[[length(all_match_ids) + 1]] <- df
    cat("✓ Retrieved", length(mv), "match IDs\n")
  } else {
    cat("✗ No match IDs found\n")
  }
}

# Combine and save
if (length(all_match_ids) > 0) {
  final_ids <- do.call(rbind, all_match_ids)
  write.xlsx(final_ids, "match_ids_all.xlsx", na.string = "NA")
  cat("✅ Saved all match IDs to match_ids_all.xlsx\n")
} else {
  cat("⚠️ No match IDs retrieved\n")
}
