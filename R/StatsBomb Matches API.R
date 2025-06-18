library(StatsBombR)
library(openxlsx)

# Account 1
username1 <- "Cem@fc32.com"
password1 <- "FxtA72Ux"

# Account 2
username2 <- "JACK71299@HOTMAIL.CO.UK"
password2 <- "J7rB7aP2"

# Replace with your actual lists of competition IDs and season IDs
competition_ids <- c()  # Example
season_ids <- c(317)    # Matching season IDs

# Safety wrapper
safe_get_matches <- function(username, password, season_id, comp_id) {
  tryCatch({
    get.matches(username, password, season_id, comp_id)
  }, error = function(e) {
    message(paste("Failed for comp:", comp_id, "season:", season_id, "user:", username))
    return(NULL)
  })
}

# Store results
all_matches_list <- list()

# Loop through all comp-season pairs
for (i in seq_along(competition_ids)) {
  comp_id <- competition_ids[i]
  season_id <- season_ids[i]
  
  cat("Trying competition:", comp_id, "season:", season_id, "\n")
  
  # Try account 1 first
  matches_df <- safe_get_matches(username1, password1, season_id, comp_id)
  
  # If account 1 fails or returns empty, try account 2
  if (is.null(matches_df) || nrow(matches_df) == 0) {
    matches_df <- safe_get_matches(username2, password2, season_id, comp_id)
  }
  
  # If successful, add to list
  if (!is.null(matches_df) && nrow(matches_df) > 0) {
    matches_df$competition_id <- comp_id
    matches_df$season_id <- season_id
    all_matches_list[[length(all_matches_list) + 1]] <- matches_df
    cat("✓ Fetched matches\n")
  } else {
    cat("✗ No matches found\n")
  }
}

# Combine and save
if (length(all_matches_list) > 0) {
  all_matches <- do.call(rbind, all_matches_list)
  write.xlsx(all_matches, file = "all_matches_two_accounts.xlsx", na.string = "NA")
  cat("✅ All matches saved to all_matches_two_accounts.xlsx\n")
} else {
  cat("⚠️ No matches found for any competition + season pair\n")
}
