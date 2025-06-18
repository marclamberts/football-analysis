library(StatsBombR)
library(openxlsx)

# Credentials (both accounts)
username1 <- "Cem@fc32.com"
password1 <- "FxtA72Ux"

username2 <- "JACK71299@HOTMAIL.CO.UK"
password2 <- "J7rB7aP2"

# Load match IDs from your Excel file
match_df <- read.xlsx("match_ids_all.xlsx")

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

# Get unique match IDs
match_ids <- unique(match_df$match_id)

# Loop through all match IDs and save events for each
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
    # Save to Excel file named by match ID
    file_name <- paste0("events_match_", match_id, ".xlsx")
    write.xlsx(ev, file_name, na.string = "NA")
    cat("✓ Saved", nrow(ev), "events to", file_name, "\n")
  } else {
    cat("✗ No events found for match ID", match_id, "\n")
  }
}

cat("✅ All done.\n")
