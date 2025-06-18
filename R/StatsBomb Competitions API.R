library(StatsBombR)
library(openxlsx)  # for writing Excel files

# Your two private accounts' credentials
username1 <- "Cem@fc32.com"
password1 <- "FxtA72Ux"

username2 <- "JACK71299@HOTMAIL.CO.UK"
password2 <- "J7rB7aP2"

# Get competitions from first account
comps1 <- competitions(username = username1, password = password1)

# Get competitions from second account
comps2 <- competitions(username = username2, password = password2)

# Combine the two competitions data frames
combined_comps <- rbind(comps1, comps2)

# Optionally, remove duplicate rows (if any) based on all columns or specific keys
combined_comps <- unique(combined_comps)

# Save combined competitions to Excel file
write.xlsx(combined_comps, file = "combined_competitions.xlsx")

cat("Combined competitions saved to combined_competitions.xlsx\n")

