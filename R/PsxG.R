# Load necessary libraries
library(data.table)
library(dplyr)
library(tidyverse)
library(magrittr)
library(xgboost)
library(Matrix)
library(ade4)
library(caret)
library(devtools)
library(openxlsx)

# Set working directory and load custom functions
# setwd('~/Documents/R_with_git/Football/')
library("dashboardFunctions")
library("footballFunctions")

# Load dummy data for predictions
load('~/R/dummy.rda')  

# Load JSON files and matches data
filenames <- list.files("~/Python/BM", pattern="*.json", full.names=TRUE)
matches <- read.csv("/Users/marclambertes/Python/Schedules/BM Matches.csv") %>%
  select(matchInfo.contestant.0.id, matchInfo.contestant.0.officialName) %>%
  unique() %>%
  `colnames<-`(c("contestantId", "TeamId")) %>%
  mutate(TeamId = gsub('/', '', TeamId))

# Initialize lists for storing data across all files
lijstShot <- list()
Wedstrijdlijst <- list()
lijstMinuten <- list()
xT_list <- list()  # New list to store xT data across files

for(i in 1:length(filenames)) {
  print(i)
  
  # Read JSON file and convert to data frame
  events2 <- jsonlite::read_json(filenames[i])
  df2 <- events2$event
  game2 <- data.table::rbindlist(df2, fill=TRUE) %>%
    left_join(matches)
  game_wide2 <- unnest_wider(game2, qualifier, names_sep = "_") 
  game_wide2 <- game_wide2 %>%
    mutate(match = gsub(".json", "", str_split_fixed(filenames[i], n=6, pattern="/")[6])) %>%
    mutate(match = gsub(".*_", "", match)) %>%
    mutate(match = gsub('/', '', match)) %>%
    separate(match, c("HomeTeam", "AwayTeam"), sep=" - ")
  
  # Add event names and qualifiers
  With_names <- game_wide2 %>%
    left_join(read.csv("https://raw.githubusercontent.com/tomh05/football-scores/master/data/reference/opta-qualifiers.csv", header=FALSE) %>%
                select(V1, V3) %>%
                rename("Qualifier_name" = "V3"), by=c("qualifier_qualifierId" = "V1")) %>%
    left_join(read.csv("https://raw.githubusercontent.com/tomh05/football-scores/master/data/reference/opta-events.csv", header=FALSE) %>%
                select(V1, V2), by=c("typeId" = "V1")) %>%
    rename(eventName = V2) %>%
    select(-c(id, lastModified, qualifier_id, playerId)) %>%
    filter(eventId > 1) %>%
    mutate(isShot = ifelse(eventName %in% c("Goal ", "Attempt Saved ", "Miss ", "Post "), TRUE, FALSE)) %>%
    mutate(isGoal = ifelse(eventName == "Goal ", TRUE, FALSE))
  
  df <- With_names %>%
    unique() %>%
    select(eventId, outcome, keyPass, periodId, timeMin, timeSec, HomeTeam, AwayTeam, playerName, TeamId, isShot, isGoal, x, y, timeStamp, eventName, Qualifier_name) %>%
    group_by(eventId, outcome, keyPass, periodId, timeMin, timeSec, HomeTeam, AwayTeam, playerName, TeamId, x, y, timeStamp, eventName, isShot, isGoal) %>%
    mutate(row = seq(n())) %>%
    ungroup() %>%
    pivot_wider(names_from = "row", values_from = "Qualifier_name")
  
  df_rel <- getRelatedKKD(With_names, df)
  
  df1 <- PrepAGameKKD(df_rel)
  
  df_for_pred <- df1 %>%
    select(Goal, x, y, Bodypart, Type_of_play, isIntentionalAssist, isAssistedShot, isBigChance, Gamestate, Time_in_sec, distance, angle)
  df_for_pred <- rbind(dummy, df_for_pred)
  df_for_pred$Goal[is.na(df_for_pred$Goal)] <- 0
  df_for_pred$Type_of_play[is.na(df_for_pred$Type_of_play)] <- "RegularPlay"
  df_for_pred$Bodypart[is.na(df_for_pred$Bodypart)] <- "RightFoot"
  
  # Predict xG
  pred <- PredictxG(df_for_pred)
  xG <- as.data.frame(pred[-c(1:15)])
  
  Wedstrijd <- cbind(xG, df1)
  colnames(Wedstrijd)[1] <- "xG"
  Wedstrijd$xG <- ifelse(Wedstrijd$isOwnGoal == TRUE, 0, Wedstrijd$xG)
  Wedstrijd$xG <- ifelse(Wedstrijd$Type_of_play == "Penalty", 0.79, Wedstrijd$xG)
  Wedstrijd <- Wedstrijd %>%
    rename(PlayerId = playerName)
  
  # Calculate PsxG
  Wedstrijd <- Wedstrijd %>%
    mutate(
      GoalMouthModifier = case_when(
        x > 80 & y > 35 & y < 65 ~ 1.2, # High probability near top corners
        x > 75 & y > 25 & y < 75 ~ 1.1, # Moderate probability near goal center
        TRUE ~ 0.8                # Low probability elsewhere
      ),
      ShotQualityModifier = case_when(
        Bodypart == "Header" ~ 0.7,   # Headers typically less accurate
        Bodypart == "LeftFoot" ~ 0.9, # Slightly lower for weaker foot
        TRUE ~ 1.0                   # Default for right foot or unspecified
      ),
      PsxG = xG * GoalMouthModifier * ShotQualityModifier
    )
  
  lijstShot[[i]] <- Wedstrijd
  write.csv(Wedstrijd, glue::glue('~/Python/BM/xgCSV/{gsub(".json", "", str_split_fixed(filenames[i], n=6, pattern="/")[6])}.csv'))
  
  # Additional logic for Home/Away summaries, xT, etc. remains unchanged
  # ...
}

# After loop - combine and save results
df <- do.call(rbind, lijstShot)
write.xlsx(df, "BM Expected Points with PsxG.xlsx")
