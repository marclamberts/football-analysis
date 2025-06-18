library(data.table)
library(dplyr)
library(tidyverse)
library(magrittr)
library(xgboost)
library(Matrix)
library(ade4)
library(caret)
library(jsonlite)
library(glue)

# Load your custom packages (make sure these are installed/loaded correctly)
library("dashboardFunctions")
library("footballFunctions")

# Load dummy data for prediction
load('~/dummy.rda')

# List of JSON files to process
filenames <- list.files("~/xG/WWC", pattern="*.json", full.names=TRUE)

# Read matches CSV for team info
matches <- read.csv("/Users/user/xG/WWC Matches.csv") %>%
  select(matchInfo.contestant.0.id, matchInfo.contestant.0.officialName) %>%
  unique() %>%
  `colnames<-`(c("contestantId","TeamId")) %>%
  mutate(TeamId = gsub('/', '', TeamId))

lijstShot <- list()
Wedstrijdlijst <- list()
lijstMinuten <- list()

for(i in seq_along(filenames)){
  print(paste("Processing file:", i))
  
  # Read JSON data
  events2 <- jsonlite::read_json(filenames[i])
  df2 <- events2$event
  
  # Bind rows into data.table
  game2 <- data.table::rbindlist(df2, fill=TRUE)
  print("Columns in game2:")
  print(colnames(game2))
  
  # Join with matches dataset - check correct column names before join
  # Adjust this join depending on actual column names in game2
  # Commonly it might be "teamId" or "TeamId" in game2, matches has "contestantId"
  if("teamId" %in% colnames(game2)){
    game2 <- game2 %>% left_join(matches, by = c("teamId" = "contestantId"))
  } else if ("TeamId" %in% colnames(game2)){
    game2 <- game2 %>% left_join(matches, by = c("TeamId" = "contestantId"))
  } else {
    warning("No teamId or TeamId column found in game2 for join with matches")
  }
  
  # Unnest qualifier column if present
  game_wide2 <- unnest_wider(game2, qualifier, names_sep = "_") 
  
  # Extract match info from filename
  game_wide2 <- game_wide2 %>%
    mutate(match = gsub(".json", "", basename(filenames[i]))) %>%
    mutate(match = gsub(".*_", "", match)) %>%
    mutate(match = gsub('/', '', match))
  
  game_wide2 <- game_wide2 %>% separate(match, c("HomeTeam", "AwayTeam"), sep = " - ")
  
  # Add qualifier and event names from external reference files
  With_names <- game_wide2 %>%
    left_join(
      read.csv("https://raw.githubusercontent.com/tomh05/football-scores/master/data/reference/opta-qualifiers.csv", header = FALSE) %>%
        select(V1, V3) %>%
        rename(Qualifier_name = V3),
      by = c("qualifier_qualifierId" = "V1")
    ) %>%
    left_join(
      read.csv("https://raw.githubusercontent.com/tomh05/football-scores/master/data/reference/opta-events.csv", header = FALSE) %>%
        select(V1, V2),
      by = c("typeId" = "V1")
    ) %>%
    rename(eventName = V2) %>%
    select(-c(id, lastModified, qualifier_id, playerId)) %>%
    filter(eventId > 1) %>%
    mutate(
      isShot = eventName %in% c("Goal ", "Attempt Saved ", "Miss ", "Post "),
      isGoal = eventName == "Goal "
    )
  
  # Create ShotOnTarget column: TRUE if eventName is "Goal " or "Attempt Saved "
  With_names <- With_names %>%
    mutate(ShotOnTarget = eventName %in% c("Goal ", "Attempt Saved "))
  
  # Create a wide df with qualifiers
  df <- With_names %>%
    unique() %>%
    select(eventId, outcome, keyPass, periodId, timeMin, timeSec, HomeTeam, AwayTeam, playerName, TeamId,
           isShot, isGoal, x, y, timeStamp, eventName, Qualifier_name, ShotOnTarget) %>%
    group_by(eventId, outcome, keyPass, periodId, timeMin, timeSec, HomeTeam, AwayTeam, playerName, TeamId,
             x, y, timeStamp, eventName, isShot, isGoal, ShotOnTarget) %>%
    mutate(row = seq(n())) %>%
    ungroup() %>%
    pivot_wider(names_from = "row", values_from = "Qualifier_name")
  
  # Prepare related KKD data
  df_rel <- getRelatedKKD(With_names, df)
  
  # Prepare df1 for prediction
  df1 <- PrepAGameKKD(df_rel)
  
  # Prepare data for prediction, add dummy data
  df_for_pred <- df1 %>%
    select(Goal, x, y, Bodypart, Type_of_play, isIntentionalAssist, isAssistedShot,
           isBigChance, Gamestate, Time_in_sec, distance, angle)
  
  df_for_pred <- rbind(dummy, df_for_pred)
  df_for_pred$Goal[is.na(df_for_pred$Goal)] <- 0
  df_for_pred$Type_of_play[is.na(df_for_pred$Type_of_play)] <- "RegularPlay"
  df_for_pred$Bodypart[is.na(df_for_pred$Bodypart)] <- "RightFoot"
  
  # Predict xG
  pred <- PredictxG(df_for_pred)
  xG <- as.data.frame(pred[-c(1:15)])
  
  # Combine predictions with df1
  Wedstrijd <- cbind(xG, df1)
  
  # Add ShotOnTarget back by joining on timeStamp
  Wedstrijd <- Wedstrijd %>%
    left_join(df %>% select(timeStamp, ShotOnTarget), by = "timeStamp")
  
  colnames(Wedstrijd)[1] <- "xG"
  Wedstrijd$xG <- ifelse(Wedstrijd$isOwnGoal == TRUE, 0, Wedstrijd$xG)
  Wedstrijd$xG <- ifelse(Wedstrijd$Type_of_play == "Penalty", 0.79, Wedstrijd$xG)
  
  Wedstrijd <- Wedstrijd %>%
    rename(PlayerId = playerName)
  
  # Save shot data with ShotOnTarget column
  lijstShot[[i]] <- Wedstrijd
  write.csv(Wedstrijd, glue('~/xG/WWC/xgCSV/{gsub(".json","",basename(filenames[i]))}.csv'), row.names = FALSE)
  
  # Summarize home and away teams xG and goals for this game
  Home <- Wedstrijd %>% filter(TeamId == HomeTeam)
  Away <- Wedstrijd %>% filter(TeamId == AwayTeam)
  
  HomexG <- Home$xG
  AwayxG <- Away$xG
  
  Home$sum <- cumsum(Home$xG)
  Away$sum <- cumsum(Away$xG)
  
  Home$Goal[is.na(Home$Goal)] <- 0
  Away$Goal[is.na(Away$Goal)] <- 0
  
  HomePoints <- ifelse(sum(Home$Goal) > sum(Away$Goal), 3,
                       ifelse(sum(Home$Goal) < sum(Away$Goal), 0, 1))
  AwayPoints <- ifelse(sum(Home$Goal) < sum(Away$Goal), 3,
                       ifelse(sum(Home$Goal) > sum(Away$Goal), 0, 1))
  
  HomeWinst <- ifelse(HomePoints == 3, 1, 0)
  AwayWinst <- ifelse(AwayPoints == 3, 1, 0)
  
  HomeGelijk <- ifelse(HomePoints == 1, 1, 0)
  AwayGelijk <- ifelse(AwayPoints == 1, 1, 0)
  
  HomeNederlaag <- ifelse(HomePoints == 0, 1, 0)
  AwayNederlaag <- ifelse(AwayPoints == 0, 1, 0)
  
  game <- calculateChance(HomexG, AwayxG, 10000)
  kansen <- data.frame(
    Wedstrijd$Date[1], Wedstrijd$Home[1], sum(Home$Goal),
    sum(Away$Goal), Wedstrijd$Away[1], round(sum(HomexG), 2), round(sum(AwayxG), 2), game,
    HomePoints, AwayPoints, HomeWinst, HomeGelijk, HomeNederlaag, AwayWinst, AwayGelijk, AwayNederlaag
  ) %>%
    set_colnames(c(
      "Datum", "ThuisTeam", "Thuisgoals", "Uitgoals", "Uitteam", "xGThuis", "xGUit", "Kans winst thuisteam",
      "Kans gelijkspel", "Kans winst Uitteam", "xPointsHome", "xPointsAway", "HomePoints", "AwayPoints",
      "HomeWinst", "HomeGelijk", "HomeNederlaag", "AwayWinst", "AwayGelijk", "AwayNederlaag"
    ))
  
  Wedstrijdlijst[[i]] <- kansen
  
  both <- AddxGenxA(Wedstrijd)
  xT <- df_rel %>%
    left_join(getEndKKD(With_names)) %>%
    rename(PlayerId = playerName) %>%
    dashboardFunctions::typeOfPlayKKD() %>%
    mutate(Type_of_play = replace_na(Type_of_play, "RegularPlay")) %>%
    filter(Type_of_play == "RegularPlay") %>%
    dashboardFunctions::getCrossesKKD() %>%
    getXTPerPassKKD() %>%
    filter(outcome == 1) %>%
    filter(eventName == "Pass ") %>%
    group_by(PlayerId, TeamId) %>%
    filter(!is.na(net)) %>%
    summarise(Total = sum(net))
  
  dfminutes <- MinutesKKD(df_rel)
  
  dfminutes <- dfminutes %>% select(PlayerId, TeamId, expandedMinute)
  dfminutes[is.na(dfminutes)] <- 0
  dfminutes <- left_join(dfminutes, both, by = c("PlayerId", "TeamId")) %>%
    left_join(xT, by = c("PlayerId", "TeamId"))
  dfminutes[is.na(dfminutes)] <- 0
  
  lijstMinuten[[i]] <- dfminutes
}
