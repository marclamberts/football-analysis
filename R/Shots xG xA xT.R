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
  filenames <- list.files("~/Python/Allsvenskan 2024", pattern="*.json", full.names=TRUE)
  matches <- read.csv("/Users/marclambertes/Python/Schedules/Allsvenskan 2024 Matches.csv") %>%
    select(matchInfo.contestant.0.id, matchInfo.contestant.0.officialName) %>%
    unique() %>%
    `colnames<-`(c("contestantId", "TeamId")) %>%
    mutate(TeamId = gsub('/', '', TeamId))
  
  # Initialize lists for storing data across all files
  lijstShot <- list()
  Wedstrijdlijst <- list()
  lijstMinuten <- list()
  xT_list <- list()  # New list to store xT data across files
  
  for(i in 1:length(filenames)){
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
    lijstShot[[i]] <- Wedstrijd
    write.csv(Wedstrijd, glue::glue('~/Python/Allsvenskan 2024/xgCSV/{gsub(".json", "", str_split_fixed(filenames[i], n=6, pattern="/")[6])}.csv'))
    
    Home <- Wedstrijd %>% filter(TeamId == HomeTeam)
    Away <- Wedstrijd %>% filter(TeamId == AwayTeam)
    Home$sum <- cumsum(Home$xG)
    Away$sum <- cumsum(Away$xG)
    Home$Goal[is.na(Home$Goal)] <- 0
    Away$Goal[is.na(Away$Goal)] <- 0
    HomePoints <- ifelse(sum(Home$Goal) > sum(Away$Goal), 3, ifelse(sum(Home$Goal) < sum(Away$Goal), 0, 1))
    AwayPoints <- ifelse(sum(Home$Goal) < sum(Away$Goal), 3, ifelse(sum(Home$Goal) > sum(Away$Goal), 0, 1))
    
    kansen <- data.frame(
      Wedstrijd$Date[1], Wedstrijd$Home[1], sum(Home$Goal), sum(Away$Goal), Wedstrijd$Away[1],
      round(sum(Home$xG), 2), round(sum(Away$xG), 2),
      calculateChance(Home$xG, Away$xG, 10000),
      HomePoints, AwayPoints,
      ifelse(HomePoints == 3, 1, 0), ifelse(HomePoints == 1, 1, 0), ifelse(HomePoints == 0, 1, 0),
      ifelse(AwayPoints == 3, 1, 0), ifelse(AwayPoints == 1, 1, 0), ifelse(AwayPoints == 0, 1, 0)
    ) %>% set_colnames(c("Datum", "ThuisTeam", "Thuisgoals", "Uitgoals", "Uitteam", "xGThuis", "xGUit", "Kans winst thuisteam",
                         "Kans gelijkspel", "Kans winst Uitteam", "xPointsHome", "xPointsAway", "HomePoints", "AwayPoints",
                         "HomeWinst", "HomeGelijk", "HomeNederlaag", "AwayWinst", "AwayGelijk", "AwayNederlaag"))
    
    Wedstrijdlijst[[i]] <- kansen
    
    # Compute xT for current game
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
    
    xT_list[[i]] <- xT
    
    dfminutes <- MinutesKKD(df_rel) %>% 
      select(PlayerId, TeamId, expandedMinute) %>%
      mutate_all(~ replace_na(., 0)) %>%
      left_join(both, by=c("PlayerId", "TeamId")) %>%
      left_join(xT, by=c("PlayerId", "TeamId")) %>%
      mutate_all(~ replace_na(., 0))
    
    lijstMinuten[[i]] <- dfminutes
  }
  
  # After loop - combine and save results
  
  # Combine Wedstrijdlijst, xT, and xA and save to Excel
  df <- do.call(rbind, Wedstrijdlijst)
  write.xlsx(df, "Allsvenskan 2024 Expected Points.xlsx")
  
  xT_combined <- do.call(rbind, xT_list)
  write.xlsx(xT_combined, "Allsvenskan 2024 xT.xlsx")
  
  xA_combined <- do.call(rbind, lijstMinuten)
  write.xlsx(xA_combined, "Allsvenskan 2024xA.xlsx")
