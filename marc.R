

library(data.table)
library(dplyr)
library(tidyverse)
library(magrittr)
library(xgboost)
library(Matrix)
library(magrittr)
library(ade4)
library(caret)
#setwd('~/Documents/R_with_git/Football/')
library("dashboardFunctions")
library("footballFunctions")

i <- 1
# path to where you stored the dummy
load('~/XG/dummy.rda') 
# make path on next line the fodler with all the json files
#list.files("~/Downloads/KKD")
filenames <- list.files("~/XG/A-League Women 2021-2022", pattern="*.json", full.names=TRUE)
#filenames <- "~/Downloads/KKD/2023-04-17_AFC Ajax II - PSV Eindhoven II.json"
all_games <- data.frame()
colnames(all_games)
#path to the csv with matches. Important for the team names (home and away)
matches <- read.csv("/Users/user/XG/A-League Women 2021-2022 Matches.csv") %>%
  select(matchInfo.contestant.0.id,matchInfo.contestant.0.officialName) %>%
  unique() %>%
  `colnames<-`(c("contestantId","TeamId")) %>%
  mutate(TeamId=gsub('/','',TeamId))
  #mutate(TeamId= 
   #        stringi::stri_replace_all_regex(TeamId,
    #                                       pattern=c('SV Roda', 'SC ', 'Eindhoven II','Alkmaar Zaanstreek','AFC ', 'HFC ','BV '),
     #                                      replacement=c('Roda', '', 'II','AZ','','',''),
      #                                     vectorize=FALSE))

lijstShot <- list()
Wedstrijdlijst <- list()
lijstMinuten <- list()
for(i in 1:length(filenames)){
  #for(i in 300:length(filenames)){
  #for(i in 276:276){
  print(i)
  
  #gsub("*._","",filenames[i])
  events2 <- jsonlite::read_json(filenames[i])
  df2 <- events2$event
  game2 <- data.table::rbindlist(df2,fill=TRUE) %>%
    left_join(matches)
  game_wide2 <- unnest_wider(game2, qualifier, names_sep = "_") 
  game_wide2 <- game_wide2 %>%
    mutate(match =
             gsub(".json","",str_split_fixed(filenames[i], n=6,pattern = "/")[6])) %>%
    mutate(match =
             gsub(".*_","",match)) %>%
      mutate(match=gsub('/','',match))
             
  game_wide2 <- 
    game_wide2 %>% separate(match, c("HomeTeam", "AwayTeam"), sep=" - ")
  
  print(gsub(".json","",str_split_fixed(filenames[i], n=7,pattern = "/")[7]))
  
  With_names <- game_wide2 %>%
    left_join( read.csv("https://raw.githubusercontent.com/tomh05/football-scores/master/data/reference/opta-qualifiers.csv",header = FALSE) %>%
      #read.csv("opta-qualifiers.csv",header=FALSE) %>%
        select(V1, V3) %>%
        rename( "Qualifier_name" = "V3"),
      by = c("qualifier_qualifierId" = "V1")) %>%
    left_join(read.csv("https://raw.githubusercontent.com/tomh05/football-scores/master/data/reference/opta-events.csv",header = FALSE) %>%
      #read.csv("opta-events.csv",header=FALSE) %>%
        select(V1,V2), by = c("typeId" = "V1")) %>%
    rename(eventName = V2) %>%
    select(-c(id,lastModified,qualifier_id,playerId)) %>%
    filter(eventId > 1) %>%
    mutate(isShot = ifelse(eventName %in% c("Goal ","Attempt Saved ", "Miss ", "Post "), TRUE,FALSE )) %>%
    mutate(isGoal = ifelse(eventName == "Goal ", TRUE,FALSE)) 
  
  df <- With_names |>
    unique() %>%
    select(eventId,outcome,keyPass,periodId,timeMin,timeSec,HomeTeam,AwayTeam,playerName,TeamId,isShot,isGoal,x,y, timeStamp,eventName,Qualifier_name ) %>%
    group_by(eventId,outcome,keyPass,periodId,timeMin,timeSec,HomeTeam,AwayTeam,playerName, TeamId,x,y,timeStamp,eventName,isShot,isGoal) |>
    mutate(row = seq(n())) |>
    ungroup() |>
    pivot_wider(names_from = "row", values_from = "Qualifier_name") 
  
  df_rel <- getRelatedKKD(With_names,df)
  # write.csv(df_rel %>% 
  #            left_join(getEndKKD(With_names)), glue::glue('KKDCSV/{gsub(".json","",str_split_fixed(filenames[i], n=7,pattern = "/")[7])}.csv'))
  
  df1 <- PrepAGameKKD(df_rel)
  
  df_for_pred <- df1 %>% select(Goal,x,y,Bodypart,Type_of_play,isIntentionalAssist,isAssistedShot,isBigChance,Gamestate,Time_in_sec,distance,angle)
  
  df_for_pred <- rbind(dummy,df_for_pred)
  df_for_pred$Goal[is.na(df_for_pred$Goal)] <- 0
  df_for_pred$Type_of_play[is.na(df_for_pred$Type_of_play)] <- "RegularPlay"
  
  df_for_pred$Bodypart[is.na(df_for_pred$Bodypart)] <- "RightFoot"
  nc <- 2
  pred <- PredictxG(df_for_pred)
  xG <- as.data.frame(pred[-c(1:15)])
  
  Wedstrijd <- cbind(xG,df1)
  colnames(Wedstrijd)[1]<- "xG"
  Wedstrijd$xG <- ifelse(Wedstrijd$isOwnGoal == TRUE,0,Wedstrijd$xG)
  Wedstrijd$xG <- ifelse(Wedstrijd$Type_of_play == "Penalty",0.79,Wedstrijd$xG)
  Wedstrijd <- Wedstrijd %>%
    rename(PlayerId = playerName)
  lijstShot[[i]] <- Wedstrijd
  write.csv(Wedstrijd, glue::glue('~/XG/A-League Women 2021-2022/xgCSV/{gsub(".json","",str_split_fixed(filenames[i], n=6,pattern = "/")[6])}.csv'))
  #######
  Home <- Wedstrijd %>% filter(TeamId == HomeTeam)
  Away <- Wedstrijd %>% filter(TeamId == AwayTeam)
  
  HomexG <- Home$xG
  AwayxG <- Away$xG
  # HomexG <- ifelse(identical(HomexG, numeric(0)),0.00001, HomexG) 
  # HomexG <- ifelse(Away$HomeTeam == "Willem II" & Away$AwayTeam == "PSV Eindhoven",0.0001,HomexG)
  
  Home$sum <- cumsum(Home$xG)
  Away$sum <- cumsum(Away$xG)
  Home$Goal[is.na(Home$Goal)] <- 0
  Away$Goal[is.na(Away$Goal)] <- 0
  HomePoints <- ifelse(sum(Home$Goal)>sum(Away$Goal),3,
                       ifelse(sum(Home$Goal)<sum(Away$Goal),0,1))
  AwayPoints <- ifelse(sum(Home$Goal)<sum(Away$Goal),3,
                       ifelse(sum(Home$Goal)>sum(Away$Goal),0,1))
  HomeWinst <- ifelse(HomePoints == 3,1,0)
  AwayWinst <- ifelse(AwayPoints == 3,1,0)
  
  HomeGelijk <- ifelse(HomePoints == 1,1,0)
  AwayGelijk <- ifelse(AwayPoints == 1,1,0)
  
  HomeNederlaag <- ifelse(HomePoints == 0,1,0)
  AwayNederlaag <- ifelse(AwayPoints == 0,1,0)
  
  game <- calculateChance(HomexG , AwayxG,10000)
  kansen <- data.frame(Wedstrijd$Date[1],Wedstrijd$Home[1],sum(Home$Goal),
                       sum(Away$Goal),Wedstrijd$Away[1],round(sum(HomexG),2),round(sum(AwayxG),2),game,HomePoints,AwayPoints,
                       HomeWinst,HomeGelijk,HomeNederlaag,AwayWinst,AwayGelijk,AwayNederlaag) %>%
    set_colnames(c("Datum","ThuisTeam","Thuisgoals","Uitgoals","Uitteam","xGThuis","xGUit","Kans winst thuisteam",
                   "Kans gelijkspel","Kans winst Uitteam","xPointsHome","xPointsAway","HomePoints","AwayPoints",
                   "HomeWinst","HomeGelijk","HomeNederlaag","AwayWinst","AwayGelijk","AwayNederlaag"))
  #WedstrijdKans <- GetWedstrijden(Wedstrijd)
  Wedstrijdlijst[[i]] <- kansen
  #WedstrijdKans <- GetWedstrijden(Wedstrijd)
  #Wedstrijdlijst[[i]] <- WedstrijdKans
  
  both<- AddxGenxA(Wedstrijd)
  xT <- df_rel %>% 
    left_join(getEndKKD(With_names)) %>%
    rename(PlayerId = playerName) %>%
    dashboardFunctions::typeOfPlayKKD() %>%
    mutate(Type_of_play = replace_na(Type_of_play,"RegularPlay"))%>%
    filter(Type_of_play == "RegularPlay") %>%
    dashboardFunctions::getCrossesKKD() %>%
    #footballFunctions::getXTPerPassKKD() %>%
    getXTPerPassKKD() %>%
    filter(outcome == 1) %>%
    filter(eventName == "Pass ") %>%
    group_by(PlayerId,TeamId) %>%
    filter(!is.na(net)) %>%
    summarise(Total = sum(net)) 
  dfminutes <- MinutesKKD(df_rel)
  
  dfminutes <-dfminutes %>% select(PlayerId,TeamId,expandedMinute)
  dfminutes[is.na(dfminutes)] <- 0
  dfminutes <- left_join(dfminutes,both,by=c("PlayerId", "TeamId")) %>% 
    left_join(xT,by=c("PlayerId", "TeamId")) 
  dfminutes[is.na(dfminutes)] <- 0
  #dfminutes$Date <- dfcsv$Date[1]
  lijstMinuten[[i]] <- dfminutes
  
}

