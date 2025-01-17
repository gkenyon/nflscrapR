library(nflscrapR)
library(magrittr)
library(tidyverse)
library(dplyr)
library(purrr)
#Splits Require PlayByPlayBoxScore.R scrape data (PassAttempt Data, could be rectified by making qb_dropback=1 count as a PassAttempt?)
# LOADING SPLIT FUNCTIONS -------------------------------------------------
find_player_name <- function(player_names){
  if (length(player_names) == 0) {
    result <- "None"
  } else{
    table_name <- table(player_names)
    result <- names(table_name)[which.max(table_name)]
  }
  return(result)
}
calc_passing_splits <- function(splits,pbp_df) {
  split_groups <- lapply(splits, as.symbol)
  # Filter to only pass attempts and add the GameDrive column:
  pbp_df <- pbp_df %>% filter(PassAttempt == 1 & PlayType != "No Play") %>%
    mutate(GameDrive = paste(as.character(GameID), as.character(Drive), sep = "-"))
  pass_output <- pbp_df %>% group_by(.dots=split_groups) %>%
    summarise(Player_Name = find_player_name(Passer[which(!is.na(Passer))]),
              Attempts = n(),Completions = sum(Reception,na.rm=TRUE),
              Drives = n_distinct(GameDrive),
              Comp_Perc = Completions / Attempts,
              Total_Yards = sum(Yards.Gained,na.rm = TRUE),
              Total_Raw_AirYards = sum(AirYards,na.rm=TRUE),
              Total_Comp_AirYards = sum(Reception*AirYards, na.rm=TRUE),
              Yards_per_Att = Total_Yards / Attempts,
              Yards_per_Comp = Total_Yards / Completions,
              Yards_per_Drive = Total_Yards / Drives,
              Raw_AirYards_per_Att = Total_Raw_AirYards / Attempts,
              Comp_AirYards_per_Att = Total_Comp_AirYards / Attempts,
              Raw_AirYards_per_Comp = Total_Raw_AirYards / Completions,
              Comp_AirYards_per_Comp = Total_Comp_AirYards / Completions,
              Raw_AirYards_per_Drive = Total_Raw_AirYards / Drives,
              Comp_AirYards_per_Drive = Total_Comp_AirYards / Drives,
              PACR = Total_Yards / Total_Raw_AirYards,
              TimesHit = sum(QBHit,na.rm=TRUE),
              TimesHit_per_Drive = TimesHit / Drives,
              Interceptions = sum(InterceptionThrown,na.rm = TRUE),
              TDs = sum(Touchdown,na.rm=TRUE),
              Air_TDs = sum(as.numeric(YardsAfterCatch==0)*Touchdown,na.rm=TRUE),
              aPACR = (Total_Yards + (20 * TDs) - (45 * Interceptions)) / Total_Raw_AirYards,
              Air_TD_Rate = Air_TDs / TDs,
              TD_to_Int = TDs / Interceptions,
              Total_EPA = sum(EPA,na.rm=TRUE),
              Success_Rate = length(which(EPA>0)) / Attempts,
              EPA_per_Att = Total_EPA / Attempts,
              EPA_per_Comp = sum(Reception*EPA,na.rm=TRUE) / Completions,
              EPA_Comp_Perc = sum(Reception*EPA,na.rm=TRUE)/sum(abs(EPA),na.rm=TRUE),
              TD_per_Att = TDs / Attempts,
              Air_TD_per_Att = Air_TDs / Attempts,
              Int_per_Att = Interceptions / Attempts,
              TD_per_Comp = TDs / Completions,
              Air_TD_per_Comp = Air_TDs / Completions,
              TD_per_Drive = TDs / Drives,
              Air_TD_per_Drive = Air_TDs / Drives,
              Int_per_Drive = Interceptions / Drives,
              EPA_per_Drive = Total_EPA / Drives,
              Total_WPA = sum(WPA,na.rm=TRUE),
              Win_Success_Rate = length(which(WPA>0)) / Attempts,
              WPA_per_Att = Total_WPA / Attempts,
              WPA_per_Comp = sum(Reception*WPA,na.rm=TRUE) / Completions,
              WPA_Comp_Perc = sum(Reception*WPA,na.rm=TRUE)/sum(abs(WPA),na.rm=TRUE),
              WPA_per_Drive = Total_WPA / Drives,
              Total_Clutch_EPA = sum(EPA*abs(WPA),na.rm=TRUE),
              Clutch_EPA_per_Att = Total_Clutch_EPA / Attempts,
              Clutch_EPA_per_Drive = Total_Clutch_EPA / Drives,
              airEPA_Comp = sum(Reception*airEPA,na.rm = TRUE),
              airEPA_Incomp = sum(as.numeric(Reception == 0)*airEPA, na.rm = TRUE),
              Total_Raw_airEPA = sum(airEPA,na.rm = TRUE),
              Raw_airEPA_per_Att = Total_Raw_airEPA / Attempts,
              Raw_airEPA_per_Drive = Total_Raw_airEPA / Drives,
              epa_PACR = Total_EPA / Total_Raw_airEPA,
              airEPA_per_Att = airEPA_Comp / Attempts,
              airEPA_per_Comp = airEPA_Comp / Completions,
              airEPA_per_Drive = airEPA_Comp / Drives,
              air_Success_Rate = length(which(airEPA>0)) / Attempts,
              air_Comp_Success_Rate = length(which((Reception*airEPA)>0)) / Attempts,
              airWPA_Comp = sum(Reception*airWPA,na.rm = TRUE),
              airWPA_Incomp = sum(as.numeric(Reception == 0)*airWPA, na.rm = TRUE),
              Total_Raw_airWPA = sum(airWPA,na.rm = TRUE),
              wpa_PACR = Total_WPA / Total_Raw_airWPA,
              Raw_airWPA_per_Att = Total_Raw_airWPA / Attempts,
              Raw_airWPA_per_Drive = Total_Raw_airWPA / Drives,
              airWPA_per_Att = airWPA_Comp / Attempts,
              airWPA_per_Comp = airWPA_Comp / Completions,
              airWPA_per_Drive = airWPA_Comp / Drives,
              air_Win_Success_Rate = length(which(airWPA>0)) / Attempts,
              air_Comp_Win_Success_Rate = length(which((Reception*airWPA)>0)) / Attempts,
              yacEPA_Comp = sum(Reception*yacEPA,na.rm=TRUE),
              yacEPA_Drop = sum(as.numeric(Reception==0)*yacEPA,na.rm=TRUE),
              Total_yacEPA = sum(yacEPA,na.rm=TRUE),
              yacEPA_per_Att = Total_yacEPA / Attempts,
              yacEPA_per_Comp = yacEPA_Comp / Completions,
              yacEPA_Rec_per_Drive = yacEPA_Comp / Drives,
              yacEPA_Drop_per_Drive = yacEPA_Drop / Drives,
              yacWPA_Comp = sum(Reception*yacWPA,na.rm=TRUE),
              yacWPA_Drop = sum(as.numeric(Reception==0)*yacWPA,na.rm=TRUE),
              Total_yacWPA = sum(yacWPA,na.rm=TRUE),
              yacWPA_per_Att = Total_yacWPA / Attempts,
              yacWPA_per_Comp = yacWPA_Comp / Completions,
              yacWPA_Comp_per_Drive = yacWPA_Comp / Drives,
              yacWPA_Drop_per_Drive = yacWPA_Drop / Drives,
              yac_Success_Rate = length(which(yacEPA>0)) / Attempts,
              yac_Rec_Success_Rate = length(which((Reception*yacEPA)>0)) / Attempts,
              yac_Win_Success_Rate = length(which(yacWPA>0)) / Attempts,
              yac_Comp_Win_Success_Rate = length(which((Reception*yacWPA)>0)) / Attempts)
  return(pass_output)
}
calc_rushing_splits <- function(splits,pbp_df) {
  split_groups <- lapply(splits, as.symbol)
  # Filter to only rush attempts:
  pbp_df <- pbp_df %>% filter(RushAttempt == 1 & PlayType != "No Play") %>%
    mutate(GameDrive = paste(as.character(GameID), as.character(Drive),sep="-"))
  rush_output <- pbp_df %>% group_by(.dots=split_groups) %>%
    summarise(Player_Name = find_player_name(Rusher[which(!is.na(Rusher))]),
              Carries = n(),
              Drives = n_distinct(GameDrive),
              Car_per_Drive = Carries / Drives,
              Total_Yards = sum(Yards.Gained,na.rm = TRUE),
              Yards_per_Car = Total_Yards / Carries,
              Yards_per_Drive = Total_Yards / Drives,
              Fumbles = sum(Fumble,na.rm = TRUE),
              TDs = sum(Touchdown,na.rm=TRUE),
              TD_to_Fumbles = TDs / Fumbles,
              Total_EPA = sum(EPA,na.rm=TRUE),
              Success_Rate = length(which(EPA>0)) / Carries,
              EPA_per_Car = Total_EPA / Carries,
              EPA_Ratio = sum(as.numeric(EPA>0)*EPA,na.rm=TRUE)/sum(abs(EPA),na.rm=TRUE),
              TD_per_Car = TDs / Carries,
              Fumbles_per_Car = Fumbles / Carries,
              Fumbles_per_Drive = Fumbles / Drives,
              TD_Drive = TDs / Drives,
              EPA_per_Drive = Total_EPA / Drives,
              Total_WPA = sum(WPA,na.rm=TRUE),
              WPA_per_Drive = Total_WPA / Drives,
              Win_Success_Rate = length(which(WPA>0)) / Carries,
              WPA_per_Car = Total_WPA / Carries,
              WPA_Ratio = sum(as.numeric(WPA>0)*WPA,na.rm=TRUE)/sum(abs(WPA),na.rm=TRUE),
              Total_Clutch_EPA = sum(EPA*abs(WPA),na.rm=TRUE),
              Clutch_EPA_per_Car = Total_Clutch_EPA / Carries,
              Clutch_EPA_per_Drive = Total_Clutch_EPA / Drives)
  return(rush_output)
}
calc_receiving_splits <- function(splits,pbp_df) {
  split_groups <- lapply(splits, as.symbol)
  # Filter to only pass attempts:
  pbp_df <- pbp_df %>% filter(PassAttempt == 1 & PlayType != "No Play") %>%
    mutate(GameDrive = paste(as.character(GameID), as.character(Drive),sep="-"))
  rec_output <- pbp_df %>% group_by(.dots=split_groups) %>%
    summarise(Player_Name = find_player_name(Receiver[which(!is.na(Receiver))]),
              Targets = n(), Receptions = sum(Reception,na.rm=TRUE),
              Drives = n_distinct(GameDrive),
              Targets_per_Drive = Targets / Drives,
              Rec_per_Drive = Receptions / Drives,
              Total_Yards = sum(Yards.Gained,na.rm = TRUE),
              Yards_per_Drive = Total_Yards / Drives,
              Total_Raw_YAC = sum(YardsAfterCatch,na.rm=TRUE),
              Yards_per_Rec = Total_Yards / Receptions,
              Yards_per_Target = Total_Yards / Targets,
              YAC_per_Target = Total_Raw_YAC / Targets,
              Total_Caught_YAC = sum(Reception*YardsAfterCatch,na.rm=TRUE),
              Total_Dropped_YAC = sum(as.numeric(Reception==0)*YardsAfterCatch,na.rm=TRUE),
              Caught_YAC_per_Target = Total_Caught_YAC / Targets,
              Dropped_YAC_per_Target = Total_Dropped_YAC / Targets,
              YAC_per_Rec = Total_Raw_YAC / Receptions,
              Caught_YAC_per_Rec = Total_Caught_YAC / Receptions,
              Dropped_YAC_per_Rec = Total_Dropped_YAC / Receptions,
              YAC_per_Drive = Total_Raw_YAC / Drives,
              Caught_YAC_per_Drive = Total_Caught_YAC / Drives,
              Dropped_YAC_per_Drive = Total_Dropped_YAC / Drives,
              Rec_Percentage = Receptions / Targets,
              Fumbles = sum(Fumble,na.rm = TRUE),
              TDs = sum(Touchdown,na.rm=TRUE),
              TDs_per_Drive = TDs / Drives,
              Fumbles_per_Drive = Fumbles / Drives,
              AC_TDs = sum(as.numeric(YardsAfterCatch > 0)*Touchdown,na.rm=TRUE),
              AC_TDs_per_Drive = AC_TDs / Drives,
              AC_TD_Rate = AC_TDs / TDs,
              TD_to_Fumbles = TDs / Fumbles,
              Total_EPA = sum(EPA,na.rm=TRUE),
              EPA_per_Drives = Total_EPA / Drives,
              Success_Rate = length(which(EPA>0)) / Targets,
              EPA_per_Rec = sum(Reception*EPA,na.rm=TRUE) / Receptions,
              EPA_per_Target = Total_EPA / Targets,
              EPA_Rec_Perc = sum(Reception*EPA,na.rm=TRUE)/sum(abs(EPA),na.rm=TRUE),
              TD_per_Targets = TDs / Targets,
              Fumbles_per_Rec = Fumbles / Receptions,
              TD_per_Rec = TDs / Receptions,
              Total_WPA = sum(WPA,na.rm=TRUE),
              WPA_per_Drive = Total_WPA / Drives,
              Win_Success_Rate = length(which(WPA>0)) / Targets,
              WPA_per_Target = Total_WPA / Targets,
              WPA_per_Rec = sum(Reception*WPA,na.rm=TRUE) / Receptions,
              WPA_Rec_Perc = sum(Reception*WPA,na.rm=TRUE)/sum(abs(WPA),na.rm=TRUE),
              Total_Clutch_EPA = sum(EPA*abs(WPA),na.rm=TRUE),
              Clutch_EPA_per_Drive = Total_Clutch_EPA / Drives,
              Total_Raw_AirYards = sum(AirYards,na.rm=TRUE),
              PACR = Total_Yards / Total_Raw_AirYards,
              Total_Caught_AirYards = sum(Reception*AirYards, na.rm=TRUE),
              Raw_AirYards_per_Target = Total_Raw_AirYards / Targets,
              RACR = Total_Yards / Total_Raw_AirYards,
              Total_Raw_airEPA = sum(airEPA, na.rm=TRUE),
              Total_Caught_airEPA = sum(Reception*airEPA, na.rm=TRUE),
              Raw_airEPA_per_Drive = Total_Raw_airEPA / Drives,
              Caught_airEPA_per_Drive = Total_Caught_airEPA / Drives,
              airEPA_per_Target = Total_Raw_airEPA / Targets,
              Caught_airEPA_per_Target = Total_Caught_airEPA / Targets,
              epa_RACR = Total_EPA / Total_Raw_airEPA,
              Total_Raw_airWPA = sum(airWPA, na.rm=TRUE),
              Total_Caught_airWPA = sum(Reception*airWPA, na.rm=TRUE),
              Raw_airWPA_per_Drive = Total_Raw_airWPA / Drives,
              Caught_airWPA_per_Drive = Total_Caught_airWPA / Drives,
              airWPA_per_Target = Total_Raw_airWPA / Targets,
              Caught_airWPA_per_Target = Total_Caught_airWPA / Targets,
              yacEPA_Rec = sum(Reception*yacEPA,na.rm=TRUE),
              yacEPA_Drop = sum(as.numeric(Reception==0)*yacEPA,na.rm=TRUE),
              Total_yacEPA = sum(yacEPA,na.rm=TRUE),
              yacEPA_per_Target = Total_yacEPA / Targets,
              yacEPA_per_Rec = yacEPA_Rec / Receptions,
              yacEPA_Rec_per_Drive = yacEPA_Rec / Drives,
              yacEPA_Drop_per_Drive = yacEPA_Drop / Drives,
              yacWPA_Rec = sum(Reception*yacWPA,na.rm=TRUE),
              yacWPA_Drop = sum(as.numeric(Reception==0)*yacWPA,na.rm=TRUE),
              Total_yacWPA = sum(yacWPA,na.rm=TRUE),
              yacWPA_per_Target = Total_yacWPA / Targets,
              yacWPA_per_Rec = yacWPA_Rec / Receptions,
              yacWPA_Rec_per_Drive = yacWPA_Rec / Drives,
              yacWPA_Drop_per_Drive = yacWPA_Drop / Drives,
              wpa_RACR = Total_WPA / Total_Raw_airWPA,
              yac_Success_Rate = length(which(yacEPA>0)) / Targets,
              yac_Rec_Success_Rate = length(which((Reception*yacEPA)>0)) / Targets,
              air_Success_Rate = length(which(airEPA>0)) / Targets,
              air_Rec_Success_Rate = length(which((Reception*airEPA)>0)) / Targets,
              yac_Win_Success_Rate = length(which(yacWPA>0)) / Targets,
              yac_Rec_Win_Success_Rate = length(which((Reception*yacWPA)>0)) / Targets,
              air_Win_Success_Rate = length(which(airWPA>0)) / Targets,
              air_Rec_Win_Success_Rate = length(which((Reception*airWPA)>0)) / Targets)
  return(rec_output)
}

# Reading 2019 Data -------------------------------------------------------
pbp_2019 <- read_rds("~/GitHub/nflscrapR/data-scrapR/splits_data/pbp_splits_2019.rds") %>% mutate(Season=2019)

# Extract the game IDs for each season, compare to GameIDs already in pbp
game_ids_19 <- extracting_gameids(2019)
pulled_games <- pbp_2019 %>% pull(GameID) %>% unique()
missing <- setdiff(game_ids_19, pulled_games)
# missing <- missing[-15]

# Get pbp for new games
pbp_new_week <- purrr::map_dfr(missing, game_play_by_play)
pbp_2019 <- dplyr::bind_rows(pbp_2019, pbp_new_week) %>% arrange(GameID, play_id)
saveRDS(pbp_2019, "~/GitHub/nflscrapR/data-scrapR/splits_data/pbp_splits_2019.rds")
write_csv(pbp_2019, "~/GitHub/nflscrapR/data-scrapR/splits_data/pbp_2019.csv")

#New data to calculate splits
pbp_data <- readRDS("~/GitHub/nflscrapR/data-scrapR/splits_data/pbp_splits_2019.rds")%>% mutate(Season=2019)

# First generate stats at the Season level for each player,
# removing the observations with missing player names:

season_passing_df <- calc_passing_splits(c("Season","Passer_ID"), pbp_data) %>%
  filter(Passer_ID != "None") %>% arrange(Season,desc(Attempts))

season_receiving_df <- calc_receiving_splits(c("Season","Receiver_ID"), pbp_data) %>%
  filter(Receiver_ID != "None") %>% arrange(Season,desc(Targets))

season_rushing_df <- calc_rushing_splits(c("Season","Rusher_ID"), pbp_data) %>%
  filter(Rusher_ID != "None") %>% arrange(Season,desc(Carries))

# Save each file
write_csv(season_passing_df, "~/GitHub/nflscrapR/data-scrapR/splits_data/2019_season _df/2019_season_passing_df.csv")
write_csv(season_receiving_df, "~/GitHub/nflscrapR/data-scrapR/splits_data/2019_season _df/2019_season_receiving_df.csv")
write_csv(season_rushing_df, "~/GitHub/nflscrapR/data-scrapR/splits_data/2019_season _df/2019_season_rushing_df.csv")

# Season level for each team:
team_season_passing_df <- calc_passing_splits(c("Season","posteam"), pbp_data) %>%
  arrange(Season,desc(Attempts)) %>% rename(Team=posteam)

team_season_receiving_df <- calc_receiving_splits(c("Season","posteam"), pbp_data) %>%
  arrange(Season,desc(Targets)) %>% rename(Team=posteam)

team_season_rushing_df <- calc_rushing_splits(c("Season","posteam"), pbp_data) %>%
  arrange(Season,desc(Carries)) %>% rename(Team=posteam)

team_def_season_passing_df <- calc_passing_splits(c("Season","DefensiveTeam"), pbp_data) %>%
  arrange(Season,desc(Attempts)) %>% rename(Team=DefensiveTeam)

team_def_season_receiving_df <- calc_receiving_splits(c("Season","DefensiveTeam"), pbp_data) %>%
  arrange(Season,desc(Targets)) %>% rename(Team=DefensiveTeam)

team_def_season_rushing_df <- calc_rushing_splits(c("Season","DefensiveTeam"), pbp_data) %>%
  arrange(Season,desc(Carries)) %>% rename(Team=DefensiveTeam)

# Save each file
setwd("~/GitHub/nflscrapR/data-scrapR/splits_data/2019_season _df")
write_csv(team_season_passing_df, "./2019_team_season_passing_df.csv")
write_csv(team_season_receiving_df, "./2019_team_season_receiving_df.csv")
write_csv(team_season_rushing_df, "./2019_team_season_rushing_df.csv")
write_csv(team_def_season_passing_df, "./2019_team_def_season_passing_df.csv")
write_csv(team_def_season_receiving_df, "./2019_team_def_season_receiving_df.csv")
write_csv(team_def_season_rushing_df, "./2019_team_def_season_rushing_df.csv")

# Game level:

game_passing_df <- calc_passing_splits(c("GameID","Passer_ID","posteam","DefensiveTeam"), pbp_data) %>%
  filter(Passer_ID != "None") %>% arrange(GameID,desc(Attempts)) %>% rename(Team=posteam,
                                                                            Opponent=DefensiveTeam)
game_receiving_df <- calc_receiving_splits(c("GameID","Receiver_ID","posteam","DefensiveTeam"), pbp_data) %>%
  filter(Receiver_ID != "None") %>% arrange(GameID,desc(Targets))  %>% rename(Team=posteam,
                                                                              Opponent=DefensiveTeam)
game_rushing_df <- calc_rushing_splits(c("GameID","Rusher_ID","posteam","DefensiveTeam"), pbp_data) %>%
  filter(Rusher_ID != "None") %>% arrange(GameID,desc(Carries))  %>% rename(Team=posteam,
                                                                            Opponent=DefensiveTeam)
# Save each file
write_csv(game_passing_df, "./2019_game_passing_df.csv")
write_csv(game_receiving_df, "./2019_game_receiving_df.csv")
write_csv(game_rushing_df, "./2019_game_rushing_df.csv")


# DFS DataFrames ----------------------------------------------------------
#SkillplAyers
setwd("~/GitHub/nflscrapR/data-scrapR/splits_data/2019_season _df")
#Import Receiver Season and Team Seaon Data Frames
player_2019_receiving_df <- read_csv("~/GitHub/nflscrapR/data-scrapR/splits_data/2019_season _df/2019_season_receiving_df.csv") %>% 
  select(
    Season, Receiver_ID, Player_Name, Targets, Receptions, TDs, Total_Yards, Total_Raw_AirYards, Total_Raw_YAC, Total_Caught_AirYards, Raw_AirYards_per_Target, YAC_per_Target, Fumbles, PACR, RACR, Total_EPA, Total_WPA, Total_Raw_airWPA, air_Success_Rate)

roster <- read_csv("~/GitHub/nflscrapR/data-scrapR/roster_data/regular_season/reg_roster_2019.csv") %>% select(full_player_name, team, position, gsis_id)

player_2019_receiving_df <- player_2019_receiving_df %>%  left_join(roster, by = c( "Receiver_ID" = "gsis_id"))

team_2019_receiving_df <- read_csv("./2019_team_season_receiving_df.csv") %>% select(Team, Targets, Total_Yards, Total_Raw_AirYards, Total_Caught_AirYards)

skill_df_joined <- left_join(player_2019_receiving_df, team_2019_receiving_df, by = c("team" = "Team")) %>% 
  mutate(
    MS_Tar = Targets.x / Targets.y,
    MS_AirYards = Total_Caught_AirYards.x /Total_Raw_AirYards.y,
    wOPR = 1.5 * MS_Tar + 0.7 * MS_AirYards
  )

dfs_receivers <- skill_df_joined %>% 
  select(
    Receiver_ID, full_player_name, position, Targets=Targets.x, MS_Tar, Receptions, Recyds=Total_Yards.x, AirYards=Total_Raw_AirYards.x,  MS_AirYards, YAC=Total_Raw_YAC, compAirYards=Total_Caught_AirYards.x, TDs, aDOT=Raw_AirYards_per_Target, PACR, RACR, EPA=Total_EPA, WPA=Total_WPA, air_Success_Rate, wOPR)

write_csv(dfs_receivers, "~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/dfs_receiver.csv")

#Quarterbacks
#Import QB Season and Team Season Data Frames
player_2019_passing_df <- read_csv("~/GitHub/nflscrapR/data-scrapR/splits_data/2019_season _df/2019_season_passing_df.csv") %>% 
  select(
    Season, Passer_ID, Player_Name, Completions, Attempts, TDs, Air_TDs, Ints=Interceptions, Total_Yards, Total_Raw_AirYards, aDot=Raw_AirYards_per_Att, compAirYards=Total_Comp_AirYards, PACR, aPACR, Total_EPA, Total_WPA
  )

roster <- read_csv("~/GitHub/nflscrapR/data-scrapR/roster_data/regular_season/reg_roster_2019.csv") %>% select(full_player_name, team, position, gsis_id)

player_2019_passing_df <- player_2019_passing_df %>%  left_join(roster, by = c( "Passer_ID" = "gsis_id"))

qbs_df_joined <- left_join(player_2019_passing_df, sacks_df, by = c("full_player_name" = "Player_Name")) %>% 
  mutate(
    AYA = (Total_Yards + 10*(Air_TDs) - 45*(Ints)) / Attempts,
    ANYA = (Total_Yards+10*(Air_TDs)-45*(Ints)-SackYds) / (Attempts + Sacks))

dfs_qbs <- qbs_df_joined %>% 
  select(
    Passer_ID, full_player_name, position, Completions, Attempts, TDs, Air_TDs, Ints, Total_Yards, Total_Raw_AirYards, aDot, compAirYards, PACR, aPACR, AYA, ANYA, Total_EPA, Total_WPA)

write_csv(dfs_qbs, "~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/dfs_qbs.csv")