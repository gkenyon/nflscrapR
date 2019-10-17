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

# Join with Number Fire ---------------------------------------------------

dfs_receiver <- left_join(dfs_receiver, NF_recs, by=c("full_player_name" = "player"))
dfs_receiver <- left_join(dfs_receiver, dvoa_def, by=c("opp_team" = "TEAM"))
write_csv(dfs_receiver, "~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/dfs_qbs.csv")
