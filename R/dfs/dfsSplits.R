#Import Receiver Season and Team Seaon Data Frames
player_2019_receiving_df <- read_csv("~/GitHub/nflscrapR/data-scrapR/splits_data/2019_season _df/2019_season_receiving_df.csv") %>% 
  select(
    Season, Receiver_ID, Player_Name, Targets, Receptions, TDs, Total_Yards, Total_Raw_AirYards, Total_Raw_YAC, Total_Caught_AirYards, Raw_AirYards_per_Target, YAC_per_Target, Fumbles, PACR, RACR, Total_EPA, Total_WPA, Total_Raw_airWPA, air_Success_Rate)

team_2019_receiving_df <- read_csv("~/GitHub/nflscrapR/data-scrapR/splits_data/2019_season _df/2019_team_season_receiving_df.csv")

player_2019_receiving <- player_2019_receiving_df %>% 
  mutate(
aDOT = player_2019_receiving_df$Total_Raw_AirYards / player_2019_receiving_df$Targets,
MS_tar = player_2019_receiving_df$Targets / team_2019_receiving_df$Targets,
MS_AirYards = player_2019_receiving_df$Total_Caught_AirYards / team_2019_receiving_df$Total_Raw_AirYards,
wOPR = (1.5 * MS_tar) + (0.7 * MS_AirYards)
)

