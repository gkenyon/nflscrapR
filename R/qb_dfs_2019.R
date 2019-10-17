nflpackages <- c('devtools', 'nflscrapR', 'XML', 'bitops', 'RCurl', 'ggplot2', 'nnet', 'magrittr', 'bindrcpp', 'tidyverse', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr', 'ggjoy', 'na.tools', 'httr')
lapply(nflpackages, require, character.only = TRUE)
#read in our csvs #####
#Import QB Season and Team Season Data Frames
player_2019_passing_df <- read_csv("~/GitHub/nflscrapR/data-scrapR/splits_data/2019_season _df/2019_season_passing_df.csv") %>% 
  select(
    Season, Passer_ID, Player_Name, Completions, Attempts, TDs, Air_TDs, Ints=Interceptions, Total_Yards, Total_Raw_AirYards, aDot=Raw_AirYards_per_Att, compAirYards=Total_Comp_AirYards, PACR, aPACR, Total_EPA, Total_WPA
    )

roster <- read_csv("~/GitHub/nflscrapR/data-scrapR/roster_data/regular_season/reg_roster_2019.csv") %>% select(full_player_name, team, position, gsis_id)

player_2019_passing_df <- player_2019_passing_df %>%  left_join(roster, by = c( "Passer_ID" = "gsis_id"))

# Get data from URL via HTTR ----------------------------------------------
library(httr)
url <- "https://www.pro-football-reference.com/years/2019/passing.htm"
urldata <- GET(url)
data <- readHTMLTable(rawToChar(urldata$content),
                      stringsAsFactors = FALSE)
passing_df <- data$passing 
passing_cols <- c(2,3,5,24,25)
sacks_df <-  passing_df[passing_cols] %>% filter(Pos=="QB") %>% select(
  Player_Name = Player, Sacks = Sk, SackYds = Yds)
sacks_df$Sacks <- as.numeric(as.character(sacks_df$Sacks))
sacks_df$SackYds <- as.numeric(as.character(sacks_df$SackYds))

# Join Sacks with Passing DF ----------------------------------------------

qbs_df_joined <- left_join(player_2019_passing_df, sacks_df, by = c("full_player_name" = "Player_Name")) %>% 
  mutate(
    AYA = (Total_Yards + 10*(Air_TDs) - 45*(Ints)) / Attempts,
    ANYA = (Total_Yards+10*(Air_TDs)-45*(Ints)-SackYds) / (Attempts + Sacks))

dfs_qbs <- skill_df_joined %>% 
  select(
    Passer_ID, full_player_name, position, Completions, Attempts, TDs, Air_TDs, Ints, Total_Yards, Total_Raw_AirYards, aDot, compAirYards, PACR, aPACR, AYA, ANYA, Total_EPA, Total_WPA)

write_csv(dfs_qbs, "~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/dfs_qbs.csv")
dfs_qbs <- read_csv("~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/dfs_qbs.csv")

# Join with Number Fire ---------------------------------------------------

dfs_receiver <- left_join(dfs_receiver, NF_recs, by=c("full_player_name" = "player"))
dfs_receiver <- left_join(dfs_receiver, dvoa_def, by=c("opp_team" = "TEAM"))
write_csv(dfs_receiver, "~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/dfs_qbs.csv")

# Join DVOA ---------------------------------------------------------------

dfs_qbs <- left_join(dfs_qbs, NF_qbs, by=c("full_player_name" = "player"))
url <- "https://www.footballoutsiders.com/stats/teamdef/2019"
urldata <- GET(url)
data <- readHTMLTable(rawToChar(urldata$content),
                      stringsAsFactors = FALSE)
def_df <- data[["NULL"]] 
def_cols <- c(2,3,7,9,11,12,13)
dvoa_df <-  def_df[def_cols]
write_csv(dvoa_df, "~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/dvoa_def.csv")
dvoa_def <- read_csv("~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/dvoa_def.csv",
col_types = cols(DEFENSEDVOA = col_number(),
`NON-ADJPASS` = col_number(), `NON-ADJRUSH` = col_number(),
`NON-ADJTOTAL` = col_number(), PASSDEF = col_number(),
RUSHDEF = col_number()))

dfs_qbs <- left_join(dfs_qbs, dvoa_def, by=c("opp_team" = "TEAM"))
write_csv(dfs_qbs, "~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/dfs_qbs.csv")

# Advanced Stats Glossary -------------------------------------------------

# MS_Tar = Targets/Atttempts
# MS_AY= Total_Caught_AirYards/Total_Raw_AirYards
# wOPR = 1.5 × Target Market Share + 0.7 × Air Yards Market Share
# AYA (Adjusted Yards per Attempt)
# AYA=passingyards+(20×TDs)−(45×INTs)/attempts
# ANYA (Adjusted Net Yards per Attempt)
# ANYA=passingyards+(20×passingTDs)−(45×INTs)−sackyards/attempts+sacks
# PACR (Passing Air Conversion Ratio)
# PACR=completedairyards+yardsafterthecatch/airyards
# PACR=passingyards/airyards
# PACR=YPA/aDOT
# Adjusted PACR (aPACR). If we add TDs and INTs to the PACR calculation with the multipliers suggested in the AYA formula we get Adjusted PACR (aPACR).
# aPACR=passingyards+(20×TD)−(45×INT)/airyards

# Speed Score – Bill Barnwell first posited the metric in Pro Football Prospectus to better predict running back success. The formula is (weight*200) / (40-time^4). It factors weight into a player’s 40-yard dash time assigning a premium to fast times run by bigger, often stronger, running backs.

#For wide receivers and tight ends, air yards are total completed receiving yards from the line of scrimmage to the catch point. Air yards are also referred to as “completed air yards” by Josh Hermsmeyer in his introduction to the predictive qualities of air yards on RotoViz.com and his AirYards.com property. On PlayerProfiler, air yards differs from total pass attempt distance and total target distance, which aggregates all completed and incomplete air yards.