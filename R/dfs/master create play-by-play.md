Run this code, making sure all the packages are installed (```install.packages("package")``` if you don't).

Make sure to replace the instances of FILENAME where you want to save your data.

``` r
library(tidyverse)
library(dplyr)
library(na.tools)

first <- 2009 #first season to grab. min available=2009
last <- 2018 # most recent season

datalist = list()
for (yr in first:last) {
  pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
  pbp <- pbp %>% inner_join(games %>% distinct(game_id, week, season)) %>% select(-fumble_recovery_2_yards)
  datalist[[yr]] <- pbp # add it to your list
}

pbp_all <- dplyr::bind_rows(datalist)

pbp_all %>% group_by(home_team) %>%summarize(n=n(), seasons=n_distinct(season), minyr=min(season), maxyr=max(season)) %>% 
  arrange(seasons)

pbp_all <- pbp_all %>% 
  mutate_at(vars(home_team, away_team, posteam, defteam), funs(case_when(
    . %in% "JAX" ~ "JAC",
    . %in% "STL" ~ "LA",
    . %in% "SD" ~ "LAC",
    TRUE ~ .
  ))) 

#save the whole big thing in case you need it later
saveRDS(pbp_all, file="FILENAME_ALL.rds")
pbp_all <- readRDS("FILENAME_ALL.rds")

pbp_all_rp <- pbp_all %>%
  filter(!is_na(epa), !is_na(posteam), play_type=="no_play" | play_type=="pass" | play_type=="run") %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0),
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, 
                                              "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    yards_gained=ifelse(play_type=="no_play",NA,yards_gained),
    play=1
  ) %>%
  filter(pass==1 | rush==1)

saveRDS(pbp_all_rp, file="FILENAME_RP.rds") #save
pbp_all_rp<- readRDS("FILENAME_RP.rds") #how to load it later


#roster data
datalist = list()
for (yr in first:last) {
  if (yr<=2017) {
    roster <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/legacy_data/team_rosters/team_", yr, "_rosters.csv")))
    roster <- roster %>% mutate(
      season = Season,
      full_player_name = Player,
      abbr_player_name = name,
      position = Pos,
      team = Team,
      gsis_id = GSIS_ID
      ) %>%
      select(season,full_player_name,abbr_player_name,position,team,gsis_id)
    }
  else {
    roster <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_", yr, ".csv")))
    roster <- roster %>% select(-season_type)
    }

  datalist[[yr]] <- roster # add it to your list
}

rosters_all <- dplyr::bind_rows(datalist)

#fix the team name problems
rosters_all <- rosters_all %>% 
  mutate_at(vars(team), funs(case_when(
    . %in% "JAX" ~ "JAC",
    . %in% "STL" ~ "LA",
    . %in% "SD" ~ "LAC",
    TRUE ~ .
  ))) 

#save raw dataset
saveRDS(rosters_all, file="FILENAME.rds")
```