nflpackages <- c('devtools', 'nflscrapR', 'XML', 'bitops', 'RCurl', 'ggplot2', 'nnet', 'magrittr', 'bindrcpp', 'tidyverse', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr', 'ggjoy', 'na.tools')
lapply(nflpackages, require, character.only = TRUE)
#read in our csvs #####
# app_path = ''
pbp = read_csv("~/GitHub/nfl-dashboard-dfs/appdata/pbp.csv")
gbg = read_csv("~/GitHub/nfl-dashboard-dfs/appdata/gbg.csv")
rosters = read_csv("~/GitHub/nfl-dashboard-dfs/appdata/rosters.csv")
pbp_df = pbp%>% mutate(year=2019)
gbg_df = gbg
rosters_df = rosters

pbp_df = gbg_df %>% 
  right_join(pbp_df, by = c("game_id","year","home_team","away_team"))
# saveRDS(pbp_df, "pbp_df.rds")

#adjust the pbp_df for use with shiny ######
pbp_df_adj = pbp_df %>% 
  mutate(
    qb_scramble = ifelse(qb_scramble==1&play_type=="pass",0,qb_scramble),
    play_type2 = as.factor(ifelse(qb_scramble==1|play_type=="pass", "dropback",play_type)),
    away_team_score = ifelse(posteam == home_team, defteam_score, posteam_score),
    home_team_score = ifelse(posteam == home_team, posteam_score, defteam_score),
    opp_yds = ifelse(air_yards < 0|is.na(air_yards), 0, air_yards),
    adj_opp_yds = ifelse(opp_yds == 0, 1, opp_yds),
    gamewk = as.numeric(paste0(year,ifelse(week>=10,week,paste0(0,week)))),
    wk_ovr_rk = dense_rank(gamewk),
    #combine Rusher_ID and Receiver_ID variable into a skillplayer ID, 'skill_ID'
    skill_ID = ifelse(play_type2=="run",rusher_player_id,ifelse(
      play_type2=="dropback",receiver_player_id,NA)),
    #combine Passer_ID and QB-scramble Rusher_ID into a dropboack_ID, 'dropback_ID'
    dropback_ID = ifelse(qb_dropback==1, ifelse(
      qb_scramble==1,skill_ID,passer_player_id),"None"),
    matchup = paste0(year, " Wk ", week, " ", away_team, " @ ", home_team)
  )%>% 
  filter(play_type2 != "no_play" & is.na(play_type2)==F & is.na(away_wp)==F) %>%
  filter(
    !is.na(home_wp),
    !is.na(away_wp),
    timeout == 0)

rosters_df = rosters_df %>%
  mutate(player_info = paste0(player_info = paste0(full_player_name,", ", position))) %>%
  mutate(Player=full_player_name) %>% 
  write_csv("~/GitHub/nfl-dashboard-dfs/appdata/rosters_df.csv")
team_agg_df = pbp_df_adj %>% 
  filter(
    !is.na(home_wp),
    !is.na(away_wp),
    timeout == 0) %>%
  group_by(game_id, posteam) %>%
  summarize(
    team_dropbacks = sum(qb_dropback, na.rm = T),
    team_pass_attempts = sum(pass_attempt, na.rm = T),
    team_rush_attempts = sum(rush_attempt, na.rm = T),
    team_opportunities = team_pass_attempts+team_rush_attempts,
    team_air_yards = sum(opp_yds, na.rm = T),
    team_total_yards = sum(yards_gained, na.rm = T)
  )
skill_df <- pbp_df_adj %>%
  filter(
    skill_ID != "None",
    qb_scramble == 0,
    !is.na(home_wp),
    !is.na(away_wp),
    timeout == 0) %>%
  transmute(
    desc = desc,
    game_id = game_id,
    year = year,
    posteam = posteam,
    target = pass_attempt,
    rush = rush_attempt,
    matchup = matchup,
    yards_gained= yards_gained,
    opp_yds = opp_yds,
    wk_ovr_rk = wk_ovr_rk,
    wks_back = 1 + max_wk - wk_ovr_rk,
    skill_ID = skill_ID,
    aracr = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      adj_opp_yds,
    anya = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      ifelse(pass_attempt+rush_attempt==0,1,pass_attempt+rush_attempt),
    racr = yards_gained/adj_opp_yds,
    epa = epa,
    wpa = wpa
  ) %>% left_join(rosters_df, by = c("skill_ID" = "gsis_id")) %>%
  mutate(Pos_grp = ifelse(position == "QB", "QB", ifelse(position %in% c("RB","FB"), "RB", "REC"))) %>%
  left_join(team_agg_df, by = c("game_id", "posteam")) %>%
  group_by(skill_ID, game_id) %>%
  mutate(
    ms_team_rushes = round(sum(rush, na.rm=T)/mean(team_rush_attempts),3),
    ms_team_opportunities = round((sum(target, na.rm=T)+sum(rush, na.rm=T))/mean(team_opportunities),3),
    ms_team_targets = round(sum(target)/mean(team_pass_attempts),3),
    ms_team_air_yards = round(sum(opp_yds)/mean(team_air_yards),3),
    wopr = round(1/2.2 * (1.5 * ms_team_targets + 0.7 * ms_team_targets),3),
    awopr = round(1/2.2 * (1.5 * ms_team_opportunities + 0.7 * ms_team_targets),3)
  ) %>% mutate(Player=full_player_name) %>% 
  write_csv("./skill_df.csv")


#text explantaions of the included advanced metrics with links for more details #####
# epa_detail = "EPA (Expected Points Added) measures the expected scoring each play will produce. Learn more " 'http://www.stat.cmu.edu/~ryurko/talk/glsac/'
# wpa_detail = "WPA (Win Probability Added) measures the expected change in the game's outcome each play will produce. There are at least six prominent NFL win probability models in deployment. Learn more "https://statsbylopez.com/2017/03/08/all-win-probability-models-are-wrong-some-are-useful/comment-page-1/"
#MS Trg— Market Share of team targets. The ratio of a player's targets relative to the       entire team.
#MS Air— Market Share of team air yards. The ratio of a player's air yards relative to the   entire team. = Total_Caught_AirYards/Total_Raw_AirYards
# WOPR- "Weighted Opportunity Rating (WOPR) is a weighted combination of a player's share of team targets and share of team air yards. WOPR = 1.5 × Target Market Share + 0.7 × Air Yards Market Share
#MS WOPR— Market Share of WOPR (Weighted Opportunity Ratio. Defined by Josh Hermsmeyer as 1.5 * MS Trg + 0.7 
# Speed Score – Bill Barnwell first posited the metric in Pro Football Prospectus to better predict running back success. The formula is (weight*200) / (40-time^4). It factors weight into a player’s 40-yard dash time assigning a premium to fast times run by bigger, often stronger, running backs.
# AYA (Adjusted Yards per Attempt)
#passingyards+(20×TDs)−(45×INTs)/attempts
# ANYA (Adjusted Net Yards per Attempt)
#passingyards+(20×passingTDs)−(45×INTs)−sackyards/attempts+sacks
# PACR (Passing Air Conversion Ratio)
#completedairyards+yardsafterthecatch/airyards
#passingyards/airyards
#YPA/aDOT
# Adjusted PACR (aPACR). If we add TDs and INTs to the PACR calculation with the multipliers suggested in the AYA formula we get Adjusted PACR (aPACR).
#passingyards+(20×TD)−(45×INT)/airyards


#For wide receivers and tight ends, air yards are total completed receiving yards from the line of scrimmage to the catch point. Air yards are also referred to as “completed air yards” by Josh Hermsmeyer in his introduction to the predictive qualities of air yards on RotoViz.com and his AirYards.com property. On PlayerProfiler, air yards differs from total pass attempt distance and total target distance, which aggregates all completed and incomplete air yards.