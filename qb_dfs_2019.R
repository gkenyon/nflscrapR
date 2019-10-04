nflpackages <- c('devtools', 'nflscrapR', 'XML', 'bitops', 'RCurl', 'ggplot2', 'nnet', 'magrittr', 'bindrcpp', 'tidyverse', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr', 'ggjoy', 'na.tools')
lapply(nflpackages, require, character.only = TRUE)
#read in our csvs #####
# app_path = ''
pbp = read_csv("~/GitHub/nfl-dashboard-dfs/appdata/pbp.csv")
gbg = read_csv("~/GitHub/nfl-dashboard-dfs/appdata/gbg.csv")
rosters = read_csv("~/GitHub/nfl-dashboard-dfs/appdata/rosters.csv")
pbp_df = pbp %>% mutate(year=2019)
gbg_df = gbg
rosters_df = rosters
min_year = min(pbp_df$year,na.rm =T)

pbp_df = gbg_df %>% 
  select(-c(state_of_game, game_url)) %>% 
  right_join(pbp_df, by = c("game_id","year","home_team","away_team"))
# saveRDS(pbp_df, "pbp_df.rds")

rosters_df = rosters_df %>%
  mutate(player_info = paste0(player_info = paste0(full_player_name,", ", position))) %>%
  mutate(Player=full_player_name)
#write_csv("~/GitHub/nfl-dashboard-dfs/appdata/rosters_df.csv")

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
max_wk = max(pbp_df_adj$wk_ovr_rk, na.rm = T)

qb_df <- pbp_df_adj %>% 
  filter(
    dropback_ID != "None",
    !is.na(home_wp),
    !is.na(away_wp),
    timeout == 0) %>% 
  transmute(
    desc = desc,
    qb_dropback = qb_dropback,
    matchup = matchup,
    year = year,
    wk_ovr_rk = wk_ovr_rk,
    wks_back = 1 + max_wk - wk_ovr_rk,
    qb_dropback = qb_dropback,
    yards_gained, opp_yds,
    dropback_ID = dropback_ID,
    apacr = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      adj_opp_yds,
    anya = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      ifelse(pass_attempt+rush_attempt==0,1,pass_attempt+rush_attempt),
    pacr = yards_gained/adj_opp_yds,
    epa = epa,
    wpa = wpa
  ) %>% left_join(rosters_df, by = c("year", "dropback_ID" = "GSIS_ID"))

qb_df <- qb_df %>% 
 select(Player, attempts, completions, air_yards, comp_air_yards, pass_yards, adot, apacr, anya, pacr,epa, wpa)


# REACTIVE ----------------------------------------------------------------


qb_reactive = reactive({
  qb_df = qb_df %>% 
    filter( !(player_info %in%  input$qb_filter)) %>%
    group_by(dropback_ID) %>% mutate(
      Player = Player, 
      total_dropbacks = sum(qb_dropback),
      apacr = mean(apacr, na.rm =T),
      pacr = mean(pacr, na.rm =T),
      epa = mean(epa, na.rm =T),
      wpa = mean(wpa, na.rm =T),
      anya = mean(anya, na.rm =T),
      yards_gained = sum(yards_gained)
    ) %>% ungroup()  %>%
    filter(total_dropbacks >= max(total_dropbacks)*.55) %>%
    transmute(
      matchup = matchup,
      total_dropbacks = total_dropbacks,
      Player = Player,
      total_yards = yards_gained,
      total_yards_rk = dense_rank(-total_yards),
      apacr = apacr,
      apacr_rk = dense_rank(-apacr),
      pacr = pacr,
      pacr_rk = dense_rank(-pacr),
      epa = epa,
      epa_rk = dense_rank(-epa),
      wpa = wpa,
      wpa_rk = dense_rank(-wpa),
      anya = anya,
      anya_rk = dense_rank(-anya)
    ) 
})
#text explantaions of the included advanced metrics with links for more details #####
# epa_detail = "EPA (Expected Points Added) measures the expected scoring each play will produce. Learn more " 'http://www.stat.cmu.edu/~ryurko/talk/glsac/'
# wpa_detail = "WPA (Win Probability Added) measures the expected change in the game's outcome each play will produce. There are at least six prominent NFL win probability models in deployment. Learn more "https://statsbylopez.com/2017/03/08/all-win-probability-models-are-wrong-some-are-useful/comment-page-1/"
# pacr_detail = "PACR (Passing Air Conversion Ratio) is an efficiency metric that measures how often a yard thrown in the air is converted into receiving yardage on the field.It combines signals from both catch rate (Catch %) and yards after the catch (YAC). Learn more "http://airyards.com/the_league.html#MathJax-Span-342"
# racr_detail = "RACR (Receiver Air Conversion Ratio) is an efficiency metric that measures how often a yard thrown in the air is converted into receiving yardage on the field.\n
# It combines signals from both catch rate (Catch %) and yards after the catch (YAC).\n
# In this graph, rushes are included as 0 yard passes. Learn more "
# racr_link = "http://airyards.com/the_league.html#MathJax-Span-342"
# apacr_detail = "aPACR is a variant of PACR that adds TDs and INTs to the PACR calculation,\n
# with the multipliers suggested by the AYA formula outlined in 'The Hidden Game of Football'. Learn more "
# apacr_link = "http://airyards.com/the_league.html#MathJax-Span-521"
# aracr_detail = "aRACR is a variant of RACR that adds TDs and INTs to the RACR calculation,\n
# with the multipliers suggested by the AYA formula outlined in 'The Hidden Game of Football'. \n
# In this graph, rushes are included as 0 yard passes. Learn more "
# aracr_link = "http://airyards.com/the_league.html#MathJax-Span-521"
# anya_detail = "ANYA (Adjusted Net Yards per Attempt, or AYA) adds multipliers for interceptions and touchdowns to the simple Yards Gained per Opportunity metric,\n
# as outlined by 'The Hidden Game of Football' by Bob Carroll, Pete Palmer, and John Thorn"
# anya_link = "https://www.pro-football-reference.com/about/glossary.htm#ay/a"
# wopr_detail = "Weighted Opportunity Rating (WOPR) is a weighted combination of a player's share of team targets and share of team air yards.\n More info "
# wopr_link = "https://www.4for4.com/fantasy-football/2018/preseason/air-yards-identify-wr-best-ball-values"
# awopr_detail = "Adjusted/Weighted WOPR (aWOPR) simply includes rushes as 0-yard passes in the WOPR calculation. More info "
# awopr_link = "https://www.4for4.com/fantasy-football/2018/preseason/air-yards-identify-wr-best-ball-values"

#market share of targets, market share of air yards, weighted opportunity ranking, ms_wopr, adot

#tARGET ms = Targets/Atttempts
#AY MS = Total_Caught_AirYards/Total_Raw_AirYards
# WOPR = 1.5 × Target Market Share + 0.7 × Air Yards Market Share

# Speed Score – Bill Barnwell first posited the metric in Pro Football Prospectus to better predict running back success. The formula is (weight*200) / (40-time^4). It factors weight into a player’s 40-yard dash time assigning a premium to fast times run by bigger, often stronger, running backs.

#For wide receivers and tight ends, air yards are total completed receiving yards from the line of scrimmage to the catch point. Air yards are also referred to as “completed air yards” by Josh Hermsmeyer in his introduction to the predictive qualities of air yards on RotoViz.com and his AirYards.com property. On PlayerProfiler, air yards differs from total pass attempt distance and total target distance, which aggregates all completed and incomplete air yards.
# AYA (Adjusted Yards per Attempt)
# AYA=passingyards+(20×TDs)−(45×INTs)/attempts
# ANYA (Adjusted Net Yards per Attempt)
# ANYA=passingyards+(20×passingTDs)−(45×INTs)−sackyards/attempts+sacks
# PACR (Passing Air Conversion Ratio)
# PACR=completedairyards+yardsafterthecatch/airyards
# PACR=passingyards/airyards
# PACR=YPA/aDOT
# Adjusted PACR (aPACR).
# If we add TDs and INTs to the PACR calculation with the multipliers suggested in the AYA formula we get Adjusted PACR (aPACR).
# aPACR=passingyards+(20×TD)−(45×INT)/airyards