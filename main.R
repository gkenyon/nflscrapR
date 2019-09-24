nflpackages <- c('devtools', 'nflscrapR', 'XML', 'bitops', 'RCurl', 'ggplot2', 'nnet', 'magrittr', 'bindrcpp', 'tidyverse', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr', 'ggjoy', 'na.tools')
lapply(nflpackages, require, character.only = TRUE)

#write_csv works best

setwd("~/GitHub/nflscrapR/data/RAW")

# Update 2019 season as it progresses:
reg_pbp_19 <- read_csv("~/GitHub/nflscrapR/data/RAW/reg_pbp_2019.csv")

# Latest week - just modify the week number:
new_week_pbp_19 <- scrape_season_play_by_play(2019, type = "reg", weeks = 3)

#Write CSV as latest_week
write_csv(new_week_pbp_19,"~/GitHub/nflscrapR/data/RAW/pbp_wk3.csv")
write_csv(new_week_pbp_19,"~/GitHub/nflscrapR/data/RAW/new_week_pbp_2019.csv")
new_week_pbp_19 <- read_csv("~/GitHub/nflscrapR/data/RAW/new_week_pbp_2019.csv")

# Append to the data and save:
reg_pbp_19 <- dplyr::bind_rows(reg_pbp_19, new_week_pbp_19)
write_csv(reg_pbp_19, "~/GitHub/nflscrapR/data/RAW/reg_pbp_2019.csv")
write_csv(reg_pbp_19, "~/GitHub/nflscrapR/data-scrapR/pbp_2019.csv")

#Keep only Runs and Passes for 2019
pbp <- read_csv("reg_pbp_2019.csv")
pbp_rp <- pbp %>% 
    filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
      mutate(
      pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
      rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
      success = ifelse(epa>0, 1 , 0)
      )
pbp_rp_19 <- pbp_rp %>% filter(pass==1 | rush==1)
write_csv(pbp_rp, "~/Github/nflscrapR/data/raw/2019_pbp_rp.csv")

#Read in old data, join and save.
pbp_all_rp <- readRDS("~/GitHub/nflscrapR/data-scrapR/pbp_all_rp.rds")
reg_pbp_all_rp <- dplyr::bind_rows(pbp_all_rp, pbp_rp_19)
saveRDS(reg_pbp_all_rp, "~/Github/nflscrapR/data-scrapR/reg_pbp_all_rp.rds")

#Pulling together multiple seasons

# first <- 2010 #first season to grab. min available=2009
# last <- 2019 # most recent season
# 
# datalist = list()
# for (yr in first:last) {
#   pbp <- read_csv(paste0("~/GitHub/nflscrapR/scrapR-data/play_by_play_data/regular_season/reg_pbp_", yr, ".csv"))
#   games <- read_csv(paste0("~/GitHub/nflscrapR/scrapR-data/games_data/regular_season/reg_games_", yr, ".csv"))
#   pbp <- pbp %>% inner_join(games %>% distinct(game_id, week, season)) %>% select(-fumble_recovery_2_yards, -blocked_player_id)
#   datalist[[yr]] <- pbp # add it to your list
# }
# 
# pbp_all <- bind_rows(datalist)
# write_csv(pbp_all, "~/Github/nflscrapR/data/RAW/pbp_all.csv")
# saveRDS(pbp_all, file="~/Github/nflscrapR/data/RAW/pbp_all.rds")
# 
# 
# #aDOT
# pbp_aDOT <- pbp_all_rp %>%
#   +   filter(!is.na(receiver_player_name) & play_type == "pass" & down <= 4) %>%
#   +   group_by(receiver_player_name) %>%
#   +   summarize(adot = mean(air_yards), targets = n(), catch_rate = sum(complete_pass)/targets) %>%
#   +   filter(adot >= 5 & targets >= 40)