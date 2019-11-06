source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")
nflpackages <- c('devtools', 'nflscrapR', 'XML', 'bitops', 'RCurl', 'ggplot2', 'nnet', 'magrittr', 'bindrcpp', 'tidyverse', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr', 'ggjoy', 'na.tools')
lapply(nflpackages, require, character.only = TRUE)

#write_csv works best
setwd("~/GitHub/nflscrapR/data-scrapR/raw")

# Update 2019 season as it progresses:
reg_pbp_19 <- readRDS("~/GitHub/nflscrapR/data-scrapR/raw/reg_pbp_2019.rds")

# SCRAPE JSON PBP by GAMEID -----------------------------------------------
games <- read_csv("http://www.habitatring.com/games.csv") %>% 
filter(season == 2019 & !is.na(result)) %>% 
  mutate(game_id=as.character(game_id))

{
  # we have data! identify any missing games
  pulled_games <- reg_pbp_19 %>% pull(game_id) %>% unique()
  missing <- games %>% filter(!(game_id %in% pulled_games)) %>% pull(game_id)
  
  # handle missing games
  if (length(missing) > 0)
  {
    # get new plays
    new_plays <- NULL
    for (g in missing)
    {
      report(paste0("Scraping plays from game: ",g))
      game_plays <- scrape_json_play_by_play(g)
      game_plays <- game_plays %>%
        fix_inconsistent_data_types() %>% 
        fix_team_abbreviations()
      new_plays <- bind_rows(new_plays,game_plays)
    }
    
    # apply game data to new plays
    report("Adding in game data for new plays")
    new_plays <- new_plays %>%
      apply_game_data()
    write_csv(new_plays,"~/GitHub/nflscrapR/data-scrapR/raw/pbp_wk8.csv")
    
    # finally merge things together
    report("Merging existing plays and new plays")
    # reg_pbp_19 <- fix_inconsistent_data_types(reg_pbp_19)
    reg_pbp_19 <- bind_rows(reg_pbp_19,new_plays) %>% arrange(game_id,play_id)
    saveRDS(reg_pbp_19, "~/GitHub/nflscrapR/data-scrapR/raw/reg_pbp_2019.rds")
    rm(new_plays)  # no need for this to take up memory anymore
    rm(game_plays)  # no need for this to take up memory anymore
    
    
  }
}

# Latest week - MODIFY WEEK:
new_week_pbp_19 <- purrr::map_dfr(missing, scrape_json_play_by_play)
write_csv(new_week_pbp_19,"~/GitHub/nflscrapR/data-scrapR/raw/pbp_wk8.csv")

#Write CSV as latest_week
write_csv(new_week_pbp_19,"~/GitHub/nflscrapR/data-scrapR/raw/new_week_pbp_2019.csv")
new_week_pbp_19 <- read_csv("~/GitHub/nflscrapR/data-scrapR/raw/new_week_pbp_2019.csv")

# Append to the data and save:
reg_pbp_19 <- dplyr::bind_rows(reg_pbp_19, new_week_pbp_19)
write_csv(reg_pbp_19, "~/GitHub/nflscrapR/data-scrapR/raw/reg_pbp_2019.csv")
write_csv(reg_pbp_19, "~/GitHub/nflscrapR/data-scrapR/pbp_2019.csv")
saveRDS(reg_pbp_19,"~/GitHub/nflscrapR/data-scrapR/pbp_2019.rds")





