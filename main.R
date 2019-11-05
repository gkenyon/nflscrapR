nflpackages <- c('devtools', 'nflscrapR', 'XML', 'bitops', 'RCurl', 'ggplot2', 'nnet', 'magrittr', 'bindrcpp', 'tidyverse', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr', 'ggjoy', 'na.tools')
lapply(nflpackages, require, character.only = TRUE)

#write_csv works best

setwd("~/GitHub/nflscrapR/data-scrapR/raw")

# Update 2019 season as it progresses:
reg_pbp_19 <- read_csv("~/GitHub/nflscrapR/data-scrapR/raw/reg_pbp_2019.csv")

# SCRAPE JSON PBP by GAMEID -----------------------------------------------
game_ids_19 <- extracting_gameids(2019)
pulled_games <- reg_pbp_19 %>% pull(game_id) %>% unique()
missing <- setdiff(game_ids_19, pulled_games)

# Latest week - MODIFY WEEK:
# new_week_pbp_19 <- scrape_season_play_by_play(2019, type = "reg", weeks = 9)
new_week_pbp_19 <- purrr::map_dfr(missing, scrape_json_play_by_play)
write_csv(new_week_pbp_19,"~/GitHub/nflscrapR/data-scrapR/raw/pbp_wk9.csv")

#Write CSV as latest_week
write_csv(new_week_pbp_19,"~/GitHub/nflscrapR/data-scrapR/raw/new_week_pbp_2019.csv")
new_week_pbp_19 <- read_csv("~/GitHub/nflscrapR/data-scrapR/raw/new_week_pbp_2019.csv")

# Append to the data and save:
reg_pbp_19 <- dplyr::bind_rows(reg_pbp_19, new_week_pbp_19)
write_csv(reg_pbp_19, "~/GitHub/nflscrapR/data-scrapR/raw/reg_pbp_2019.csv")
write_csv(reg_pbp_19, "~/GitHub/nflscrapR/data-scrapR/pbp_2019.csv")
saveRDS(reg_pbp_19,"~/GitHub/nflscrapR/data-scrapR/pbp_2019.rds")





