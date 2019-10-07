nflpackages <- c('devtools', 'nflscrapR', 'XML', 'bitops', 'RCurl', 'ggplot2', 'nnet', 'magrittr', 'bindrcpp', 'tidyverse', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr', 'ggjoy', 'na.tools')
lapply(nflpackages, require, character.only = TRUE)

#write_csv works best

setwd("~/GitHub/nflscrapR/data-scrapR/raw")

# Update 2019 season as it progresses:
reg_pbp_19 <- read_csv("~/GitHub/nflscrapR/data-scrapR/raw/reg_pbp_2019.csv")

# Latest week - MODIFY WEEK:
new_week_pbp_19 <- scrape_season_play_by_play(2019, type = "reg", weeks = 5)
write_csv(new_week_pbp_19,"~/GitHub/nflscrapR/data-scrapR/raw/pbp_wk5.csv")

#Write CSV as latest_week
write_csv(new_week_pbp_19,"~/GitHub/nflscrapR/data-scrapR/raw/new_week_pbp_2019.csv")
new_week_pbp_19 <- read_csv("~/GitHub/nflscrapR/data-scrapR/raw/new_week_pbp_2019.csv")
new_week_pbp_19 <- apply_baldwin_mutations(new_week_pbp_19)

# Append to the data and save:
reg_pbp_19 <- dplyr::bind_rows(reg_pbp_19, new_week_pbp_19)
write_csv(reg_pbp_19, "~/GitHub/nflscrapR/data-scrapR/raw/reg_pbp_2019.csv")
write_csv(reg_pbp_19, "~/GitHub/nflscrapR/data-scrapR/pbp_2019.csv")
saveRDS(reg_pbp_19,"~/GitHub/nflscrapR/data-scrapR/raw/pbp_2019.rds")

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
write_csv(pbp_rp, "~/Github/nflscrapR/data-scrapR/raw/2019_pbp_rp.csv")

#Read in old data, join and save.
pbp_all_rp <- readRDS("~/GitHub/nflscrapR/data-scrapR/reg_pbp_all_rp.rds")
reg_pbp_all_rp <- dplyr::bind_rows(pbp_all_rp, pbp_rp_19)
saveRDS(reg_pbp_all_rp, "~/Github/nflscrapR/data-scrapR/reg_pbp_all_rp.rds")

# aDOT --------------------------------------------------------------------
pbp_aDOT <- pbp_data %>%
  filter(PlayType == "Pass") %>%
  group_by("Receiver") %>%
  summarize(adot = mean(air_yards), targets = n(), catch_rate = sum(complete_pass)/targets)
