library("ffanalytics", lib.loc="~/R/R-3.6.1/library")
library(tidyr)
library(dplyr)

sources <- c('CBS', 'Yahoo', 'FantasySharks', 'NumberFire', 'FantasyPros', 'FantasyData', 'FleaFlicker')
week <- 6
scrape <- scrape_data(src = sources,
                      pos=c('QB', 'RB', 'WR', 'TE', 'DST'),
                      season = 2019, 
                      week=week)

nf_scrape <- scrape_data(src = "NumberFire", pos = c("QB", "RB", "WR", "TE"),
                         season = 2019, week = 6)
my_projections <-  projections_table(my_scrape) %>% add_player_info() %>% filter(avg_type=="average")

NF_qbs <- nf_scrape$QB %>%
  select(data_src, id, src_id, player, team, opp_team, opp_rank, draftkings_fp, draftkings_cost, draftkings_value)
write_csv(NF_qbs, "~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/NF_qbs.csv")
NF_rb <- nf_scrape$RB %>% dplyr::filter(data_src=="NumberFire")
NF_wr <- nf_scrape$WR %>% dplyr::filter(data_src=="NumberFire")
NF_te <- nf_scrape$TE %>% dplyr::filter(data_src=="NumberFire")
NF_recs <- bind_rows(NF_rb, NF_wr, NF_te) %>%
  select(data_src, id, src_id, player, team, opp_team, opp_rank, draftkings_fp, draftkings_cost, draftkings_value)
remove(NF_rb, NF_te, NF_wr)
write_csv(NF_recs, "~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/NF_recs.csv")

dfs_receiver <- dfs_receiver %>% filter(MS_AirYards > .20) %>% filter(PASSDEF > 20)


