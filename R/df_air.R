library(jsonlite)
df_air_wks <- fromJSON('http://airyards.com/2019/weeks')
write_csv(df_air_wks, "./df_air.csv")
rec_pos <- c("WR", "TE", "RB")
df_air_rec <- df_air_wks %>%
  group_by(player_id, full_name, position, team) %>% 
    summarise(
      tars = sum(tar),                                             
      tds = sum(td),
      rush_tds = sum(rush_td),
      recs = sum(rec),
      rec_yard = sum(rec_yards),
      rush_yard = sum(rush_yards),
      yac = sum(yac),
      air_yard = sum(air_yards),
      tm_atts = sum(tm_att),
      team_air = sum(team_air),
      aDOT = air_yard/tars,
      racr = rec_yard/air_yard,
      ms_AY = air_yard/team_air,
      ms_TAR = tars/tm_atts,
      wopr = 1.5 * ms_TAR + 0.7 * ms_AY) %>%
        filter(position %in% rec_pos) %>% 
          arrange(desc(tars))
saveRDS(df_air_rec, "./df_ay_rec.rds")
