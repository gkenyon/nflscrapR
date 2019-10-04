season_passing_df <- calc_passing_splits(c("Season","Passer_ID"), pbp_data) %>% 
  filter(Passer_ID != "None") %>% arrange(Season,desc(Attempts))
season_passing_df <- 

season_receiving_df <- calc_receiving_splits(c("Season","Receiver_ID"), pbp_data) %>% 
  filter(Receiver_ID != "None") %>% arrange(Season,desc(Targets)) 

season_rushing_df <- calc_rushing_splits(c("Season","Rusher_ID"), pbp_data) %>%
  filter(Rusher_ID != "None") %>% arrange(Season,desc(Carries))