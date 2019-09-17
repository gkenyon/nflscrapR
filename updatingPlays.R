#update GameIDs
reg_games_19 <- scrape_game_ids(2019, type = "reg")
readr::write_csv(reg_games_19,"games_data/regular_season/reg_games_2019.csv")


# Update 2019 season as it progresses:
#reg_pbp_19 <- readr::read_csv("play_by_play_data/regular_season/reg_pbp_2019.csv")
# Latest week - just modify the week number:
new_week_pbp_19 <- scrape_season_play_by_play(2019, type = "reg", weeks = "1")
readr::write_csv(new_week_pbp_19,"play_by_play_data/regular_season/latest_week_pbp_2019.csv")
new_week_pbp_19 <- readr::read_csv("play_by_play_data/regular_season/latest_week_pbp_2019.csv")
# Append to the data and save:
reg_pbp_19 <- dplyr::bind_rows(reg_pbp_19, new_week_pbp_19)
