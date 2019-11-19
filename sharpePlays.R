source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")

########## INPUTS ##########

# filename to store plays
plays_filename = "~/GitHub/nflscrapR/pbp_2019.rds" # replace this with file where you want to play data
baldwin_mutations <- TRUE   # do you want to apply Ben Baldwin's mutations?
comp_prob <- FALSE           # do you want to apply completion probability?
series_data <- FALSE         # do you want to apply series data?

## NOTE: you must set baldwin_mutations to TRUE to get comp_prob

########## LOAD EXISTING DATA ##########

# load game data
report("Loading game data")
games <- read_csv("http://www.habitatring.com/games.csv")
games <- games %>%
  filter(season >= 2019 & !is.na(result)) %>% 
  mutate(game_id=as.character(game_id))

# load previous data
report("Loading existing plays data")
old_warning_level <- getOption("warn")
options(warn=-1)
tryCatch(plays <- readRDS(plays_filename),error=report)
options(warn=old_warning_level)

########## INITIAL COMP PROBABILITY ##########

# temporarily here to help people who want to add cp to existing data
# if (exists("plays") & baldwin_mutations & comp_prob &
#     !("cp" %in% colnames(plays)))
# {
#   plays <- apply_completion_probability(plays)
#   saveRDS(plays,plays_filename)
# }

########## SCRAPE GAMES IN CURRENT SEASON ##########

# we have data! identify any missing games
pulled_games <- plays %>% pull(game_id) %>% unique()
missing <- games %>% filter(!(game_id %in% pulled_games)) %>% pull(game_id)

# handle missing games
if (length(missing) > 0)
{
  # get new plays
  new_plays <- NULL
  for (g in missing)
  {
    # scrape
    report(paste0("Scraping plays from game: ",g))
    game_plays <- scrape_json_play_by_play(g)
    
    # add in basic data
    new_plays <- game_plays %>%
      fix_inconsistent_data_types() %>% 
      fix_team_abbreviations() %>% 
      apply_game_data()
    
    # additional optional modifications
    if (baldwin_mutations) new_plays <- apply_baldwin_mutations(new_plays)
    if (baldwin_mutations & comp_prob)
      new_plays <- apply_completion_probability(new_plays)
    if (series_data) new_plays <- apply_series_data(new_plays)
    
    # finally merge things together
    report("Merging existing plays and new plays")
    plays <- bind_rows(plays,new_plays) %>% arrange(game_id,play_id)
    
    # save after each game
    saveRDS(plays,plays_filename)
    report("Saving new plays")
  }

  # no need for this to take up memory anymore
  rm(game_plays)
  rm(new_plays)
}