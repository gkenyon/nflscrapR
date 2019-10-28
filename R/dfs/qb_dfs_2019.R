nflpackages <- c('devtools', 'nflscrapR', 'XML', 'bitops', 'RCurl', 'ggplot2', 'nnet', 'magrittr', 'bindrcpp', 'tidyverse', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr', 'ggjoy', 'na.tools', 'httr')
lapply(nflpackages, require, character.only = TRUE)
#read in our csvs #####
#Import QB Season and Team Season Data Frames


# Join DVOA ---------------------------------------------------------------

url <- "https://www.footballoutsiders.com/stats/teamdef/2019"
urldata <- GET(url)
data <- readHTMLTable(rawToChar(urldata$content),
                      stringsAsFactors = FALSE)
def_df <- data[["NULL"]] 
def_cols <- c(2,3,7,9,11,12,13)
dvoa_df <-  def_df[def_cols]
write_csv(dvoa_df, "~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/dvoa_def.csv")
dvoa_def <- read_csv("~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/dvoa_def.csv",
col_types = cols(DEFENSEDVOA = col_number(),
`NON-ADJPASS` = col_number(), `NON-ADJRUSH` = col_number(),
`NON-ADJTOTAL` = col_number(), PASSDEF = col_number(),
RUSHDEF = col_number()))

qb_df_adj <- left_join(qb_df_adj, dvoa_def, by=c("opp_team" = "TEAM"))
write_csv(qb_df_adj, "~/GitHub/nflscrapR/data-scrapR/splits_data/dfs/qb_df_adj.csv")

# Advanced Stats Glossary -------------------------------------------------

# MS_Tar = Targets/Atttempts
# MS_AY= Total_Caught_AirYards/Total_Raw_AirYards
# wOPR = 1.5 × Target Market Share + 0.7 × Air Yards Market Share
# AYA (Adjusted Yards per Attempt)
# AYA=passingyards+(20×TDs)−(45×INTs)/attempts
# ANYA (Adjusted Net Yards per Attempt)
# ANYA=passingyards+(20×passingTDs)−(45×INTs)−sackyards/attempts+sacks
# PACR (Passing Air Conversion Ratio)
# PACR=completedairyards+yardsafterthecatch/airyards
# PACR=passingyards/airyards
# PACR=YPA/aDOT
# Adjusted PACR (aPACR). If we add TDs and INTs to the PACR calculation with the multipliers suggested in the AYA formula we get Adjusted PACR (aPACR).
# aPACR=passingyards+(20×TD)−(45×INT)/airyards

# Speed Score – Bill Barnwell first posited the metric in Pro Football Prospectus to better predict running back success. The formula is (weight*200) / (40-time^4). It factors weight into a player’s 40-yard dash time assigning a premium to fast times run by bigger, often stronger, running backs.

#For wide receivers and tight ends, air yards are total completed receiving yards from the line of scrimmage to the catch point. Air yards are also referred to as “completed air yards” by Josh Hermsmeyer in his introduction to the predictive qualities of air yards on RotoViz.com and his AirYards.com property. On PlayerProfiler, air yards differs from total pass attempt distance and total target distance, which aggregates all completed and incomplete air yards.