
# Load Libraries ----------------------------------------------------------
library(XML)
library(RCurl)
library(httr)

# Get data from URL via HTTR ----------------------------------------------
url <- "https://www.pro-football-reference.com/years/2019/passing.htm"
urldata <- GET(url)
data <- readHTMLTable(rawToChar(urldata$content),
stringsAsFactors = FALSE)
passing_df <- data$passing 
passing_cols <- c(2,3,5,24,25)
sacks_df <-  passing_df[passing_cols] %>% filter(Pos=="QB")