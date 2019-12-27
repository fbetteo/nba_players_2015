## Import and basic cleaning ----

library(tidyverse)
library(lubridate)


# Import

rawdata= vroom::vroom("inputs/basketball_research_data_set.csv")


# Transformamos un poco la data

rawdata$team <- as.character(rawdata$team)
rawdata$opp <- as.character(rawdata$opp)
rawdata$team <- ifelse(rawdata$team == "NOH", "NOP", rawdata$team)
rawdata$opp <- ifelse(rawdata$opp == "NOH", "NOP", rawdata$opp)
rawdata$team <- ifelse(rawdata$team == "SEA", "OKC", rawdata$team)
rawdata$opp <- ifelse(rawdata$opp == "SEA", "OKC", rawdata$opp)
rawdata$team <- ifelse(rawdata$team == "NJN", "BRK", rawdata$team)
rawdata$opp <- ifelse(rawdata$opp == "NJN", "BRK", rawdata$opp)
rawdata$team <- ifelse(rawdata$team == "CHA", "CHO", rawdata$team)
rawdata$opp <- ifelse(rawdata$opp == "CHA", "CHO", rawdata$opp)

rawdata$date <- ymd(rawdata$date)
rawdata$season <- ifelse(month(rawdata$date)<9, year(rawdata$date)-1,year(rawdata$date))

# Importamos las odds de los partidos

odds_raw = vroom::vroom("inputs/NBA_Odds_FULL_TO_USE.csv") %>%
  janitor::clean_names()

# Transformamos un poco la data

odds_raw$team = as.factor(str_trim(odds_raw$team))
odds = odds_raw %>% select(-open,-close,-x2h) %>% rename(date=date_fix)
odds$date = as.Date(odds$date,format = "%d/%m/%Y")
odds$team <- str_trim(odds$team)
odds$team <- ifelse(odds$team == "Seattle", "Oklahoma City", odds$team)
odds$team <- ifelse(odds$team == "New Jersey", "Brooklyn", odds$team)



# export ----

saveRDS(rawdata, "Working/rawdata.RDS")
saveRDS(odds, "Working/odds.RDS")
