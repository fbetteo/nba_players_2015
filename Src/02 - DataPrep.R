

library(dplyr)
library(stringr)
library(lubridate)

rawdata = read_rds("working/rawdata.rds")
odds = read_rds("working/odds.rds")

# Excel para emprolijar abreviacion de equipos

abreviacion= vroom::vroom("inputs/abreviacion.csv")

#clean, solo lo que es por equipo
cleandata= select(rawdata,venue, team, date, season, opp, starts_with("team"), starts_with("opp"))

cleandata_unique = distinct(cleandata, team, date, .keep_all = TRUE  )
cleandata_unique$team <- as.character(cleandata_unique$team)
cleandata_unique$opp <- as.character(cleandata_unique$opp)
cleandata_unique$team <- ifelse(cleandata_unique$team == "NOH", "NOP", cleandata_unique$team)
cleandata_unique$opp <- ifelse(cleandata_unique$opp == "NOH", "NOP", cleandata_unique$opp)
cleandata_unique$team <- ifelse(cleandata_unique$team == "SEA", "OKC", cleandata_unique$team)
cleandata_unique$opp <- ifelse(cleandata_unique$opp == "SEA", "OKC", cleandata_unique$opp)
cleandata_unique$team <- ifelse(cleandata_unique$team == "NJN", "BRK", cleandata_unique$team)
cleandata_unique$opp <- ifelse(cleandata_unique$opp == "NJN", "BRK", cleandata_unique$opp)
cleandata_unique$team <- ifelse(cleandata_unique$team == "CHA", "CHO", cleandata_unique$team)
cleandata_unique$opp <- ifelse(cleandata_unique$opp == "CHA", "CHO", cleandata_unique$opp)

cleandata_unique$team <- as.factor(cleandata_unique$team)
cleandata_unique$opp <- as.factor(cleandata_unique$opp)

# Extraer desde opp los valores para el local ----
opp_table <- select(cleandata_unique, date, opp,starts_with("opp") )

names(opp_table) <- gsub("opp", "team", names(opp_table))

full <- merge(cleandata_unique, opp_table, by.x=c("date","team"), by.y =c("date", "team"))

# formato fechas
full$date <- ymd(full$date)

full$id_orig = rownames(full)


# back to backs ----

## b2b_hh
full1 <- full %>% arrange(team,date) %>% group_by(team) %>% mutate(b2b_hh = as.numeric(ifelse(date == as.numeric(lag(date)) + 1 & venue == "H" & lag(venue) == "H", 1,0)))
## b2b_hr
full1 <- full1 %>% arrange(team,date) %>% group_by(team) %>% mutate(b2b_hr = as.numeric(ifelse(date == as.numeric(lag(date)) + 1 & venue == "A" & lag(venue) == "H", 1,0)))
## b2b_rr
full1 <- full1 %>% arrange(team,date) %>% group_by(team) %>% mutate(b2b_rr = as.numeric(ifelse(date == as.numeric(lag(date)) + 1 & venue == "A" & lag(venue) == "A", 1,0)))
## b2b_rh
full1 <- full1 %>% arrange(team,date) %>% group_by(team) %>% mutate(b2b_rh = as.numeric(ifelse(date == as.numeric(lag(date)) + 1 & venue == "H" & lag(venue) == "A", 1,0)))

## Puntos del local ----

puntos <- select(rawdata, team,opp,date,pts,opp_pts)
puntos2 <- puntos %>% arrange(team,date) %>% group_by(team,date,opp) %>% summarise(home_pts = sum(pts, na.rm=TRUE)) #suma de todos los jugadores por partido

full2 <- merge(full1, puntos2, by.x=c("date","team", "opp"), by.y =c("date", "team", "opp"))

# Home Result ----

full2$home_result <- as.factor(ifelse(full2$home_pts > full2$opp_pts, "W","L"))

# Nombres y Odds ----
# Nombre completos
full2 <- merge(full2, abreviacion, by.x="team", by.y="Abreviado") %>% rename(hometeam = Fullname)
full2 <- merge(full2, abreviacion, by.x="opp", by.y = "Abreviado") %>% rename(awayteam = Fullname)




full2 <- full2 %>% arrange(team,date,season) 
full2 <- full2 %>% mutate(w_dummy=0)
full2$w_dummy <-  ifelse(full2$venue == "H" &  full2$home_result=="W",1 ,ifelse(full2$venue == "A" & full2$home_result == "L",1,0))
full2$l_dummy <-  ifelse(full2$venue == "H" &  full2$home_result=="L",1 ,ifelse(full2$venue == "A" & full2$home_result == "W",1,0))

full2 = full2 %>% group_by(team,season) %>% mutate(w_count = cumsum(w_dummy))
full2 = full2 %>% group_by(team,season) %>% mutate(l_count = cumsum(l_dummy)) %>% ungroup()
full2 = full2 %>% mutate(played_games = w_count + l_count)
full2 = full2 %>% mutate(win_ratio = w_count / played_games)
full2 = full2 %>% mutate(win_ratio_lag = lag(win_ratio, k =1, default = 0))

temp <- full2 %>% select(team,date,away_w_count = w_count, away_l_count= l_count, away_played_games =  played_games,away_win_ratio = win_ratio, away_win_ratio_lag =win_ratio_lag)

full2 <- left_join(full2,temp,
                   by = c("opp" ="team", "date" = "date"))

#Odds
odds_home <- odds %>% select(team, date, odds_home) %>% filter(odds_home >0)
odds_away <- odds %>% select(team, date, odds_away) %>% filter(odds_away >0)

team_level <- merge(full2, odds_home, by.x=c("hometeam", "date"), by.y=c("team","date"))
team_level <- merge(team_level, odds_away, by.x=c("awayteam", "date"), by.y=c("team","date"))


# export ----

saveRDS(team_level, "working/team_level.rds")

