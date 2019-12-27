

library(dplyr)
library(stringr)
library(lubridate)

rawdata = read_rds("working/rawdata.rds")
team_level = read_rds("working/team_level.rds")

# Stats Player By Season ----

player_stats <- rawdata %>% group_by(name,season,team) %>% summarise(points = sum(pts, na.rm = TRUE),
                                                                     assists = sum(ast, na.rm = TRUE),
                                                                     def_reb = sum(drb, na.rm = TRUE),
                                                                     off_reb = sum(orb, na.rm = TRUE),
                                                                     turnovers = sum(tov, na.rm =TRUE),
                                                                     steals = sum(stl, na.rm = TRUE)) %>% as.data.frame()

# Stats Team By Season ----

team_stats <- rawdata %>% group_by(team, season) %>% summarise(points = sum(pts, na.rm = TRUE),
                                                                     assists = sum(ast, na.rm = TRUE),
                                                                     def_reb = sum(drb, na.rm = TRUE),
                                                                     off_reb = sum(orb, na.rm = TRUE),
                                                                     turnovers = sum(tov, na.rm =TRUE),
                                                                     steals = sum(stl, na.rm = TRUE)) %>% as.data.frame()


## Allowed points by Team per Season

opp_points_home <- team_level %>% group_by(team, season) %>% summarise(allowed_points = sum(opp_pts, na.rm = TRUE))
opp_points_away <- team_level %>% group_by(opp, season) %>% summarise(allowed_points = sum(home_pts, na.rm = TRUE))
opp_points_season <- merge(opp_points_home, opp_points_away, by.x = c("team","season"), by.y = c("opp","season"))
opp_points_season <- rename(opp_points_season, allowed_points_home  = allowed_points.x, allowed_points_away  = allowed_points.y)
opp_points_season$allowed_points_total = opp_points_season$allowed_points_home + opp_points_season$allowed_points_away




# WIN/LOSES ----

home_standing <- team_level %>% group_by(team,season) %>% summarise(home_wins =  sum(home_result =="W", na.rm = TRUE),
                                                               home_loses = sum(home_result =="L", na.rm = TRUE)) %>% as.data.frame()

away_standing <- team_level %>% group_by(opp, season) %>% summarise(away_wins = sum(home_result == "L", na.rm =TRUE),
                                                                    away_loses =  sum(home_result =="W", na.rm =TRUE)) %>% as.data.frame()

standings <- merge(home_standing, away_standing, by.x = c("team","season"), by.y = c("opp","season"))
standings$wins <-standings$home_wins + standings$away_wins
standings$loses <- standings$home_loses + standings$away_loses

# Summary Table by Team - Season ----

stats_standing <- merge(team_stats, standings) 
stats_standing <- merge(stats_standing, opp_points_season)
head(stats_standing)


# export

saveRDS(stats_standing, "working/stats_standing.rds")
