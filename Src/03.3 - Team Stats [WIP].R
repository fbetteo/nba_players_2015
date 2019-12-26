
contribution <- rawdata %>% select(name,team,date, season, opp, minutes, pts, ast, stl, orb, drb, blk, tov, fga, tpa, fta, opp_pts) 

contribution_plus10min <- contribution[contribution$minutes > 8,]

team_contribution <- contribution_plus10min %>% group_by(team, date, opp) %>% summarise(pts = sum(pts, na.rm =TRUE),
                                                                                   ast = sum(ast, na.rm = TRUE),
                                                                                   stl = sum(stl, na.rm = TRUE),
                                                                                   orb = sum(orb, na.rm = TRUE),
                                                                                   drb = sum(drb, na.rm = TRUE),
                                                                                   blk = sum(blk, na.rm = TRUE),
                                                                                   tov = sum(tov, na.rm = TRUE),
                                                                                   fga = sum(fga, na.rm = TRUE),
                                                                                   tpa = sum(tpa, na.rm = TRUE),
                                                                                   fta = sum(fta, na.rm = TRUE),
                                                                                   season = mean(season),
                                                                                   opp_pts = mean(opp_pts),
                                                                                   minutes = sum(minutes, na.rm = TRUE))    


head(team_contribution)


################


# Cumulative Wins/losses and WinRate

