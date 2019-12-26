
team_level = read_rds("working/team_level.rds")

glimpse(team_level)

# making df ----

home_ma = team_level %>%
  select(starts_with("team_")) %>%
  select(ends_with("_ma"))


home_ma_1 = team_level %>%
  select(starts_with("team_")) %>%
  select(ends_with("_ma_1"))


away_ma = team_level %>%
  select(starts_with("opp_")) %>%
  select(ends_with("_ma"))


away_ma_1 = team_level %>%
  select(starts_with("opp_")) %>%
  select(ends_with("_ma_1"))


b2b = team_level %>%
  select(starts_with("b2b"))

model_df = team_level %>%
  select(w_dummy,
         win_ratio_lag,
         away_win_ratio_lag) %>%
  cbind.data.frame(home_ma,
                   home_ma_1,
                   away_ma,
                   away_ma_1,
                   b2b)

# model ----
