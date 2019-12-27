
team_level = read_rds("working/team_level.rds")

glimpse(team_level)

# making df ----


home_ma_1 = team_level %>%
  select(starts_with("team_")) %>%
  select(ends_with("_ma_1"))



away_ma_1 = team_level %>%
  select(starts_with("opp_")) %>%
  select(ends_with("_ma_1"))


b2b = team_level %>%
  select(starts_with("b2b"))

model_df = team_level %>%
  select(w_dummy,
         win_ratio_lag,
         away_win_ratio_lag) %>%
  cbind.data.frame(home_ma_1,
                   away_ma_1,
                   b2b) %>%
  mutate(w_dummy = as.factor(w_dummy)) %>%
  mutate_at(., .vars = vars(starts_with("b2b")), .funs = as.factor) %>%
  na.omit() %>% # remove NA
  select(-b2b_hr,-b2b_rr) # no variability
  



# export ----

saveRDS(model_df, "working/model_df.rds")
