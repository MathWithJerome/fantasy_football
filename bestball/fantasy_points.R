{
  library(tidyr)
  library(dplyr)
  library(nflfastR)
}

fp.load = load_player_stats(2012:2022)
fp = fp.load %>% 
  select(player_id, player_display_name, position, fantasy_points, fantasy_points_ppr, receptions, season, season_type, week) %>% 
  filter(season_type=="REG", position %in% c("QB","TE","WR","RB")) %>% 
  mutate(half_ppr = fantasy_points + 0.5*receptions)
rm(fp.load)
