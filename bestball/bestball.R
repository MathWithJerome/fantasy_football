library(dplyr)
library(ROI)
library(gurobi)
library(ROI.plugin.gurobi)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
library(tidyr)
library(tibble)

players.l = list()
for (y in 2010:2020) {
  print(y)
  player.pool = fd.w %>% filter(season == y, Pos %in% c("QB","RB","WR","TE")) %>% 
    group_by(Player) %>% 
    summarise(total.pts = sum(HalfPPRFantasyPoints)) %>% 
    filter(total.pts > 50) %>% 
    select(Player)
  
  # pts = fd.w %>% filter(season == y, Pos %in% c("QB","RB","WR","TE"), Player %in% player.pool$Player) %>%
  #   select(Player, Pos, Tm, week, HalfPPRFantasyPoints) %>%
  #   mutate(is.flex = if_else(Pos %in% c("RB","WR","TE"), T, F))
  # 
  # points = pts %>% pivot_wider(id_cols = Player, names_from = week, values_from = HalfPPRFantasyPoints) %>%
  #   replace(is.na(.), 0) %>%
  #   column_to_rownames(var = "Player")
  
  points = fd.w %>% filter(season == y, Pos %in% c("QB","RB","WR","TE"), Player %in% player.pool$Player) %>% 
    select(Player, Pos, Tm, week, HalfPPRFantasyPoints) %>% 
    pivot_wider(names_from = week, values_from = HalfPPRFantasyPoints, values_fill = 0) %>% 
    mutate(is.flex = if_else(Pos %in% c("RB","WR","TE"), T, F)) # %>% 
    #replace(is.na(.), 0) %>% 
    #column_to_rownames(var = "Player")
  
  qb.set = unique(pts$Player[which(pts$Pos=="QB")])
  rb.set = unique(pts$Player[which(pts$Pos=="RB")])
  wr.set = unique(pts$Player[which(pts$Pos=="WR")])
  te.set = unique(pts$Player[which(pts$Pos=="TE")])
  
  qb = which(row.names(points) %in% qb.set)
  rb = which(row.names(points) %in% rb.set)
  wr = which(row.names(points) %in% wr.set)
  te = which(row.names(points) %in% te.set)
  #dst = which(pts$Pos=="DST")
  #k = which(pts$Pos=="K")
  flex = c(rb,wr,te)
  
  n_players = nrow(points)
  n_weeks = ifelse(y < 2021, 17, 18)
  L = 18
  
  bestball.base = MIPModel() %>%
    add_variable(player[i], i = 1:n_players, type = "binary") |>
    add_variable(x[i,j], i = 1:n_players, j = 1:n_weeks, type = "binary") %>%
    #add_variable(z, lb = 0) %>% 
    set_objective(sum_over(points[i,j] * x[i,j], i = 1:n_players, j = 1:n_weeks), "max") %>%
    #add_constraint(sum_expr(cost[i] * player[i], i = 1:n) <= cap) %>%
    add_constraint(sum_over(x[i,j], i = qb) == 1, j = 1:n_weeks) %>%
    #add_constraint(sum_expr(player[i], i = dst) <= 1) %>%
    add_constraint(sum_over(x[i,j], i = rb) <= 3, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = wr) <= 4, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = te) <= 2, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = flex) == 7, j = 1:n_weeks) %>% 
    #add_constraint(sum_over(x[i,j], j = 1:n_weeks) <= 17*player[i], i = 1:n_players) %>% 
    add_constraint(x[i,j] <= player[i], i = 1:n_players, j = 1:n_weeks) %>% 
    add_constraint(sum_over(player[i], i = 1:n_players) == L)
  
  draftees.sol = solve_model(bestball.base, with_ROI(solver = "gurobi", verbose = F))
  #weekly = draftees.sol %>% get_solution(x[i,j]) %>%  filter(value > 0) 
  players.sol = draftees.sol %>% get_solution(player[i]) %>%  filter(value > 0) 
  
  tmp = unique(merge(data.frame(Player = row.names(points[players.sol$i,])), pts[,c("Player","Pos")]))
  tmp$Year = y
  players.l[[as.character(y)]] = tmp
}
players = do.call(rbind,players.l)
table(players[c("Pos","Year")])
