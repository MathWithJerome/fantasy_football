library(dplyr)
library(ROI)
library(gurobi)
library(ROI.plugin.gurobi)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
library(tidyr)
library(tibble)

bestball.l = list()
for (y in 2010:2021) {
  print(y)
  player.pool = fp.w %>% filter(season == y, position %in% c("QB","RB","WR","TE")) %>% 
    group_by(full_name) %>% 
    summarise(total.pts = sum(half_ppr)) %>% 
    filter(total.pts > 50) %>% 
    select(full_name)
  
  # pts = fp.w %>% filter(season == y, Pos %in% c("QB","RB","WR","TE"), Player %in% player.pool$Player) %>%
  #   select(Player, Pos, Tm, week, HalfPPRFantasyPoints) %>%
  #   mutate(is.flex = if_else(Pos %in% c("RB","WR","TE"), T, F))
  # 
  # points = pts %>% pivot_wider(id_cols = Player, names_from = week, values_from = HalfPPRFantasyPoints) %>%
  #   replace(is.na(.), 0) %>%
  #   column_to_rownames(var = "Player")
  
  points = fp.w %>% filter(season == y, position %in% c("QB","RB","WR","TE"), full_name %in% player.pool$full_name) %>% 
    select(full_name, position, recent_team, week, half_ppr) %>% 
    pivot_wider(names_from = week, values_from = half_ppr, values_fill = 0) %>% 
    mutate(is.flex = if_else(position %in% c("RB","WR","TE"), T, F)) # %>% 
    #replace(is.na(.), 0) %>% 
    #column_to_rownames(var = "Player")
  
  qb.set = unique(points$full_name[which(points$position=="QB")])
  rb.set = unique(points$full_name[which(points$position=="RB")])
  wr.set = unique(points$full_name[which(points$position=="WR")])
  te.set = unique(points$full_name[which(points$position=="TE")])
  
  qb = which(points$position=="QB") #which(row.names(points) %in% qb.set)
  rb = which(points$position=="RB") #which(row.names(points) %in% rb.set)
  wr = which(points$position=="WR") #which(row.names(points) %in% wr.set)
  te = which(points$position=="TE") #which(row.names(points) %in% te.set)
  #dst = which(pts$position=="DST")
  #k = which(pts$position=="K")
  flex = c(rb,wr,te)
  
  n_players = nrow(points)
  n_weeks = ifelse(y < 2021, 17, 18)
  L = 18
  
  pts = as.matrix(points[,as.character(1:n_weeks)])
  
  bestball.base = MIPModel() %>%
    add_variable(player[i], i = 1:n_players, type = "binary") %>% 
    add_variable(x[i,j], i = 1:n_players, j = 1:n_weeks, type = "binary") %>%
    set_objective(sum_over(pts[i,j] * x[i,j], i = 1:n_players, j = 1:n_weeks), "max") %>%
    ##add_constraint(sum_expr(cost[i] * player[i], i = 1:n) <= cap) %>%
    add_constraint(sum_over(x[i,j], i = qb) == 1, j = 1:n_weeks) %>%
    #add_constraint(sum_expr(player[i], i = dst) <= 1) %>%
    add_constraint(sum_over(x[i,j], i = rb) <= 3, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = wr) <= 4, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = te) <= 2, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = rb) >= 2, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = wr) >= 2, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = te) >= 1, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = flex) == 7, j = 1:n_weeks) %>% 
    #add_constraint(sum_over(x[i,j], j = 1:n_weeks) <= 17*player[i], i = 1:n_players) %>% 
    add_constraint(x[i,j] <= player[i], i = 1:n_players, j = 1:n_weeks) %>% 
    add_constraint(sum_over(player[i], i = 1:n_players) == L)
  
  draftees.sol = solve_model(bestball.base, with_ROI(solver = "gurobi", verbose = F))
  weekly = draftees.sol %>% get_solution(x[i,j]) %>%  filter(value > 0) 
  players.sol = draftees.sol %>% get_solution(player[i]) %>%  filter(value > 0) 
  
  #tmp = unique(merge(data.frame(Player = row.names(points[players.sol$i,])), pts[,c("Player","position")]))
  tmp = unique(points[players.sol$i,c("full_name", "position", "recent_team")])
  tmp$Year = y
  bestball.l[[as.character(y)]] = tmp
}
bestball = do.call(rbind,bestball.l)
table(bestball[c("position","Year")])
