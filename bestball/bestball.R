{
  library(dplyr)
  library(ROI)
#  library(gurobi)
#  library(ROI.plugin.gurobi)
  library(ROI.plugin.glpk)
  library(ROI.plugin.symphony)
  library(ompr)
  library(ompr.roi)
  library(tidyr)
  library(tibble)
}

bestball.l = bestball.w.l = list()

for (y in 2018:2022) {
  print(y)
  player.pool = fp %>% filter(season == y, position %in% c("QB","RB","WR","TE")) %>% 
    group_by(player_id, player_display_name) %>% 
    summarise(total.pts = sum(half_ppr)) %>% 
    filter(total.pts > 50) %>% 
    select(player_id, player_display_name)
  
  points = fp %>% filter(season == y, position %in% c("QB","RB","WR","TE"), #player_display_name %in% player.pool$player_display_name) %>% 
                         player_id %in% player.pool$player_id) %>% 
    select(player_display_name, position, week, half_ppr) %>% 
    pivot_wider(names_from = week, values_from = half_ppr, values_fill = 0) %>% 
    mutate(is.flex = if_else(position %in% c("RB","WR","TE"), T, F)) 
  
  qb = which(points$position=="QB") #which(row.names(points) %in% qb.set)
  rb = which(points$position=="RB") #which(row.names(points) %in% rb.set)
  wr = which(points$position=="WR") #which(row.names(points) %in% wr.set)
  te = which(points$position=="TE") #which(row.names(points) %in% te.set)
  
  #dst = which(pts$position=="DST")
  #k = which(pts$position=="K")
  
  qb.set = unique(points$player_display_name[qb])
  rb.set = unique(points$player_display_name[rb])
  wr.set = unique(points$player_display_name[wr])
  te.set = unique(points$player_display_name[te])

  flex = c(rb,wr,te)
  
  n_players = nrow(points)
  n_weeks = ifelse(y < 2021, 17, 18)
  L = 18
  
  pts = as.matrix(points[,as.character(1:n_weeks)])
  
  bestball.base = MIPModel() %>%
    ## define decision variables
    add_variable(player[i], i = 1:n_players, type = "binary") %>% 
    add_variable(x[i,j], i = 1:n_players, j = 1:n_weeks, type = "binary") %>%
    
    ## objective is to maximize total fantasy points scored (this may not be the best choice!)
    set_objective(sum_over(pts[i,j] * x[i,j], i = 1:n_players, j = 1:n_weeks), "max") %>%
    
    ## for lineup diversity, we may want to consider an overall salary cap,
    ## but it's commented out for now
    # add_constraint(sum_expr(cost[i] * player[i], i = 1:n) <= cap) %>%
    
    ## constraints for the lineups requirements each week
    add_constraint(sum_over(x[i,j], i = qb) == 1, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = rb) <= 3, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = wr) <= 4, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = te) <= 2, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = rb) >= 2, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = wr) >= 2, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = te) >= 1, j = 1:n_weeks) %>%
    add_constraint(sum_over(x[i,j], i = flex) == 7, j = 1:n_weeks) %>%
    #add_constraint(sum_over(x[i,j], j = 1:n_weeks) <= 17*player[i], i = 1:n_players) %>% 
    add_constraint(x[i,j] <= player[i], i = 1:n_players, j = 1:n_weeks) %>%
    
    ## constraints for total number of players drafted, position maximums taken from www.fanduel.com/best-ball-rules
    add_constraint(sum_over(player[i], i = 1:n_players) == L) %>% 
    add_constraint(sum_over(player[i], i = qb) <= 4) %>% 
    add_constraint(sum_over(player[i], i = rb) <= 8) %>% 
    add_constraint(sum_over(player[i], i = wr) <= 10) %>% 
    add_constraint(sum_over(player[i], i = te) <= 4)
  
  ## solve the IP, extract unique players drafted and weekly assignments
  draftees.sol = solve_model(bestball.base, with_ROI(solver = "gurobi", verbose = F))
  players.sol = draftees.sol %>% get_solution(player[i]) %>%  filter(value > 0) 
  weekly = draftees.sol %>% get_solution(x[i,j]) %>%  filter(value > 0) 
  
  tmp = unique(points[players.sol$i,c("player_display_name", "position")])
  tmp$season = y
  bestball.l[[as.character(y)]] = tmp
  bestball.w.l[[as.character(y)]] = data.frame(player = points$player_display_name[weekly$i],
                                               position = points$position[weekly$i],
                                               week = weekly$j,
                                               season = y)
}

x = points[weekly$i,(weekly$j+2)]
points[weekly[,c("i","j")]]

bestball = do.call(rbind,bestball.l)
rownames(bestball) = NULL
table(bestball[c("position","season")])

bestball.w = do.call(rbind,bestball.w.l)
rownames(bestball.w) = NULL
