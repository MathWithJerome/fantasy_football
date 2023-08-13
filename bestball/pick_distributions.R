library(ggplot2)
library(tidyr)
library(dplyr)
library(ROI)
library(gurobi)
library(ROI.plugin.gurobi)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

picks = 0:319
draft.pdf = data.frame(picks = picks)

pdf.m = matrix(nrow = 320, ncol = 320)

for (p in picks) {
  pdf.m[p+1,] = round(dpois(picks, p+1),5)
}

# pdf.m.s = apply(pdf.m, 2, function(x) {round(x/sum(x),5)})
pdf.m.m = as_tibble(pdf.m) %>%
  mutate(Rank = 1:nrow(pdf.m),.before = V1) %>%
  pivot_longer(cols = -Rank, names_to = "pick") %>%
  mutate(pick = as.numeric(gsub("V","",pick)))

n = 240
pdf.m.v1 = matrix(nrow = n, ncol = n)
for (i in 1:n) {
  for (j in 1:n) {
    pdf.m.v1[i,j] = 1 - (pdf.m[i,j] / (1-sum(pdf.m[i,1:(j-1)])))
  }
}

n = 320
pdf.m.v2 = matrix(nrow = n, ncol = n)
pdf.m.v2[,1] = 1
for (i in 1:n) {
  for (j in 2:n) {
    pdf.m.v2[i,j] = max(0,1 - sum(pdf.m[i,1:(j-1)]))
  }
}

df.v2 = cbind(data.frame(Rank = 1:n, as.data.frame(pdf.m.v2))) %>% 
  pivot_longer(cols = 2:last_col(), names_to = "pick") %>% 
  mutate(pick = as.numeric(gsub("V","",pick)))
  
                
#ggplot(data = df.v2 %>% filter(player %in% paste0("player",seq(1,220,20)), pick <= 340), aes(x = pick, y = value, color = player)) +
#  geom_point(show.legend = F) + geom_line(show.legend = F) 

m.picks = matrix(data = 1:240, nrow = 20, ncol = 12, byrow = T, dimnames = list(paste0("round",1:20), paste0("pick",1:12)))
for (i in seq(2,20,2)) {
  m.picks[i,] = rev(m.picks[i,])    
}

p = data.frame(round = names(m.picks[,6]), pick = m.picks[,6])

df.v2.p = df.v2 %>% filter(pick %in% p$pick, value > 0.1) %>% left_join(p) %>% 
  left_join(pdf.m.m, by = c("Rank" = "Rank","pick" = "pick"), suffix = c(".cdf", ".pdf") ) %>% 
  filter(value.pdf > .001)
points.nf.r = merge(points.nf %>% select(-round), df.v2.p, by = "Rank") %>% 
  mutate(player.round = paste0(player_name,"_",round))

for (i in as.character(1:18)){
  points.nf.r[,i] = points.nf.r[,i]*points.nf.r$value.cdf
}

qb = which(points.nf.r$position=="QB") 
rb = which(points.nf.r$position=="RB") 
wr = which(points.nf.r$position=="WR") 
te = which(points.nf.r$position=="TE") 
flex = c(rb,wr,te)

pts = points.nf.r[,as.character(1:18)]
n_players = nrow(pts)

bestball.base = MIPModel() %>%
  add_variable(player[i], i = 1:n_players, type = "binary") %>% 
  add_variable(x[i,j], i = 1:n_players, j = 1:n_weeks, type = "binary") %>%
  set_objective(sum_over(pts[i,j] * x[i,j], i = 1:n_players, j = 1:n_weeks), "max") %>%
  add_constraint(sum_over(x[i,j], i = qb) == 1, j = 1:n_weeks) %>%
  add_constraint(sum_over(x[i,j], i = rb) <= 3, j = 1:n_weeks) %>%
  add_constraint(sum_over(x[i,j], i = wr) <= 4, j = 1:n_weeks) %>%
  add_constraint(sum_over(x[i,j], i = te) <= 2, j = 1:n_weeks) %>%
  add_constraint(sum_over(x[i,j], i = rb) >= 2, j = 1:n_weeks) %>%
  add_constraint(sum_over(x[i,j], i = wr) >= 3, j = 1:n_weeks) %>%
  add_constraint(sum_over(x[i,j], i = te) >= 1, j = 1:n_weeks) %>%
  add_constraint(sum_over(x[i,j], i = flex) == 7, j = 1:n_weeks) %>% 
  add_constraint(sum_over(x[i,j], j = 1:n_weeks) <= 17*player[i], i = 1:n_players) %>% 
  #add_constraint(x[i,j] <= player[i], i = 1:n_players, j = 1:n_weeks) %>% 
  add_constraint(sum_over(player[i], i = 1:n_players) == 20)

for (r in unique(points.nf.r$Rank)) {
  k = which(points.nf.r$Rank==r)
  bestball.base = bestball.base %>% add_constraint(sum_over(player[i], i = k) <= 1)
}

for (r in unique(points.nf.r$round)) {
  k = which(points.nf.r$round==r)
  bestball.base = bestball.base %>% add_constraint(sum_over(player[i], i = k) == 1)
}
t = Sys.time(); draftees.sol = solve_model(bestball.base, with_ROI(solver = "gurobi", verbose = T)); print(Sys.time() - t)

weekly.sol = draftees.sol %>% get_solution(x[i,j]) %>%  filter(value > 0) 
players.sol = draftees.sol %>% get_solution(player[i]) %>%  filter(value > 0) 

draftees = points.nf.r[players.sol$i,c("player_name", "position", "Team", "round", "value.cdf")] %>% 
  mutate(round = as.integer(gsub("round","",round))) %>% 
  arrange(round)
View(draftees)
weekly = points.nf.r[weekly.sol$i,c("player_name", "position", "Team", "round")] %>% 
  mutate(round = as.integer(gsub("round","",round)))
weekly$week = weekly.sol$j
View(weekly)
table(draftees$position)
table(weekly$player_name)

