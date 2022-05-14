library(dplyr)
library(ROI)
library(gurobi)
library(ROI.plugin.glpk)
library(ROI.plugin.symphony)
library(ROI.plugin.lpsolve)
library(ROI.plugin.gurobi)
library(ompr)
library(ompr.roi)
library(readr)
library(tidyr)

#capt_lineups <- read_csv("C:/Users/jerom/Downloads/DKSalaries (1).csv")
#projections <- read_csv("C:/Users/jerom/Downloads/projections.csv") %>% mutate(position = toupper(position))

week = 19
nf.read = read_csv(paste0("C:/Users/jerom/Downloads/week-",week,"-fantasy-football-player-projections.csv"), skip = 1)
nf_D.read = read_csv(paste0("C:/Users/jerom/Downloads/week-",week,"-fantasy-football-defense-special-teams-projections.csv"), skip = 1)
nf_K.read = read_csv(paste0("C:/Users/jerom/Downloads/week-",week,"-fantasy-football-k-projections.csv"), skip = 1)
names(nf.read)[c(3,16:21)] = c("Opp","FDpoints","FDcost","FDvalue","DKpoints","DKcost","DKValue")
names(nf_D.read)[c(4,13:18)] = c("Opp","FDpoints","FDcost","FDvalue","DKpoints","DKcost","DKValue")
names(nf_K.read)[c(4,15:20)] = c("Opp","FDpoints","FDcost","FDvalue","DKpoints","DKcost","DKValue")

nf.read$Team = trimws(gsub("\\)","",substr(nf.read$Player, nchar(nf.read$Player)-3, nchar(nf.read$Player))))
nf_D.read$Team = trimws(gsub("\\)","",substr(nf_D.read$Player, nchar(nf_D.read$Player)-3, nchar(nf_D.read$Player))))
nf_K.read$Team = trimws(gsub("\\)","",substr(nf_K.read$Player, nchar(nf_K.read$Player)-3, nchar(nf_K.read$Player))))

nf_D.read$Pos = "DST"
nf_K.read$Pos = "K"
nf.read$Pos = "RB"
for (p in c("QB","WR","TE") ){
  nf.read$Pos[grep(paste0("\\(",p,","), nf.read$Player, ignore.case = F)] = p
}
game = c("BUF","NE")
dk = read_csv("C:/Users/jerom/Downloads/DKSalaries (24).csv")
nf = nf.read %>% filter(Team %in% game)
nf_D = nf_D.read %>% filter(Team %in% game)
nf_K = nf_K.read %>% filter(Team %in% game)

nf = rbind(nf[,c("Player","Team","Pos","Opp","FDpoints","FDcost","FDvalue","DKpoints","DKcost","DKValue")],
           nf_K[,c("Player","Team","Pos","Opp","FDpoints","FDcost","FDvalue","DKpoints","DKcost","DKValue")])

source("name_match.R")
tmp2 = merge(tmp, name.match, by.x = "name", by.y = "nf.name")
tmp2 = merge(tmp2, dk, by.x = c("ID","Team"), by.y = c("ID","TeamAbbrev")) # ,"Team"
tmp2.d = merge(nf_D, dk[which(dk$Position=="DST"),], by.x = "Team", by.y = "TeamAbbrev")
projections = rbind(tmp2[,c("Name","ID","Pos","Roster Position","Team","FDpoints","FDcost","DKpoints","DKcost","Salary")], 
                    tmp2.d[,c("Name","ID","Pos","Roster Position","Team","FDpoints","FDcost","DKpoints","DKcost","Salary")])
gc()
no.play = c("NA")
projections = projections %>% 
  mutate(DKpoints = if_else(`Roster Position`=="CPT", 1.5*DKpoints, DKpoints)) %>% 
  filter(!Name %in% no.play) %>% 
  select(c(Name,ID,Pos,`Roster Position`,Team,DKpoints,Salary)) %>% 
  distinct()

write.csv(projections, paste0("projections_",week,".csv"),row.names = F)

n = nrow(projections)  
points = projections$DKpoints
cost = projections$Salary
cap = 50000

qb = which(projections$Pos=="QB")
rb = which(projections$Pos=="RB")
wr = which(projections$Pos=="WR")
te = which(projections$Pos=="TE")
dst = which(projections$Pos=="DST")
k = which(projections$Pos=="K")
flex = which(projections$`Roster Position`=="FLEX")
capt = which(projections$`Roster Position`=="CPT")
team1 = which(projections$Team==game[1])
team2 = which(projections$Team==game[2])

save.image()

#### single lineup ----
capt.base = MIPModel() %>%
  add_variable(player[i], i = 1:n, type = "binary") %>%
  set_objective(sum_expr(points[i] * player[i], i = 1:n), "max") %>%
  add_constraint(sum_expr(cost[i] * player[i], i = 1:n) <= cap) %>%
  #add_constraint(sum_expr(cost[i] * player[i], i = 1:n) >= 0.95*cap) %>%
  add_constraint(sum_expr(player[i], i = qb) <= 2) %>%
  add_constraint(sum_expr(player[i], i = dst) <= 1) %>%
  add_constraint(sum_expr(player[i], i = k) <= 1) %>%
  add_constraint(sum_expr(player[i], i = rb) <= 3) %>%
  add_constraint(sum_expr(player[i], i = wr) <= 3) %>%
  add_constraint(sum_expr(player[i], i = te) <= 2) %>%
  add_constraint(sum_expr(player[i], i = flex) == 5) %>%
  add_constraint(sum_expr(player[i], i = capt) == 1) 

for (t in unique(projections$Name)) {
  id = which(projections$Name==t)
  capt.base = add_constraint(capt.base, sum_expr(player[i], i = id) <= 1)
}

capt.lineup = solve_model(capt.base, with_ROI(solver = "gurobi")) %>% get_solution(player[i]) %>%  filter(value > 0)
projections[as.numeric(capt.lineup$i),] %>% arrange(`Roster Position`)

L = 50
lineup.list = showdown.l = list()
for (so in c("gurobi")) {
  pb = txtProgressBar(min = 0, max = L, initial = 0)
  t1 = Sys.time()
  capt.mult = capt.base
  for (l in 1:L) {
    capt.lineup = solve_model(capt.mult, with_ROI(solver = so)) %>% get_solution(player[i]) %>%  filter(value > 0)
    tmp = projections[as.numeric(capt.lineup$i),] %>% arrange(`Roster Position`)
    tmp$lineup = l
    lineup.list[[paste(l,sum(tmp$DKpoints),sum(tmp$Salary))]] = tmp
    capt.mult = add_constraint(capt.mult, sum_expr(player[i], i = capt.lineup$i) <= 5)
    setTxtProgressBar(pb,l)
  }
  print(paste(so, Sys.time()-t1))
  showdown.l[[so]] = do.call(rbind,lineup.list)
  showdown.l[[so]]$so = so
}
showdown = do.call(rbind,showdown.l)
showdown$so = ifelse(grepl("gurobi",rownames(showdown)), "gurobi","lpsolve")

s = showdown %>%
  group_by(so,lineup) %>%
  summarise(fantasyPoints = sum(DKpoints), 
            Salary = sum(Salary))
#### basis ----
table(showdown$Name)
showdown$one = 1
x = expand.grid(Name = unique(showdown$Name), lineup = 1:l) %>% left_join(showdown[,c("Name","lineup","one")])
x$one[which(is.na(x$one))] = 0

x = x %>% spread(lineup,one)# %>%
x.name = x$Name
x$Name = NULL
z = (s$Salary - min(s$Salary))/(max(s$Salary) - min(s$Salary))
z = z^(1/3)
x = rbind(x,z)
#v = as.data.frame(t(x))

nb = 20
w = 0
while(w < 20 & nb < l) {
  v.pc = prcomp(x, rank. = nb)
  basis = apply(abs(v.pc$rotation), 2, which.max)
  basis = basis[order(basis)]
  basis = unique(basis)
  w = length(basis)
  nb = nb+1
}
showdown.final = showdown[which(showdown$lineup %in% basis),] 
showdown.final$`Roster Position`[which(showdown.final$`Roster Position`=="FLEX")] = paste0(showdown.final$`Roster Position`[which(showdown.final$`Roster Position`=="FLEX")], 1:5)
showdown.csv = showdown.final[,c("lineup","ID","Roster Position")] %>% 
  pivot_wider(id_cols = lineup, names_from = `Roster Position`, values_from = ID) %>%
  select(-lineup)
table(showdown.final$Name, showdown.final$`Roster Position`)
table(showdown.final$Name)[order(-table(showdown.final$Name))]
s[basis,]
basis = c(1,2,basis)
names(showdown.csv) = c("CPT", rep("FLEX",5))
#write.csv(showdown.csv, "showdown.csv", row.names = F)
write.table(showdown.csv, "clipboard", sep="\t", row.names=FALSE)

v = t(x)
v.km = kmeans(v, centers = 20, iter.max = 500)
x.c = v.km$centers

d.ij = matrix(ncol = nrow(x.c), nrow = nrow(v))
for (v.i in 1:nrow(v)) {
  for (c.j in 1:nrow(x.c)) {
    tmp = data.frame(v = v[v.i,], x.c = x.c[c.j,])
    d.ij[v.i,c.j] = dist(t(tmp), method = "manhattan")
  }
}
basis = apply(d.ij, 2, which.min)
basis = basis[order(basis)]

showdown.final = showdown[which(showdown$lineup %in% basis),] 
showdown.final$`Roster Position`[which(showdown.final$`Roster Position`=="FLEX")] = paste0(showdown.final$`Roster Position`[which(showdown.final$`Roster Position`=="FLEX")], 1:5)
showdown.csv = showdown.final[,c("lineup","ID","Roster Position")] %>% 
  pivot_wider(id_cols = lineup, names_from = `Roster Position`, values_from = ID) %>%
  select(-lineup)
table(showdown.final$Name, showdown.final$lineup)
table(showdown.final$Name)[order(-table(showdown.final$Name))]
s[basis,]
basis = c(1,basis[-20])
names(showdown.csv) = c("CPT", rep("FLEX",5))
#write.csv(showdown.csv, "showdown.csv", row.names = F)
write.table(showdown.csv, "clipboard", sep="\t", row.names=FALSE)

#### double MIP ----
showdown

set.cover = MIPModel() %>% 
  add_variable(lineup[i], i = 1:l, type = "binary")

for (t in unique(showdown$Name)) {
  id = which(projections$Name==t)
  capt.mult = capt.mult %>% 
    add_constraint(sum_expr(player[i,l], i = id) <= 1, l = 1:20) %>%
    add_constraint(sum_expr(player[i,l], i = id, l = 1:20) <= exposure)
}
  

#### multi-lineup ----
solve.time = data.frame(n.lineups = integer(), exposure = integer(), time = numeric())
lineup.list = list()
for (e1 in 20) {
  n.lineups = 20
  print(n.lineups)
  for (e2 in c(0)) {
    exposure = n.lineups - 5
    print(exposure)
    #n.lineups = 5; exposure = 2
    capt.mult = MIPModel() %>%
      add_variable(player[i,l], i = 1:n, l = 1:n.lineups, type = "binary") %>%
      #add_variable(r, type = "continuous", lb = 0) %>%
      #set_objective(r, "max") %>%
      #add_constraint(r <= sum_expr(points[i] * player[i,l], i = 1:n), l = n.lineups) %>%
      set_objective(sum_expr(points[i] * player[i,l], i = 1:n, l = 1:n.lineups)) %>% 
      add_constraint(sum_expr(points[i] * player[i,l], i = 1:n) - sum_expr(points[i] * player[i,(l+1)], i = 1:n) >= 0.05, l = 1:(n.lineups-1)) %>%
      add_constraint(sum_expr(cost[i] * player[i,l], i = 1:n) <= cap, l = 1:n.lineups) %>%
      add_constraint(sum_expr(player[i,l], i = qb) <= 2, l = 1:n.lineups) %>%
      add_constraint(sum_expr(player[i,l], i = dst) <= 1, l = 1:n.lineups) %>%
      add_constraint(sum_expr(player[i,l], i = rb) <= 3, l = 1:n.lineups) %>%
      add_constraint(sum_expr(player[i,l], i = wr) <= 3, l = 1:n.lineups) %>%
      add_constraint(sum_expr(player[i,l], i = te) <= 2, l = 1:n.lineups) %>%
      add_constraint(sum_expr(player[i,l], i = k) <= 2, l = 1:n.lineups) %>%
      add_constraint(sum_expr(player[i,l], i = flex) == 5, l = 1:n.lineups) %>%
      add_constraint(sum_expr(player[i,l], i = capt) == 1, l = 1:n.lineups) %>% 
      add_constraint(sum_expr(player[i,l], i = team1) <= 5, l = 1:n.lineups) %>% 
      add_constraint(sum_expr(player[i,l], i = team2) <= 5, l = 1:n.lineups)
    
    # for (l1 in 1:n.lineups) {
    #   for(l2 in 1:n.lineups) {
    #     if(l1 != l2) {
    #       capt.mult = capt.mult %>%
    #         add_constraint(player[i,l] + player[i,k] <= 2*comp[i,l,k], l = 1:n.lineups, i = 1:n) %>% 
    #         add_constraint(sum_expr(comp[i,l,k], i = 1:n) >= 7, l = l1, k = l2)
    #     }
    #   }
    # }
    
    for (t in unique(projections$Name)) {
      id = which(projections$Name==t)
      if (length(id)==2) {
        capt.mult = capt.mult %>% 
          add_constraint(sum_expr(player[i,l], i = id) <= 1, l = 1:n.lineups) %>%
          add_constraint(sum_expr(player[i,l], i = id, l = 1:n.lineups) <= exposure)
      } else {
        print(t)
      }
    }
    
#    for (solver in c("gurobi")) { #,
      #print(solver)
      t1 = Sys.time(); capt.mult.lineup = solve_model(capt.mult, with_ROI(solver = "gurobi", verbose = TRUE)) ; save.image()
      solve.time = rbind(solve.time, data.frame(n.lineups = n.lineups, exposure = exposure, time = Sys.time() - t1))
      write.csv(solve.time, "solve_times.csv", row.names = F)
#    }
    lineup.list[paste(e1,e2)] = capt.mult.lineup
  }
}

capt.mult.lineup = solve_model(capt.mult, with_ROI(solver = "gurobi", verbose = TRUE))

ggplot(data = solve.time %>% filter(n.lineups==exposure), aes(x = n.lineups, y = time/60)) + geom_point() + geom_line() + 
  scale_x_continuous(breaks = 7:20, minor_breaks = NULL, name = "Number of Lineups Created") + 
  scale_y_continuous(name = "Solve Time") + 
  labs(title = "Gurobi Solve Time (in Minutes)", subtitle = "Exposure Constraint effectively turned off (i.e. nlineups = exposure)")

ggplot(data = solve.time %>% filter(n.lineups %in% 8:15), aes(x = exposure, y = time/60)) + geom_point() + geom_line() + 
  scale_x_continuous(breaks = 1:20, minor_breaks = NULL, name = "Exposure Limit") + 
  scale_y_continuous(name = "Solve Time") + 
  facet_wrap(.~n.lineups, nrow = 4, scales = "free_y")+
  labs(title = "Gurobi Solve Time (in Minutes)", subtitle = "Exposure constraint is turned ON (i.e. nlineups > exposure)")


X = projections[as.numeric(capt.mult.lineup$i),] %>% mutate(lineup = rep(c(1:n.lineups), 6)[order(rep(c(1:n.lineups), 6))])
X %>% group_by(lineup) %>% 
  summarise(lineup.points = sum(DKpoints))
#### exponential decay ----
l = -log(.025)/250
p = seq(1,250, 13)/250
ceiling(-log(1-p)/l)
basis = c(ceiling(-log(1-p)/l)[-20],250)

basis = c()

showdown.final = showdown[which(showdown$lineup %in% basis),] 
showdown.final$`Roster Position`[which(showdown.final$`Roster Position`=="FLEX")] = paste0(showdown.final$`Roster Position`[which(showdown.final$`Roster Position`=="FLEX")], 1:5)
showdown.csv = showdown.final[,c("lineup","ID","Roster Position")] %>% 
  pivot_wider(id_cols = lineup, names_from = `Roster Position`, values_from = ID) %>%
  select(-lineup)
table(showdown.final$Name, showdown.final$lineup)
table(showdown.final$Name)[order(-table(showdown.final$Name))]
s[basis,]
basis = c(1,basis[-20])
names(showdown.csv) = c("CPT", rep("FLEX",5))
#write.csv(showdown.csv, "showdown.csv", row.names = F)
write.table(showdown.csv, "clipboard", sep="\t", row.names=FALSE)

#### two-phase ----
points = s$fantasyPoints
n = length(unique(showdown$Name))
l = 50
l = nrow(v)
lineups = v[1:l,1:n]
exposure = 18
two.phase = MIPModel() %>%
  add_variable(lineup[i,p], i = 1:l, p = 1:n, type = "binary") %>%
  add_variable(dec[i], i = 1:l, type = "binary") %>%
  add_variable(expo, type = "continuous") %>%
  #set_objective(sum_expr(points[i] * dec[i], i = 1:l), "max") %>%
  set_objective(expo, "min") %>%
  add_constraint(6*dec[i] <= sum_expr(lineup[i,p], p = 1:n), i = 1:l) %>%
  add_constraint(lineup[i,p] <= lineups[i,p], i = 1:l, p = 1:n) %>%
  add_constraint(sum_expr(dec[i], i = 1:l) == 20) %>%
  add_constraint(sum_expr(lineup[i,p], i = 1:l) <= expo, p = 1:n)

t1 = Sys.time(); sol = solve_model(two.phase, with_ROI(solver = "gurobi")); Sys.time() - t1
sol %>% get_solution(expo) 

write.csv(v[,1:n], "lineups")
write.table(v[,1:n], "clipboard", sep="\t", row.names=FALSE, col.names = F)
write.table(points, "clipboard", sep="\t", row.names=FALSE, col.names = F)

gurobi_write(capt.mult, "capt_mult.mps")


A = 