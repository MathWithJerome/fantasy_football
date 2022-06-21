library(readxl)
pfr_fan = read_excel("pfr_data_pulls.xls", sheet = "fantasy")
pfr_fan$pro.bowl = grepl("\\*",pfr_fan$Player)
pfr_fan$all.pro = grepl("\\+",pfr_fan$Player)
pfr_fan$Player = trimws(gsub("[\\*\\+]", "", pfr_fan$Player))

pfr_scrim = read_excel("pfr_data_pulls.xls", sheet = "scrimmage")
pfr_scrim$Player = trimws(gsub("[\\*\\+]", "", pfr_scrim$Player))

pfr = merge(pfr_fan, pfr_scrim[,c("uid","Year",setdiff(names(pfr_scrim), names(pfr_fan)))], by = c("uid","Year"), all = T)
pfr = pfr[order(-pfr$Year, pfr$Rk),]

rec_adv = read_excel("pfr_data_pulls.xls", sheet = "rec_advanced")
rec_adv$Player = trimws(gsub("[\\*\\+]", "", rec_adv$Player))
pfr = merge(pfr, rec_adv[c("uid","Year","YBC","YBC/R","YAC","YAC/R","BrkTkl","Rec/Br","Drop","Drop%")], by = c("uid","Year"), all.x = T)

pfr = pfr %>% 
  group_by(FantPos, Year) %>%
  mutate(PPRrank = rank(-PPR, ties.method = "min")) %>% 
  filter(!is.na(Player))


#"BeckOd00",
browns.wr = pfr.wr[which(pfr.wr$uid %in% c("LandJa00")),-c(1, 3, grep("Passing", names(pfr)), grep("Rushing", names(pfr)))]

x = wr %>% filter(Year == 2019) %>%
  group_by(Player) %>%
  summarise(mean.ppr = mean(PPR, na.rm = T), 
            median.ppr = median(PPR, na.rm = T), 
            sd.ppr = sd(PPR, na.rm = T),
            total.rec = sum(ReceivingRec), 
            cv = sd.ppr/mean.ppr) %>%
  filter(total.rec > 40)


  