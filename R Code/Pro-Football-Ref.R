
pfr_sched <- c()

url_pfr <- "https://www.pro-football-reference.com/years/"
for (yr_pfr in 2005:2017) {
pfr <- read_html(paste0(url_pfr,yr_pfr,"/games.htm"))
pfr %>%
  html_nodes(xpath = '///*[@id="games"]') %>%
  html_table() -> pfr_temp
pfr_temp <- pfr_temp[[1]]
pfr_temp$Season_id <- yr_pfr
names(pfr_temp) <- c("Week", "Day", "Date", "Time", "Winner/tie", "at", "Loser/tie", "box", 
                     "PtsW", "PtsL", "YdsW", "TOW", "YdsL", "TOL", "Season_id")
pfr_temp <- pfr_temp[pfr_temp$box != "",]
pfr_sched <- rbind(pfr_sched, pfr_temp)
}

pfr_sched$DATE <- NA

for (i in 1:nrow(pfr_sched)) {
   if (format(as.Date(pfr_sched$Date[i],'%b %d'), '%m') <= '02') {
      pfr_sched$DATE[i] <- as.Date(paste0(pfr_sched$Date[i], ' ', pfr_sched$Season_id[i]+1), format = "%b %d %Y")
    } else {
      pfr_sched$DATE[i] <- as.Date(paste0(pfr_sched$Date[i], ' ', pfr_sched$Season_id[i]), format = "%b %d %Y")
    }
}
pfr_sched$DATE <- as.Date(pfr_sched$DATE, origin = "1970-01-01")

# merge using aggregate to solve cases with multiple matches of same ID
# create aggregate of city with rk = 1 referring to most recent team in that city
team_agg <- teams %>% arrange(Tm, Year_To) %>%
  group_by(Tm) %>% 
  mutate(rank = rank(-Year_To, ties.method = "first"))
team_agg2 <- team_agg[team_agg$rank == 1,]

pfr_sched2 <- merge(pfr_sched, team_agg2[,c(1,2)], by.x="Winner/tie", by.y = "Tm", all.x= T, sort = F)
pfr_sched2 <- merge(pfr_sched2, team_agg2[,c(1,2)], by.x="Loser/tie", by.y = "Tm", all.x = T, sort = F)

pfr_sched2$Game_id <- ifelse(pfr_sched2$team_id.x < pfr_sched2$team_id.y,
                             paste(format(pfr_sched2$DATE, '%Y%m%d'), pfr_sched2$team_id.x, pfr_sched2$team_id.y, sep="_"),
                             paste(format(pfr_sched2$DATE, '%Y%m%d'), pfr_sched2$team_id.y, pfr_sched2$team_id.x, sep="_"))

pfr_sched2 <- pfr_sched2[, c(15,3:6,2,17,7,1,18,8:14,16,19)]
names(pfr_sched2)[c(7,10)] <- c("W_id", "L_id")

nfl_full <- merge(nfl_played2, pfr_sched2[,c(1,7,10,19,12,13)], by = "Game_id", all.x = T)

# mislabeled date in betting table
nfl_full[is.na(nfl_full$PtsW),]

nfl_full[1095,18] <- nfl_full[1095,18]+60*60*24
nfl_full[1095,c(1,5,16,19:23)] <- c("20100920_22_28", "9/20 8:20 ET", "2010-09-19", 
                                     2010, 22, 28, 25, 22)

nfl_full$PtsW <- as.numeric(nfl_full$PtsW)
nfl_full$PtsL <- as.numeric(nfl_full$PtsL)

nfl_full$Week <- factor(nfl_full$Week, levels = c("1","2","3","4","5","6","7","8",
                                                    "9","10","11","12","13","14","15",
                                                    "16","17","WC","Div","Conf","SB"))

# table is off on 2016 - Week 15 (can't be 17 games in a week)
table(nfl_full$Week, nfl_full$Season_id)
nfl_full[nfl_full$Week == '15' & nfl_full$Season_id == 2016,]

# NE played twice in Week 15 once on 12/12 and again on 12/18, not possible.
pfr_sched2[(pfr_sched2$W_id == 21 | pfr_sched2$L_id == 21) &
           (pfr_sched2$Week == '14' | pfr_sched2$Week == '15') &
            pfr_sched2$Season_id == 2016,]

# 12/12/18 was a Monday game in Week 14. The Week 15 listed game is incorrect.
nfl_full[(nfl_full$Fav_id == 21 | nfl_full$Dog_id == 21) &
             (nfl_full$Week == '14' | nfl_full$Week == '15') &
              nfl_full$Season_id == 2016,]  

row_to_delete = which(nfl_full$Game_id == '20161212_3_21' & nfl_full$Week == '15')
nfl_full <- nfl_full[-row_to_delete,]

# correct table
table(nfl_full$Week, nfl_full$Season_id)

write.csv(nfl_full, file = "nfl_full.csv" , row.names = F)
#nfl_full <- read.csv("nfl_full.csv", stringsAsFactors = F)

