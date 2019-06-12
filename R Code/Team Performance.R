## looking at team performance vs spread

names(nfl_2016)
library(ggplot2)

nfl_2016 <- nfl_full[nfl_full$Season_id == 2016 ,]
nfl_2016 <- nfl_2016[ , -c(18)]
#nfl_2016 <- nfl_2016[, c(1,16,17,7,10,12,13,22,23)]


library(sqldf)

xx<-sqldf("
       SELECT a.*, 
             CASE WHEN a.score_diff > 0 THEN 'Win'
                  WHEN a.score_diff < 0 THEN 'Loss'
                  ELSE 'Tie' END as rec,
              CASE WHEN a.tm_line + a.score_diff > 0 THEN 'Cover'
                   WHEN a.tm_line + a.score_diff < 0 THEN 'Loss'
                   ELSE 'Push' END as vs_spread
       FROM (SELECT n.week, t.team_id, t.abbrev, n.date,
       CASE t.team_id WHEN n.fav_id THEN n.dog_abv
                     ELSE n.fav_abv END as opp,
       CASE t.team_id WHEN n.fav_id THEN n.fav_home
                     ELSE n.dog_home END as at_home,
       CASE t.team_id WHEN n.fav_id then n.line
                     ELSE n.line*-1 END as tm_line,
       CASE t.team_id WHEN n.w_id then floor(ptsw)||'-'||floor(ptsl)
                      ELSE floor(ptsl)||'-'||floor(ptsw) END as score,
       CASE t.team_id WHEN n.w_id then ptsw-ptsl
                      ELSE ptsl-ptsw END as score_diff
      FROM  nfl_2016 n, teams_2018 t
      WHERE t.team_id IN (n.fav_id, n.dog_id)
        /*and t.abbrev IN ('SF','SEA')*/
      ) a
      ORDER BY a.date
      ")

rec2016 <- as.data.frame.matrix(table(xx$abbrev, xx$rec))
rec_ats2016 <- as.data.frame.matrix(table(xx$abbrev, xx$vs_spread))

rec2016$Wpct <- round((rec2016$Win + 0.5*rec2016$Tie)/
                (rec2016$Win + rec2016$Loss + rec2016$Tie),3)
rec_ats2016$Wpct <- round((rec_ats2016$Cover + 0.5*rec_ats2016$Push)/
                    (rec_ats2016$Cover + rec_ats2016$Loss + rec_ats2016$Push),3)


comb_recs2016 <- cbind(rec_ats2016, rec2016)
comb_recs2016 <- comb_recs2016[,c(1:4,7,6,5,8)]
names(comb_recs2016) <- c("ats_cover", "ats_loss", "ats_push", "ats_wpct",
                          "rec_win", "rec_tie", "rec_loss", "rec_wpct")

# 1/ add text with geom_text, use nudge to nudge the text
ggplot(comb_recs2016, aes(x=rec_wpct, y=ats_wpct)) +
  geom_point() + 
  geom_label(label=rownames(comb_recs2016), color="black",nudge_y = 0.02, size=3)
  #geom_text(label=rownames(comb_recs2016), nudge_y = 0.02, check_overlap = T)

ggplot(data, aes(x=wt, y=mpg, fill=cyl)) +
  geom_label(label=rownames(data), color="white", size=5)



#strc("Game_id", "DATETIME", "Team_id", "Opp", "Home", "Line", "Total", "Own_score", "Opp_score")