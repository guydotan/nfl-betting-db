####################################################
##             Guy Dotan - 403-882-450            ##
##            Stats 405 - Final Project           ##
##                    06/16/2018                  ##
####################################################


## Set working directory folder
setwd("~/Documents/UCLA MAS/2018 Spring/STATS 405/Final Project")

## Load packages
library("rvest")
library("stringr")
library("XML")
library("data.table")
library("dplyr")

url <- "http://www.footballlocks.com/nfl_lines_"  # base part of url to scrape
reg_lines <- c()  # data frame to hold all regular season lines

## REGULAR SEASON ##
# outer loop for selecting correct url based on week #1-17
for (wk in 1:17) {
  url_reg <- paste0(url,"week_",wk,".shtml") 
  
  # read in HTML table using XML package
  wk_num <- readHTMLTable(url_reg, stringsAsFactors=FALSE)
  #str(wk1, list.len = 2)
  Week_Num <- wk_num[[1]]
  
  # isolate boundary rows for each season
  cutoffs <- which(Week_Num$V1 == "Date & Time")
  last_cutoff <- max(str_which(Week_Num$V2,"<!--//")) - 1 ## last table cutoff
  cutoffs <- c(cutoffs, last_cutoff)
  
  # inner loop to break down tables season by season
  for (i in 1:(length(cutoffs)-1)){
    temp <- Week_Num[ c( (cutoffs[i]-1):(cutoffs[i+1]-2) ) , ]  # select one week of one season
    temp$Year <- str_extract(temp[1,], "\\b[0-9][0-9][0-9][0-9]\\b")[1]  # isolate season name
    temp$Week <- wk  # isolate week number
    temp$Note <- sub( '\n\n.*', '', Week_Num[ cutoffs[i]-1,] )[1]  # make a note of the table header
    temp <- temp[-1,] # remove season row 
    temp <- temp[,c(22,23,1,2,3,4,5,24)]  # eliminate unneeded columns 
    names(temp)[3:7] <- temp[1,3:7]  # rename headers
    temp <- temp[-1,] # remove header row
    temp <- temp[grep("[0-9]",temp$`Date & Time`),] # eliminate blank row
    
    reg_lines <- rbind(reg_lines, temp)  # add to data frame
  }
}

## POST SEASON ##
post_lines <- c() # data frame to hold all postseason lines
post_names <- c("wild_card_playoff_games","divisional_playoff_games",
                "conference_championship_playoff_games","super_bowl" )
post_abbrev <- c("WC", "Div", "Conf", "SB")

for (p in 1:length(post_names)) {
  url_post <- paste0(url,post_names[p],".shtml") # 
  
  # read in HTML table using XML package
  p_num <- readHTMLTable(url_post, stringsAsFactors=FALSE)
  #str(wk1, list.len = 2)
  Post_Num <- p_num[[1]]
  
  # isolate boundary rows for each season
  cutoffs_post <- which(Post_Num$V1 == "Date & Time")
  last_cutoff_post <- max(str_which(Post_Num$V2,"<!--//")) - 1 ## last table cutoff
  cutoffs_post <- c(cutoffs_post, last_cutoff_post)
  
  # inner loop to break down tables season by season
  for (i in 1:(length(cutoffs_post)-1)){
    temp2 <- Post_Num[ c( (cutoffs_post[i]-1):(cutoffs_post[i+1]-2) ) , ]  # select one week of one season
    temp2$Year <- str_extract(temp2[1,], "\\b[0-9][0-9][0-9][0-9]\\b")[1]  # isolate season name
    temp2$Week <- post_abbrev[p]  # isolate week number
    temp2$Note <- sub( '\n\n.*', '', Post_Num[ cutoffs[i]-1,] )[1]  # make a note of the table header
    temp2 <- temp2[-1,] # remove season row 
    temp2 <- temp2[,c(22,23,1,2,3,4,5,24)]  # eliminate unneeded columns 
    names(temp2)[3:7] <- temp2[1,3:7]  # rename headers
    temp2 <- temp2[-1,] # remove header row
    temp2 <- temp2[grep("[0-9]",temp2$`Date & Time`),] # eliminate blank row
    
    post_lines <- rbind(post_lines, temp2)  # add to data frame
  }
}

# 2018 SB listed on site but on a different page for some reason, manually inserted below
sb2018 <- c("2018", "SB", "2/4 6:30 ET", "New England", "-4.5", "Philadelphia", "49", NA, "post")
post_lines <- rbind(post_lines, sb2018)

reg_lines$Game_Type <- "reg"
post_lines$Game_Type <- "post"

# full table (reg + post)
nfl_lines <- rbind(reg_lines, post_lines)

# save file so I don't have to constantly rescrape the site
write.csv(nfl_lines, file = "nfl_lines.csv", row.names = F)

#nfl_lines <- read.csv("nfl_lines.csv", stringsAsFactors = F)

### Table Cleaning ###
nfl_clean <- nfl_lines

## Team Names ##

# Favorite
nfl_clean$FAVORITE <- str_replace_all(nfl_clean$Favorite, 'At ', "")  # only include team (city) name
nfl_clean$FAVORITE <- str_replace_all(nfl_clean$FAVORITE, "\n.*", "") # remove new line text
nfl_clean$FAVORITE <- str_replace_all(nfl_clean$FAVORITE, "\\(.*", "") # remove parenthesis (venue) text
nfl_clean$Fav_Home <- ifelse(grepl('At ', nfl_clean$Favorite),'Y','N')  # identify if favorite is home/away

# Underdog
nfl_clean$UNDERDOG <- str_replace_all(nfl_clean$Underdog, 'At ', "")  # only include team (city) name
nfl_clean$UNDERDOG <- str_replace_all(nfl_clean$UNDERDOG, "\n.*", "") # remove new line text
nfl_clean$UNDERDOG <- str_replace_all(nfl_clean$UNDERDOG, "\\(.*", "") # remove parenthesis (venue) text
nfl_clean$Dog_Home <- ifelse(grepl('At ', nfl_clean$Underdog),'Y','N')  # identify if underdog is home/away

# should be just 32 team names but we see some inconsistently labeled teams 
table(nfl_clean$FAVORITE)
table(nfl_clean$UNDERDOG)

# fix two messed up team names
nfl_clean[ nfl_clean$UNDERDOG == "JacksonvilleLondonsize =-1>" , ]$UNDERDOG <- "Jacksonville"
nfl_clean[ nfl_clean$UNDERDOG == "Miami " , ]$UNDERDOG <- "Miami"

# should be 32 NFL teams but is at 35
length(table(nfl_clean$FAVORITE))
length(table(nfl_clean$UNDERDOG))

# change all Los Angeles to LA Rams (Los Angeles is used as team name for 2016 season when Rams were only LA team)
nfl_clean[ nfl_clean$FAVORITE == "Los Angeles" , ]$FAVORITE <- "LA Rams"  
nfl_clean[ nfl_clean$UNDERDOG == "Los Angeles" , ]$UNDERDOG <- "LA Rams"

# csv from https://www.pro-football-reference.com/teams/
teams <- read.csv("NFL Teams.csv", stringsAsFactors = F, skip = 1)
names(teams)[c(2,3)] <- c("Year_From", "Year_To")

# assign team_ids to all 32 active teams
teams$team_id = 1
for (i in 2:nrow(teams)) {
  teams[i,]$team_id <- ifelse(teams[i,]$AV == "",  teams[i-1,]$team_id, (teams[i-1,]$team_id)+1)
}

# split team name column at the last space (works because all NFL team names are just one word)
teams$LOCATION <- do.call(rbind, strsplit(teams$Tm, ' (?=[^ ]+$)', perl=TRUE))[,1]
teams$NICKNAME <- do.call(rbind, strsplit(teams$Tm, ' (?=[^ ]+$)', perl=TRUE))[,2]

teams <- teams[,c(18,1,19,20,2:17)]

# Since AFL existance in 1960 (AFL-NFL merger in 1970)
recent_teams <- teams[teams$Year_From >= 1960 | teams$Year_To == 2018,]

# merge using aggregate to solve cases with multiple matches of same ID
# create aggregate of city with rk = 1 referring to most recent team in that city
city_agg <- recent_teams %>% arrange(LOCATION, Year_To) %>%
  group_by(LOCATION) %>% 
  mutate(rank = rank(-Year_To, ties.method = "first"))
city_agg2 <- city_agg[city_agg$rank == 1,]


nfl_clean2 <- merge(nfl_clean, city_agg2[,c(1,3)]  , by.x=c("FAVORITE"), by.y=c("LOCATION"), all.x=TRUE)
nfl_clean2 <- merge(nfl_clean2, city_agg2[,c(1,3)] , by.x=c("UNDERDOG"), by.y=c("LOCATION"), all.x=TRUE)

# see which team's had blank IDs
table(nfl_clean2[is.na(nfl_clean2$team_id.x),]$FAVORITE)
table(nfl_clean2[is.na(nfl_clean2$team_id.y),]$UNDERDOG)

# manually assign IDs because naming convention is off for NY/LA teams.
recent_teams[recent_teams$NICKNAME %in% c("Chargers", "Rams","Giants","Jets"),c(1:4)]
nfl_clean2[nfl_clean2$FAVORITE == "LA Chargers",]$team_id.x <- 17
nfl_clean2[nfl_clean2$FAVORITE == "LA Rams",]$team_id.x <- 18
nfl_clean2[nfl_clean2$FAVORITE == "NY Giants",]$team_id.x <- 23
nfl_clean2[nfl_clean2$FAVORITE == "NY Jets",]$team_id.x <- 24
nfl_clean2[nfl_clean2$UNDERDOG == "LA Chargers",]$team_id.y <- 17
nfl_clean2[nfl_clean2$UNDERDOG == "LA Rams",]$team_id.y <- 18
nfl_clean2[nfl_clean2$UNDERDOG == "NY Giants",]$team_id.y <- 23
nfl_clean2[nfl_clean2$UNDERDOG == "NY Jets",]$team_id.y <- 24

nfl_clean3 <- nfl_clean2[ , c(3,11,4,5,2,14,1,15,7,9,12,13)]
names(nfl_clean3) <-  c("Year", "Game_Type", "Week", "Date_Time", "Favorite", "Fav_id", "Underdog", 
                        "Dog_id", "Line", "Total", "Fav_Home", "Dog_Home")

## Format Date and Time ##
# Label all postponed games
nfl_clean3[grep(c("postponed|ppd|delay"), tolower(nfl_clean3$Date_Time)),]$Date_Time <- "Postponed/Delayed" 

# fix any incorrectly enterred time (e.g. "1;00" instead of "1:00")
nfl_clean3$Date_Time <- gsub(';',':',nfl_clean3$Date_Time)

# Select only played games 
nfl_played <- nfl_clean3[nfl_clean3$Date_Time != 'Postponed/Delayed',]

# create date colum
nfl_played$DATE <- as.Date(paste0(gsub( " .*$", "", nfl_played$Date_Time ),
                                  '/', nfl_played$Year), format = "%m/%d/%Y")

# create time column - select 2nd element of time list and append AM/PM
nfl_played$TIME <- paste(sapply(strsplit(nfl_played$Date_Time," "),"[[", 2),
                         ifelse(sapply(strsplit(nfl_played$Date_Time," "),"[[", 3)=='AM','AM','PM'))

# combine into a date-time format
nfl_played$DATETIME <- strptime(paste(nfl_played$DATE, nfl_played$TIME),  "%Y-%m-%d %I:%M %p",  tz = "EST5EDT")


# Convert to numeric columns
nfl_played[nfl_played$Line == 'PK',]$Line <- 0
nfl_played$Line <- as.numeric(nfl_played$Line)
nfl_played$Total <- as.numeric(nfl_played$Total)

# Create a game_id in order to merge with other data sets - format is yearmonthday_lowerid_higherid
nfl_played$Game_id <- ifelse(nfl_played$Dog_id < nfl_played$Fav_id ,
                             paste(format(nfl_played$DATE, '%Y%m%d'), nfl_played$Dog_id, nfl_played$Fav_id, sep="_"),
                             paste(format(nfl_played$DATE, '%Y%m%d'), nfl_played$Fav_id, nfl_played$Dog_id, sep="_"))



# Scrape team abbreviations chart from Wikipedia
wiki <- read_html("https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Football_League/National_Football_League_team_abbreviations")
wiki %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table() -> tm_abv
tm_abv <- tm_abv[[1]]
tm_abv <- tm_abv[-1,]
names(tm_abv) <- c("Abbrev","Name")

# Merge it with a current (2018) teams list
teams_2018 <- recent_teams[recent_teams$Year_To == 2018 & recent_teams$AV != "" ,] 
teams_2018 <- merge(teams_2018, tm_abv, by.x = "Tm", by.y = "Name")

nfl_played2 <- merge(nfl_played, teams_2018[,c(2,21)], by.x="Fav_id", by.y = "team_id",sort = F)
nfl_played2 <- merge(nfl_played2, teams_2018[,c(2,21)], by.x="Dog_id", by.y = "team_id",sort = F)

nfl_played2 <- nfl_played2[ , c(3:7,2,17,8,1,18,9:16)]
names(nfl_played2)[c(7,10)] <- c("Fav_abv", "Dog_abv")

nfl_played2 <- nfl_played2[order(nfl_played2$DATETIME, decreasing = T), ]

write.csv(nfl_played2, file = "nfl_played.csv", row.names = F )
