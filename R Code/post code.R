

url <- "http://www.footballlocks.com/nfl_lines_"  # base part of url to scrape
post_lines <- c()  # data frame to hold all lines

post_names <- c("wild_card_playoff_games","divisional_playoff_games",
                "conference_championship_playoff_games","super_bowl" )
post_abbrev <- c("WC", "Div", "Conf", "SB")



for (p in 1:length(post_names)) {
  url <- paste0(url,post_names[p],".shtml") # 
  
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
