#'Download play by play from live.fibaeurope.com using gameid
#'
#'@param gameid numeric, game id from live.fibaeurope.com
#'
#'@return data.frame with play by play data
#'
#'@seealso \code{\link[euRobasket]{get_raw_pbp_fibalivestats}}\code{\link[euRobasket]{get_stints_livefibaeurope}}
#'
#'
#'@examples
#'#Download play by play for game 412182
#'get_raw_pbp_livefibaeurope(412182)

get_raw_pbp_livefibaeurope = function(gameid) {

#define url for play by play
url <- paste("http://live.fibaeurope.com/www/PlayByPlay.ashx?acc=5&gameID=",gameid,"&from=1", sep = "")

#download play by play
dat = xmlToList(xmlParse(url))
pbp_list = dat$PLAYBYPLAY

#remove last element
pbp_list[length(pbp_list)] = NULL

#add columns to if missing
add_cols <- function(pbp_list) {
    g <- length(pbp_list)
    length(pbp_list) = 13
    return(pbp_list)
}
pbp_list = lapply(pbp_list, add_cols)


#pbp to data.frame
pbp = data.frame(matrix(unlist(pbp_list), ncol = 13, byrow = TRUE))

#revers pbp
pbp = pbp[rev(rownames(pbp)),]

#remove columns 1,4,8,9
pbp = pbp[,-c(1,4,8,9)]

#first ten rows of score to NA
pbp$X12[1:10] = NA
pbp$X13[1:10] = NA

#score to numeric
pbp$X13 = as.numeric(levels(pbp$X13))[pbp$X13]
pbp$X12 = as.numeric(levels(pbp$X12))[pbp$X12]

#change names and column order
pbp = pbp[,c(4,5,3,8,9,1,6,2,7)]

names(pbp) = c('gt',
               'period',
               'team',
               'home_score',
               'away_score',
               'home_action',
               'home_sub_action',
               'away_action',
               'away_sub_action')

#fill home_score and away_score using tidyr::fill()
pbp$home_score[1] = 0
pbp$away_score[1] = 0

pbp = pbp %>%
  fill(home_score) %>%
  fill(away_score)

#add teams names
pbp$home_team = dat$HEADER[[1]][1]
pbp$away_team = dat$HEADER[[2]][1]

return(pbp)
}


