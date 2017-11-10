#'Load shots data from live.fibaeurope.com with gameid
#'
#'@param gameid game ID from live.fibaeurope.com, numeric
#'
#'@return data frame with shots
#'
#'@examples
#'#Load shots from game 742430
#'get_shots_data_livefibaeurope(742430)
#'
#'@seealso \code{\link[euRobasket]{get_shots_data_fibalivestats}}
#'



get_shots_data_livefibaeurope = function(gameid) {

#Load from live.fibaeurope.com
url <- paste("http://live.fibaeurope.com/www/ShotChart.ashx?acc=5&lng=en&gameID=",gameid,"", sep = "")
dat = xmlToList(xmlTreeParse(url))

#extract shots

shots <- data.frame(matrix(unlist(dat$SHOTCHART$SHOTS), ncol=7, byrow = TRUE))

#rename columns

colnames(shots) <- c("outcome", "time", "x", "y", "team", "player", "quarter")

#get team names

home_team = matrix(unlist(dat$HEADER[1]), ncol=4, byrow = TRUE)[,1]
away_team = matrix(unlist(dat$HEADER[2]), ncol=4, byrow = TRUE)[,1]

#get players list

dat$SHOTCHART[[2]]$.attrs = NULL
dat$SHOTCHART[[1]]$.attrs = NULL

home_players = data.frame(matrix(unlist(dat$SHOTCHART[[1]]), ncol = 8, byrow = TRUE))[,1:2]
away_players = data.frame(matrix(unlist(dat$SHOTCHART[[2]]), ncol = 8, byrow = TRUE))[,1:2]

#define function for multiple gsub

players_sub = function(players_to_sub, players_list) {
  players_list$X1 = as.character(players_list$X1)
  players_list$X2 = as.character(players_list$X2)
  players = c()
  for(i in 1:nrow(players_to_sub)) {
    players[i] = players_list$X1[players_list$X2 == as.character(players_to_sub$player[i])]
  }
  return(players)
}

#add teams names and players names to shots

shots_home = shots[shots$team == 0,]
shots_away = shots[shots$team == 1,]

shots_home$player = players_sub(shots_home, home_players)
shots_away$player = players_sub(shots_away, away_players)

shots_home$team = home_team
shots_away$team = away_team

shots = rbind(shots_home, shots_away)

#convert column outcome to factor, and x, y to numeric
shots$outcome[shots$outcome == 2] = 0
shots$outcome = factor(shots$outcome,
                       levels = c(0,1),
                       labels = c('made', 'missed'))

shots$x = as.numeric(levels(shots$x))[shots$x]
shots$y = as.numeric(levels(shots$y))[shots$y]

#convert shot coordinates

shots$ref_x = shots$x - 750

shots1 = shots[shots$ref_x <= 0,]
shots2 = shots[shots$ref_x > 0,]

shots1$x = 750-shots1$ref_x
shots2$x = 750-shots2$ref_x

shots = rbind(shots1, shots2)

#select and orders columns

shots = shots[,c(1,7,3,4,6,5)]

return(shots)

}
