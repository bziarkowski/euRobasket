#'Download assists from live.fibaeurope.com using game id
#'
#'@param gameid numeric, game id from live.fibaeurope.com
#'
#'@return data.frame with assists
#'
#'@seealso \code{\link[euRobasket]{get_assists_fibalivestats}}, @seealso \code{\link[euRobasket]{assist_network}}
#'
#'@examples
#'get_assists_livefibaeurope(742430)
#'

get_assists_livefibaeurope = function(gameid) {

#function for shots
get_shots = function(gameid) {

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

  shots = shots[,c(1,7,2,3,4,6,5)]

  return(shots)

}

#load shots
shots = get_shots(gameid)

#load play by play
pbp = get_raw_pbp_livefibaeurope(gameid)

#subset home shots and away shots
home_shots = shots[shots$team == unique(pbp$home_team),]
away_shots = shots[shots$team == unique(pbp$away_team),]

#get home assists and away assists
home_assists = pbp[which(grepl('assist', pbp$home_sub_action)),][,c(1,2,7)]
away_assists = pbp[which(grepl('assist', pbp$away_sub_action)),][,c(1,2,9)]

home_assists[,3] = gsub(' -.*', '', home_assists[,3])
away_assists[,3] = gsub(' -.*', '', away_assists[,3])

#merge shots with assists
assists_home = merge(home_shots, home_assists, by.x = c('quarter', 'time'), by.y = c('period', 'gt'))
names(assists_home)[8] = 'assist'
assists_away = merge(away_shots, away_assists, by.x = c('quarter', 'time'), by.y = c('period', 'gt'))
names(assists_away)[8] = 'assist'

#combine assists
assists = rbind(assists_home, assists_away)

#order columns and change names
assists = assists[,c(6, 4, 5, 7, 8)]

names(assists)[1] = 'shot'

return(assists)
}
