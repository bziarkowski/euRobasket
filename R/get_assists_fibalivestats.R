#'Download assists from fibalivestats.com using game id
#'
#'@param gameid numeric, game id from fibalivestats.com
#'
#'@return data.frame with assists
#'
#'@seealso \code{\link[euRobasket]{get_assists_livefibaeurope}}, @seealso \code{\link[euRobasket]{assist_network}}
#'
#'@examples
#'get_assists_fibalivestats(742430)
#'

get_assists_fibalivestats = function(gameid) {

  #load shots and play by play

  shots = get_shots_data_fibalivestats(gameid)

  pbp = get_raw_pbp_fibalivestats(gameid)

  #subset away and home pbp

  home_pbp = pbp[pbp$tno == 1,]
  away_pbp = pbp[pbp$tno == 2,]

  #extract shots actions

  home_shots_time = home_pbp$gt[home_pbp$actionType %in% c('2pt', '3pt')]
  away_shots_time = away_pbp$gt[away_pbp$actionType %in% c('2pt', '3pt')]

  #subset home and away shots with team name

  shots_home = shots[shots$team == unique(home_pbp$team_name),]
  shots_away = shots[shots$team == unique(away_pbp$team_name),]

  #check if number of rows for shots_home and shots_away bigger than 0,
  #otherwise subset with team short name

  if(nrow(shots_home)==0) {
    shots_home = shots[shots$team == unique(home_pbp$team_short_name),]
  }

  if(nrow(shots_away)==0) {
    shots_away = shots[shots$team == unique(away_pbp$team_short_name),]
  }

  #arrange shots by quarter

  shots_away = arrange(shots_away, quarter)
  shots_home = arrange(shots_home, quarter)

  #add time to shots from play by play

  shots_home$time = home_shots_time
  shots_away$time = away_shots_time

  #combine shots and get rows numbers with shots from play by play

  shots = rbind(shots_away, shots_home)
  rows = as.numeric(rownames(pbp[pbp$actionType != 'freethrow' & pbp$scoring == 1,]))

  #define functions for assist

  get_assists1 = function(i) {
    row = rows[i]
    ast_check = pbp$actionType[row-1]

    if(ast_check=='assist') {
      ast_time = pbp$gt[row-1]
      shot_time = pbp$gt[row]


      assist = data.frame(assist = pbp$player[row-1],
                          shooter = pbp$player[row],
                          period = pbp$period[row-1],
                          time = pbp$gt[row-1])
      shot = shots[shots$player == assist$shooter & shots$quarter == assist$period & shots$time == assist$time,]

      shot$assist = assist$assist
      shot = shot[,c(5, 3, 4, 6, 8)]
      names(shot)[1] = 'shot'
    } else {
      shot = NULL
    }
    return(shot)
  }

  get_assists2 = function(i) {
    row = rows[i]
    ast_check = pbp$actionType[row+1]

    if(ast_check=='assist') {
      ast_time = pbp$gt[row+1]
      shot_time = pbp$gt[row]

      #helper function for time
      time_difference = function(shot_time, ast_time) {
        time_shot = as.numeric(gsub(':.*','', shot_time)) + as.numeric(gsub('*.:','', shot_time))/60
        time_ast = as.numeric(gsub(':.*','', ast_time)) + as.numeric(gsub('*.:','', ast_time))/60
        return(time_shot - time_ast)
      }


      assist = data.frame(assist = pbp$player[row+1],
                          shooter = pbp$player[row],
                          period = pbp$period[row+1],
                          time = pbp$gt[row+1])
      shot = shots[shots$player == assist$shooter & shots$quarter == assist$period,]

      shot$time_diff = time_difference(shot$time, assist$time)
      shot = shot[which(shot$time_diff > 0),]

      suppressWarnings(shot = shot[which(shot$time_diff == min(shot$time_diff)),])


      shot$assist = assist$assist
      shot = shot[,c(5, 3, 4, 6, 9)]
      names(shot)[1] = 'shot'
    } else {
      shot = NULL
    }
    return(shot)
  }

  #use for loop with tryCatch

  ast1 = data.frame()
  for(i in 1:length(rows)) {
    tryCatch({
      ast1 = rbind(ast1, get_assists1(i))
    },
    error = function(e) NULL)
  }


  ast2 = data.frame()
  for(i in 1:length(rows)) {
    tryCatch({
      ast2 = rbind(ast2, get_assists2(i))
    },
    error = function(e) NULL)
  }

  #rbind assists
  ast = rbind(ast1, ast2)
  ast = ast[!duplicated(ast),]

  return(ast)

}
