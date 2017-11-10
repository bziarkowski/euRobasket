#'Get available players for on_off_splits() from stints data frame
#'
#'Use this function if you want to find players for on_off_splits.
#'
#'@param stints.df data.frame, stints downloaded with get_stints_fibalivestats() or get_stints_livefibaeurope()
#'
#'@param print logical, TRUE by default, set to FALSE if you do not want to print results.
#'For on_off_splits() puropses mostly.
#'
#'@return data.frame with all players and teams available in stints.df
#'
#'@seealso \code{\link[euRobasket]{get_on_off_splits_players}}, \code{\link[euRobasket]{get_stints_fibalivestats}}, \code{\link[euRobasket]{get_stints_livefibaeurope}}
#'
#'@examples
#'#get stints from Basketball Champions League using data()
#'#it will be stored in cl_stints
#'data("cl_stints")
#'
#'#get all different players and teams from this data
#'get_on_off_splits_players(cl_stints)


get_on_off_splits_players = function(stints.df, print = TRUE) {

#store all players with teams in matrux
players = rbind(cbind(stints.df$home_P1, stints.df$home_team),
                cbind(stints.df$home_P2, stints.df$home_team),
                cbind(stints.df$home_P3, stints.df$home_team),
                cbind(stints.df$home_P4, stints.df$home_team),
                cbind(stints.df$home_P5, stints.df$home_team),
                cbind(stints.df$away_P1, stints.df$away_team),
                cbind(stints.df$away_P2, stints.df$away_team),
                cbind(stints.df$away_P3, stints.df$away_team),
                cbind(stints.df$away_P4, stints.df$away_team),
                cbind(stints.df$away_P5, stints.df$away_team))

#remove duplicated rows to get unique players and teams
players = data.frame(players[!duplicated(players),], stringsAsFactors = FALSE)
names(players) = c('Player', 'Team')
players = arrange(players, Team)

if(print == TRUE) {
print(players)
} else {
return(players)
}
}

