#'Download scores from realgm.com for given period of time.
#'You need to specify start date, end date and league name
#'
#'@param start_date Date in format YYYY-MM-DD
#'@param end_date Date in format YYYY-MM-DD
#'
#'
#'@param league character, league name from realgm.com, you can find all
#'available names using get_realgm_league_names()
#'
#'@return data frame with scores
#'
#'@examples
#'
#'#Load scores from Israeli BSL, first week of march 2017
#'get_scores_realgm('2017-03-01', '2017-03-07', 'Israeli BSL')
#'

get_scores_realgm = function(start_date, end_date, league) {

  #convert start_date and end_date to Date object and create date sequence
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  date_seq = seq.Date(start_date, end_date, 1)

  #get id of the choosen league from euRobasket::league_ids
  leagues = euRobasket::leagues_ids

  #check if selected league name correct, otherwise stop

  if(league %in% leagues$league_name == FALSE) {
    stop('Incorrect league name, use get_realgm_league_names() to find available leagues')
  }

  league_id = leagues$id[leagues$league_name == league]

  #download scores from given days
  scores = lapply(1:length(date_seq), function(x) {

  date = date_seq[x]
  url = paste('https://basketball.realgm.com/international/league/',league_id,'/Israeli-BSL/scores/',date,'/All',
              sep = '')
  dat = htmlParse(readLines(url))

  games = xpathSApply(dat, '//td[@class = "game_details"]', xmlValue, trim = TRUE)

  if(length(games)>0) {
  #define number of games
  n_games = length(games)/4

  games_df = data.frame()

  for(i in 1:n_games) {

    start = 1
    end = 4

    if(i > 1) {
      start = start + (4*(i-1))
      end = end + (4*(i-1))
    }

  game = games[start:end]

  home_team = gsub('\n.*', '', game[2])
  away_team = gsub('\n.*', '', game[1])

  home_score = as.numeric(game[4])
  away_score = as.numeric(game[3])

  game_df = data.frame(home_team, away_team, home_score, away_score)

  games_df = rbind(games_df, game_df)

  }

  #add date to games

  games_df$date = date

  } else {
    games_df = NULL
  }

  return(games_df)
})
  #convert scores to data frame
  scores = do.call(rbind.data.frame, scores)

  #add league name

  scores$league = league

  return(scores)
}
