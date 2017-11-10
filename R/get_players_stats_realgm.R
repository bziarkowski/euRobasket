#'Load players stats from realgm.com
#'
#'Load different types of players stats for choosen league and season
#'from realgm.com. You need to specify league name, season and stats type.
#'
#'@param league character, league name from realgm.com, you can find all
#'available names using get_realgm_league_names()
#'
#'@param season numeric, second year of season, ie. for season 2016-17 use 2017,
#'for season 2013-14 use 2014
#'
#'@param type character, type of stats to downlod, choose from:
#' Totals, Averages, Per_48, Per_40, Per_36, Per_Minute,
#' Minute_Per, Misc_Stats, Advanced_Stats
#'
#'@param position character, 'All' set by default, change to specific position ie.
#'if you want to download stats only for point guards use type = 'PG', available positions:
#'All, PG, SG, SF, PF, C
#'
#'@return data frame with players stats
#'
#'@examples
#'
#'#Load Advanced Stats for PGs from Spanish ACB, season 2014-15
#'get_players_stats_realgm('Spanish ACB', 2015, type = 'Advanced_Stats', position = 'PG')
#'
#'#Load Totals for all players from Polish TBL, season 2012-13
#'get_players_stats_realgm('Polish TBL', 2013, type = 'Totals')


get_players_stats_realgm = function(league, season, type, position = 'All') {

  #get real_gm id for chosen league

  leagues = euRobasket::leagues_ids

  #check if selected name correct, otherwise stop

  if(league %in% leagues$league_name == FALSE) {
    stop('Incorrect league name, use get_realgm_league_names() to find available leagues')
  }

  league_id = leagues$id[leagues$league_name == league]

  #check if selected type correct, otherwise stop

  available_types = c('Totals',
                      'Averages',
                      'Per_48',
                      'Per_40',
                      'Per_36',
                      'Per_Minute',
                      'Minute_Per',
                      'Misc_Stats',
                      'Advanced_Stats')

  if(type %in% available_types == FALSE) {
    stop('Incorrect type of stats, available types:
         Totals,
         Averages,
         Per_48,
         Per_40,
         Per_36,
         Per_Minute,
         Minute_Per,
         Misc_Stats,
         Advanced_Stats')
  }

  #check if selected position correct, otherwise stop

  available_positions = c('All',
                          'PG',
                          'SG',
                          'SF',
                          'PF',
                          'C')

  if(position %in% available_positions == FALSE) {
    stop('Incorrect position, available positions:
         All,
         PG,
         SG,
         SF,
         PF,
         C')
  }


  #download data from realgm with for loop

  dat = data.frame()

  for(i in 1:5) {

    #create url

    url = paste('https://basketball.realgm.com/international/league/',league_id,'/Polish-TBL/stats/',season,'/',type,'/All/All/points/',position,'/asc/',i,'', sep = '')
    df = readHTMLTable(htmlParse(readLines(url)))

    if(length(df)>0) {
      df = df[[1]]
    } else {
      df = NULL
    }

    if(length(df)>0) {
      dat = rbind(dat, df)
    }
  }

  #remove duplicates from dat

  dat = dat[!duplicated(dat),]

  #convert factors to numeric

  col_names = names(dat)
  col_names = col_names[!(col_names %in% c('Player', 'Team'))]

  for(i in 1:length(col_names)) {
  dat[,col_names[i]] = as.numeric(levels(dat[,col_names[i]]))[dat[,col_names[i]]]
  }

  return(dat)

  }
