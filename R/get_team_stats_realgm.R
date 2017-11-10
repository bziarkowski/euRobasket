#'Load team stats from realgm.com
#'
#'Load different types of team stats for chosen league and season
#'from realgm.com. You need to specify league name, season and stats type.
#'
#'@param league character, league name from realgm.com, you can find all
#'available names using get_realgm_league_names()
#'
#'@param season numeric, second year of season, ie. for season 2016-17 use 2017,
#'for season 2013-14 use 2014
#'
#'@param type character, type of stats to download, choose from:
#' Totals, Averages, Per_48, Per_40, Per_36, Per_Minute,
#' Minute_Per, Misc_Stats, Advanced_Stats
#'
#'@param split character, 'Team_Totals' set by default, choose from:
#'Team_Totals, Team_Starters, Team_Bench,
#'Opponent_Totals, Opponent_Starters, Opponent_Bench
#'
#'
#'@return data frame with team stats
#'
#'@examples
#'
#'#Load Advanced Stats for Spanish ACB Teams, season 2014-15
#'get_team_stats_realgm('Spanish ACB', 2015, type = 'Advanced_Stats')
#'
#'#Load Totals for British BBL teams, season 2015-16,
#'#use 'Opponent Starters as a split
#'get_team_stats_realgm('British BBL', 2016, type = 'Totals', split = 'Opponent_Starters')





get_team_stats_realgm = function(league, season, type, split = 'Team_Totals') {

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

#check if selected split correct, otherwise stop

available_splits = c('Team_Totals',
                     'Team_Starters',
                     'Team_Bench',
                     'Opponent_Totals',
                     'Opponent_Starters',
                     'Opponent_Bench')

if(split %in% available_splits == FALSE) {
  stop('Incorrect split, available splits:
       Team_Totals,
       Team_Starters,
       Team_Bench,
       Opponent_Totals,
       Opponent_Starters,
       Opponent_Bench')
}

#Download team stats from realgm.com

url = paste('https://basketball.realgm.com/international/league/',league_id,'/Polish-TBL/team-stats/',season,'/',type,'/',split,'', sep = '')

dat = readHTMLTable(htmlParse(readLines(url)))[[1]]

#convert factors to numeric

col_names = names(dat)
col_names = col_names[!(col_names %in% c('Team'))]

for(i in 1:length(col_names)) {
  dat[,col_names[i]] = as.numeric(levels(dat[,col_names[i]]))[dat[,col_names[i]]]
}

return(dat)
}
