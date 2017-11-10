#'Get stints from live.fibaeurope.com using game id
#'
#'Get stints from play by play.
#'Stint is a period of time that the same set of ten players are on the court.
#'For every stint this function calculates stats such as total assists, total points, total possessions.
#'
#'@param gameid numeric, game id from live.fibaeurope.com
#'
#'@return data.frame with stints
#'
#'@seealso \code{\link[euRobasket]{get_stints_fibalivestats}} \code{\link[euRobasket]{on_off_splits}}, \code{\link[euRobasket]{get_on_off_splits_players}}
#'
#'@examples
#'#Get stints for id 108510
#'get_stints_livefibaeurope(108510)



get_stints_livefibaeurope = function(gameid) {

pbp = get_raw_pbp_livefibaeurope(gameid)

#get starting 5s for both teams
home_s5 = pbp$home_action[which(grepl('Substitution', pbp$home_action))[1:5]]
home_s5 = gsub('Substitution - ', '', home_s5)
home_s5 = gsub(' IN', '', home_s5)

away_s5 = pbp$away_action[which(grepl('Substitution', pbp$away_action))[1:5]]
away_s5 = gsub('Substitution - ', '', away_s5)
away_s5 = gsub(' IN', '', away_s5)


#extract rows numbers with
subs_rows = which(grepl('Substitution', pbp$home_action) | grepl('Substitution', pbp$away_action))[-c(1:10)]
subs = data.frame(row_in = subs_rows)


#add player in, player out and team
subs$team = pbp$team[subs$row_in]
subs$player_in = ''
subs$player_out = ''

for(i in 1:nrow(subs)) {
  if(subs$team[i] == 1) {

  player_in = gsub('.*<br />', '', pbp$home_action[subs$row_in[i]])
  player_in = gsub(' IN', '', player_in)

  player_out = gsub(' OUT.*', '', pbp$home_action[subs$row_in[i]])
  player_out = gsub('Substitution - ', '', player_out)

  subs$player_in[i] = player_in
  subs$player_out[i] = player_out

  } else if(subs$team[i] == 2) {

    player_in = gsub('.*<br />', '', pbp$away_action[subs$row_in[i]])
    player_in = gsub(' IN', '', player_in)

    player_out = gsub(' OUT.*', '', pbp$away_action[subs$row_in[i]])
    player_out = gsub('Substitution - ', '', player_out)

    subs$player_in[i] = player_in
    subs$player_out[i] = player_out
  }

}

#some substitutions happens on the same time, so it's needed create a unique id for each 'group' of substitutions
subs$sub_group = NA
subs$sub_group[1] = 1

for(i in 2:(nrow(subs))) {
  row_diff = subs$row_in[i] - subs$row_in[i-1]
  if(row_diff == 1) {
    subs$sub_group[i] = subs$sub_group[i-1]
  } else {
    subs$sub_group[i] = subs$sub_group[i-1]+1
  }
}

#create data.frame with players on court
players_on_court = data.frame(cbind(t(home_s5), t(away_s5)), stringsAsFactors = FALSE)

names(players_on_court) = c('home_P1',
                            'home_P2',
                            'home_P3',
                            'home_P4',
                            'home_P5',
                            'away_P1',
                            'away_P2',
                            'away_P3',
                            'away_P4',
                            'away_P5')

#function for stints data
get_stint_data = function(start_row, end_row) {

  #subset stint with start_row and end_row
  stint = pbp[start_row:end_row,]

  #subset home actions and away actions
  home_actions = stint[,6:7]
  away_actions = stint[,8:9]

  #calculate actions types for home team

  #assists
  home_assists = nrow(home_actions[which(grepl('assist', home_actions$home_action)),]) +
    nrow(home_actions[which(grepl('assist', home_actions$home_sub_action)),])

  #scoring
  #2pt
  home_2pt = home_actions$home_action[which(grepl('2pt', home_actions$home_action))]
  home_2pt_fga = length(home_2pt)
  home_2pt_fgm = length(home_2pt[which(grepl('made', home_2pt))])
  `home_2pt_fg%` = round((home_2pt_fgm/home_2pt_fga),2)

  #3pt
  home_3pt = home_actions$home_action[which(grepl('3pt', home_actions$home_action))]
  home_3pt_fga = length(home_3pt)
  home_3pt_fgm = length(home_3pt[which(grepl('made', home_3pt))])
  `home_3pt_fg%` = round((home_3pt_fgm/home_3pt_fga),2)

  #free throws
  home_fts = home_actions$home_action[which(grepl('free throw', home_actions$home_action))]
  home_fta = length(home_fts)
  home_ftm = length(home_fts[which(grepl('made', home_fts))])
  `home_ft%` = round((home_ftm/home_fta),2)


  #turnovers
  home_tovs = nrow(home_actions[which(grepl('turnover', home_actions$home_action)),])

  #rebounds
  home_rebs = home_actions$home_sub_action[which(grepl('rebound', home_actions$home_sub_action))]
  home_orebs = length(home_rebs[which(grepl('offensive', home_rebs))])
  home_drebs = length(home_rebs[which(grepl('defensive', home_rebs))])

  #steals
  home_steals = length(home_actions$home_sub_action[which(grepl('steal', home_actions$home_sub_action))])

  #blocks
  home_blocks = length(home_actions$home_action[which(grepl('block', home_actions$home_action))])

  #pts scored home
  home_pts = (home_2pt_fgm*2) + (home_3pt_fgm*3) + home_ftm

  #create data.frame with actions
  home = data.frame(cbind(
    home_pts,
    home_2pt_fgm,
    home_2pt_fga,
    `home_2pt_fg%`,
    home_3pt_fgm,
    home_3pt_fga,
    `home_3pt_fg%`,
    home_ftm,
    home_fta,
    `home_ft%`,
    home_drebs,
    home_orebs,
    home_assists,
    home_tovs,
    home_steals,
    home_blocks))

  #calculate home possesions
  home$home_possesions = 0.96*((home$home_2pt_fga+home$home_3pt_fga) + home$home_tovs + (0.44*home$home_fta) - home$home_orebs)
  home$home_possesions[home$home_possesions < 0] = 0


  #calculate actions types for away team

  #assists
  away_assists = nrow(away_actions[which(grepl('assist', away_actions$away_action)),]) +
    nrow(away_actions[which(grepl('assist', away_actions$away_sub_action)),])

  #scoring
  #2pt
  away_2pt = away_actions$away_action[which(grepl('2pt', away_actions$away_action))]
  away_2pt_fga = length(away_2pt)
  away_2pt_fgm = length(away_2pt[which(grepl('made', away_2pt))])
  `away_2pt_fg%` = round((away_2pt_fgm/away_2pt_fga),2)

  #3pt
  away_3pt = away_actions$away_action[which(grepl('3pt', away_actions$away_action))]
  away_3pt_fga = length(away_3pt)
  away_3pt_fgm = length(away_3pt[which(grepl('made', away_3pt))])
  `away_3pt_fg%` = round((away_3pt_fgm/away_3pt_fga),2)

  #free throws
  away_fts = away_actions$away_action[which(grepl('free throw', away_actions$away_action))]
  away_fta = length(away_fts)
  away_ftm = length(away_fts[which(grepl('made', away_fts))])
  `away_ft%` = round((away_ftm/away_fta),2)


  #turnovers
  away_tovs = nrow(away_actions[which(grepl('turnover', away_actions$away_action)),])

  #rebounds
  away_rebs = away_actions$away_sub_action[which(grepl('rebound', away_actions$away_sub_action))]
  away_orebs = length(away_rebs[which(grepl('offensive', away_rebs))])
  away_drebs = length(away_rebs[which(grepl('defensive', away_rebs))])

  #steals
  away_steals = length(away_actions$away_sub_action[which(grepl('steal', away_actions$away_sub_action))])

  #blocks
  away_blocks = length(away_actions$away_action[which(grepl('block', away_actions$away_action))])

  #pts scored home
  away_pts = (away_2pt_fgm*2) + (away_3pt_fgm*3) + away_ftm

  #create data.frame with actions
  away = data.frame(cbind(
    away_pts,
    away_2pt_fgm,
    away_2pt_fga,
    `away_2pt_fg%`,
    away_3pt_fgm,
    away_3pt_fga,
    `away_3pt_fg%`,
    away_ftm,
    away_fta,
    `away_ft%`,
    away_drebs,
    away_orebs,
    away_assists,
    away_tovs,
    away_steals,
    away_blocks))

  #calculate away possesions
  away$away_possesions = 0.96*((away$away_2pt_fga+away$away_3pt_fga) + away$away_tovs + (0.44*away$away_fta) - away$away_orebs)
  away$away_possesions[away$away_possesions < 0] = 0

  #combine home and away
  stint_data = cbind(home, away)

  #NaNs to 0
  stint_data[stint_data == 'NaN'] = 0

  return(stint_data)

}

#create stints data.frame
#first row
first_row_start = 1
first_row_end = subs$row_in[1]-1
stints.df = cbind(players_on_court, get_stint_data(first_row_start, first_row_end))


#loop for all other substitutions
for(i in 1:max(subs$sub_group)) {
  #get players on court before substitution
  players_on_court = stints.df[i, 1:10]

  #make substitutions
  subs_to_make = subs[subs$sub_group == i,]

  for(j in 1:nrow(subs_to_make)) {

    sub = subs_to_make[j,]
    if(sub$team == 1) {
      players_on_court[1:5][grep(sub$player_out, players_on_court[1:5])] = sub$player_in
    } else if(sub$team == 2) {
      players_on_court[6:10][grep(sub$player_out, players_on_court[6:10])] = sub$player_in
    }
  }

  #cbind players on court after substitutions with actions

  if(i != max(subs$sub_group)) {

    stints.df[i+1,] = cbind(players_on_court,
                            get_stint_data(start_row = min(subs_to_make$row_in),
                                           end_row = (min(subs$row_in[subs$sub_group == i+1])-1)))

  } else if(i == max(subs$sub_group)) {

    stints.df[i+1,] = cbind(players_on_court,
                            get_stint_data(start_row = min(subs_to_make$row_in),
                                           end_row = nrow(pbp)))

  }

}


#add teams names
stints.df$home_team = unique(pbp$home_team)
stints.df$away_team = unique(pbp$away_team)
stints.df = stints.df[,c(1:10, 45, 46, 11:44)]

return(stints.df)
}





