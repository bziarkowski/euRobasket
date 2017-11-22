#'Get on/off splits for selected players from stints.
#'
#'On/off splits shows how team and opponents played with and without selected players.
#'For this function selected players needs to be from the same team. You can select
#'up to 5 players. To get all available players in stints use get_on_off_splits_players()
#'
#'@param stints.df data.frame, stints download with get_stints_fibalivestats() or get_stints_livefibaeurope()
#'
#'@param players a character vector with player or players for on/off splits,
#'Max number of players is 5.
#'
#'@return list with data.frames, data frames contains different stats types:
#'Ratings, Team Shooting, Opponent Shooting, Team Playmaking, Opponent Playmaking,
#'Team Boxscore Defensive, Opponent Boxscore Defensive
#'
#'@seealso  \code{\link[euRobasket]{get_on_off_splits_players}}, \code{\link[euRobasket]{get_stints_fibalivestats}}, \code{\link[euRobasket]{get_stints_livefibaeurope}}
#'
#'@examples
#'#get on/off splits from Basketball Champions League
#'#firstly get the stints from bcl using data(), stored in cl_stints
#'data("cl_stints")
#'
#'#check available players
#'get_on_off_splits_players(cl_stints)
#'
#'#get on/off splits for M. PONITKA
#'on_off_splits(cl_stints, players = 'M. PONITKA')
#'
#'#get on/off splits for M. PONITKA, D. SUMMERS and J. BROWN
#'on_off_splits(cl_stints, players = c('M. PONITKA', 'D. SUMMERS', 'J. BROWN'))
#'


on_off_splits = function(stints.df, players) {

#check if selected players appear in stints.df
stint_players = get_on_off_splits_players(stints.df, print = FALSE)
#change encoding to  utf-8
Encoding(stint_players$Player) = 'UTF-8'
Encoding(players) = 'UTF-8'

player_appear = players %in% stint_players$Player

if(length(player_appear[player_appear == FALSE])) {
  players_not = players[which(player_appear == FALSE)]
  if(length(players_not)==1){

  stop(paste('Player: ', "'", players_not[1],"'" , ' does not appear in stints.df.
             Select different players.
             Get all available players for stints.df using get_on_off_splits_players()', sep = ''))
  } else if(length(players_not)>1) {

    players_paste = c()
    for(i in 1:length(players_not)) {
     if(i != length(players_not)) {
      players_paste = paste(players_paste, players_not[i], ', ', sep = '')
     } else if(i == length(players_not)) {
       players_paste = paste(players_paste, players_not[i], sep = '')
    }
    }

    stop(paste('Players: ', "'" , players_paste, "'", ' does not appear in stints.df.
             Select different players.
             Get all available players for stints.df using get_on_off_splits_players()', sep = ''))
  }
}

#check if selected players are from the same team, otherwise stop
unique_teams = unique(stint_players$Team[stint_players$Player %in% players])

if(length(unique_teams)>1) {
  stop('Selected players are not from the same team.
        Select players only from one team.
        Get all available players for stints.df using get_on_off_splits_players()')
}


#home
home_on_court = as.data.frame(t(apply(stints.df[,1:5], 1, '%in%', x = players)))

if(length(players)==1) {
 home_rows_on_court = which(t(home_on_court) == TRUE)
} else if(length(players)==2) {
 home_rows_on_court = which(home_on_court$V1 == TRUE & home_on_court$V2 == TRUE)
} else if(length(players)==3) {
 home_rows_on_court = which(home_on_court$V1 == TRUE & home_on_court$V2 == TRUE & home_on_court$V3 == TRUE)
} else if(length(players)==4) {
 home_rows_on_court = which(home_on_court$V1 == TRUE & home_on_court$V2 == TRUE & home_on_court$V3 == TRUE & home_on_court$V4 == TRUE)
} else if(length(players)==5) {
 home_rows_on_court = which(home_on_court$V1 == TRUE & home_on_court$V2 == TRUE & home_on_court$V3 == TRUE & home_on_court$V4 == TRUE & home_on_court$V5 == TRUE)
} else if(length(players) > 5) {
  stop('Too much players selected!
       Max number is 5.')
}

home_team_name = stints.df$home_team[home_rows_on_court[1]]
home_rows_all = which(stints.df$home_team == home_team_name)
home_rows_off_court = home_rows_all[!(home_rows_all %in% home_rows_on_court)]

#pts, possessions home
#ortg
home_ortg_data_on = data.frame(poss = sum(stints.df$home_possesions[home_rows_on_court]),
                           pts = sum(stints.df$home_pts[home_rows_on_court]))
home_drtg_data_on = data.frame(poss = sum(stints.df$away_possesions[home_rows_on_court]),
                               pts = sum(stints.df$away_pts[home_rows_on_court]))
#drtg
home_ortg_data_off = data.frame(poss = sum(stints.df$home_possesions[home_rows_off_court]),
                              pts = sum(stints.df$home_pts[home_rows_off_court]))
home_drtg_data_off = data.frame(poss = sum(stints.df$away_possesions[home_rows_off_court]),
                                pts = sum(stints.df$away_pts[home_rows_off_court]))

#away
away_on_court = as.data.frame(t(apply(stints.df[,6:10], 1, '%in%', x = players)))

if(length(players)==1) {
  away_rows_on_court = which(t(away_on_court) == TRUE)
} else if(length(players)==2) {
  away_rows_on_court = which(away_on_court$V1 == TRUE & away_on_court$V2 == TRUE)
} else if(length(players)==3) {
  away_rows_on_court = which(away_on_court$V1 == TRUE & away_on_court$V2 == TRUE & away_on_court$V3 == TRUE)
} else if(length(players)==4) {
  away_rows_on_court = which(away_on_court$V1 == TRUE & away_on_court$V2 == TRUE & away_on_court$V3 == TRUE & away_on_court$V4 == TRUE)
} else if(length(players)==5) {
  away_rows_on_court = which(away_on_court$V1 == TRUE & away_on_court$V2 == TRUE & away_on_court$V3 == TRUE & away_on_court$V4 == TRUE & away_on_court$V5 == TRUE)
}

#stop if selected players does not appear in stint.df

if(length(home_rows_on_court)==0 & length(away_rows_on_court)==0) {
  stop('Selected players does not appear on the court in the same time.
        Select different players.')
}

away_team_name = stints.df$away_team[away_rows_on_court[1]]
away_rows_all = which(stints.df$away_team == away_team_name)
away_rows_off_court = away_rows_all[!(away_rows_all %in% away_rows_on_court)]

#pts, possessions away
#ortg
away_ortg_data_on = data.frame(poss = sum(stints.df$away_possesions[away_rows_on_court]),
                              pts = sum(stints.df$away_pts[away_rows_on_court]))
away_drtg_data_on = data.frame(poss = sum(stints.df$home_possesions[away_rows_on_court]),
                               pts = sum(stints.df$home_pts[away_rows_on_court]))
#drtg
away_ortg_data_off = data.frame(poss = sum(stints.df$away_possesions[away_rows_off_court]),
                               pts = sum(stints.df$away_pts[away_rows_off_court]))
away_drtg_data_off = data.frame(poss = sum(stints.df$home_possesions[away_rows_off_court]),
                                pts = sum(stints.df$home_pts[away_rows_off_court]))


#drtg, ortg, nrtg
drtg_data_on = colSums(rbind(away_drtg_data_on, home_drtg_data_on))
drtg_data_off = colSums(rbind(away_drtg_data_off, home_drtg_data_off))

ortg_data_on = colSums(rbind(away_ortg_data_on, home_ortg_data_on))
ortg_data_off = colSums(rbind(away_ortg_data_off, home_ortg_data_off))

#on
on = data.frame(ORtg = 100*(ortg_data_on[2]/ortg_data_on[1]),
                DRtg = 100*(drtg_data_on[2]/drtg_data_on[1]),
                NetRtg = (100*(ortg_data_on[2]/ortg_data_on[1])) - (100*(drtg_data_on[2]/drtg_data_on[1])))

#off
off = data.frame(ORtg = 100*(ortg_data_off[2]/ortg_data_off[1]),
                DRtg = 100*(drtg_data_off[2]/drtg_data_off[1]),
                NetRtg = (100*(ortg_data_off[2]/ortg_data_off[1])) - (100*(drtg_data_off[2]/drtg_data_off[1])))

ratings = rbind(on, off, (on-off))

#add rownames for ratings
  selected_players = ''
  for(i in 1:length(players)) {
    if(i != length(players)) {
    paste_players = paste(players[i],', ', sep = '')
    } else {
      paste_players = players[i]
    }
    selected_players = paste(selected_players, paste_players, sep = '')
  }


rownames(ratings) = c(paste(selected_players, 'ON'),
                      paste(selected_players, 'OFF'),
                      'Difference')

#team shooting
#on
fga_on = sum(stints.df$home_2pt_fga[home_rows_on_court]) +
         sum(stints.df$away_2pt_fga[away_rows_on_court]) +
         sum(stints.df$home_3pt_fga[home_rows_on_court]) +
         sum(stints.df$away_3pt_fga[away_rows_on_court])

fgm_on = sum(stints.df$home_2pt_fgm[home_rows_on_court]) +
         sum(stints.df$away_2pt_fgm[away_rows_on_court]) +
         sum(stints.df$home_3pt_fgm[home_rows_on_court]) +
         sum(stints.df$away_3pt_fgm[away_rows_on_court])

`3pa_on` = sum(stints.df$home_3pt_fga[home_rows_on_court]) +
           sum(stints.df$away_3pt_fga[away_rows_on_court])

`3pm_on` = sum(stints.df$home_3pt_fgm[home_rows_on_court]) +
           sum(stints.df$away_3pt_fgm[away_rows_on_court])

fta_on = sum(stints.df$home_fta[home_rows_on_court]) +
         sum(stints.df$away_fta[away_rows_on_court])

ftm_on = sum(stints.df$home_ftm[home_rows_on_court]) +
        sum(stints.df$away_ftm[away_rows_on_court])

`fg%_on` = round(100*(fgm_on/fga_on),2)
`3p%_on` = round(100*(`3pm_on`/`3pa_on`),2)
`ft%_on` = round(100*(ftm_on/fta_on),2)

efg = round(100*((fgm_on + 0.5*`3pm_on`)/fga_on),2)

pts = sum(stints.df$home_pts[home_rows_on_court]) +
      sum(stints.df$away_pts[away_rows_on_court])

`ts%` = round(100*(pts/(2*(fga_on + 0.44*fta_on))),2)

`%fgm_ast` = round(100*((sum(stints.df$home_assists[home_rows_on_court]) +
                         sum(stints.df$away_assists[away_rows_on_court]))/fgm_on),2)

team_shooting_on = data.frame(pts,
                              fga_on,
                             `fg%_on`,
                             `3pa_on`,
                             `3p%_on`,
                              fta_on,
                             `ft%_on`,
                              efg,
                             `ts%`,
                             `%fgm_ast`)

names(team_shooting_on) = c('PTS',
                            'FGA',
                            'FG%',
                            '3PA',
                            '3P%',
                            'FTA',
                            'FT%',
                            'eFG%',
                            'TS%',
                            '%FGM Assisted')


#off
fga_off = sum(stints.df$home_2pt_fga[home_rows_off_court]) +
          sum(stints.df$away_2pt_fga[away_rows_off_court]) +
          sum(stints.df$home_3pt_fga[home_rows_off_court]) +
          sum(stints.df$away_3pt_fga[away_rows_off_court])

fgm_off = sum(stints.df$home_2pt_fgm[home_rows_off_court]) +
          sum(stints.df$away_2pt_fgm[away_rows_off_court]) +
          sum(stints.df$home_3pt_fgm[home_rows_off_court]) +
          sum(stints.df$away_3pt_fgm[away_rows_off_court])

`3pa_off` = sum(stints.df$home_3pt_fga[home_rows_off_court]) +
            sum(stints.df$away_3pt_fga[away_rows_off_court])

`3pm_off` = sum(stints.df$home_3pt_fgm[home_rows_off_court]) +
            sum(stints.df$away_3pt_fgm[away_rows_off_court])

fta_off = sum(stints.df$home_fta[home_rows_off_court]) +
          sum(stints.df$away_fta[away_rows_off_court])

ftm_off = sum(stints.df$home_ftm[home_rows_off_court]) +
          sum(stints.df$away_ftm[away_rows_off_court])

`fg%_off` = round(100*(fgm_off/fga_off),2)
`3p%_off` = round(100*(`3pm_off`/`3pa_off`),2)
`ft%_off` = round(100*(ftm_off/fta_off),2)

efg = round(100*((fgm_off + 0.5*`3pm_off`)/fga_off),2)

pts = sum(stints.df$home_pts[home_rows_off_court]) +
      sum(stints.df$away_pts[away_rows_off_court])

`ts%` = round(100*(pts/(2*(fga_off + 0.44*fta_off))),2)

`%fgm_ast` = round(100*((sum(stints.df$home_assists[home_rows_off_court]) +
                         sum(stints.df$away_assists[away_rows_off_court]))/fgm_off),2)

team_shooting_off = data.frame(pts,
                              fga_off,
                              `fg%_off`,
                              `3pa_off`,
                              `3p%_off`,
                              fta_off,
                              `ft%_off`,
                              efg,
                              `ts%`,
                              `%fgm_ast`)

names(team_shooting_off) = c('PTS',
                            'FGA',
                            'FG%',
                            '3PA',
                            '3P%',
                            'FTA',
                            'FT%',
                            'eFG%',
                            'TS%',
                            '%FGM Assisted')

#combine splits, add rownames
team_shooting = rbind(team_shooting_on,
                         team_shooting_off)
#add points per shot (including free throws)
team_shooting$PPS = round(team_shooting$PTS/(team_shooting$FGA+team_shooting$FTA),2)

rownames(team_shooting) = c(paste(selected_players, 'ON'),
                            paste(selected_players, 'OFF'))



#opponent shooting
#on
opp_fga_on = sum(stints.df$away_2pt_fga[home_rows_on_court]) +
             sum(stints.df$home_2pt_fga[away_rows_on_court]) +
             sum(stints.df$away_3pt_fga[home_rows_on_court]) +
             sum(stints.df$home_3pt_fga[away_rows_on_court])

opp_fgm_on = sum(stints.df$away_2pt_fgm[home_rows_on_court]) +
             sum(stints.df$home_2pt_fgm[away_rows_on_court]) +
             sum(stints.df$away_3pt_fgm[home_rows_on_court]) +
             sum(stints.df$home_3pt_fgm[away_rows_on_court])

`opp_3pa_on` = sum(stints.df$away_3pt_fga[home_rows_on_court]) +
               sum(stints.df$home_3pt_fga[away_rows_on_court])

`opp_3pm_on` = sum(stints.df$away_3pt_fgm[home_rows_on_court]) +
               sum(stints.df$home_3pt_fgm[away_rows_on_court])

opp_fta_on = sum(stints.df$away_fta[home_rows_on_court]) +
             sum(stints.df$home_fta[away_rows_on_court])

opp_ftm_on = sum(stints.df$away_ftm[home_rows_on_court]) +
             sum(stints.df$home_ftm[away_rows_on_court])

`opp_fg%_on` = round(100*(opp_fgm_on/opp_fga_on),2)
`opp_3p%_on` = round(100*(`opp_3pm_on`/`opp_3pa_on`),2)
`opp_ft%_on` = round(100*(opp_ftm_on/opp_fta_on),2)

opp_efg = round(100*((opp_fgm_on + 0.5*`opp_3pm_on`)/opp_fga_on),2)

opp_pts = sum(stints.df$away_pts[home_rows_on_court]) +
          sum(stints.df$home_pts[away_rows_on_court])

`opp_ts%` = round(100*(opp_pts/(2*(opp_fga_on + 0.44*opp_fta_on))),2)

`opp_%fgm_ast` = round(100*((sum(stints.df$away_assists[home_rows_on_court]) +
                         sum(stints.df$home_assists[away_rows_on_court]))/opp_fgm_on),2)

opp_shooting_on = data.frame(opp_pts,
                              opp_fga_on,
                              `opp_fg%_on`,
                              `opp_3pa_on`,
                              `opp_3p%_on`,
                              opp_fta_on,
                              `opp_ft%_on`,
                              opp_efg,
                              `opp_ts%`,
                              `opp_%fgm_ast`)

names(opp_shooting_on) = c('PTS',
                            'FGA',
                            'FG%',
                            '3PA',
                            '3P%',
                            'FTA',
                            'FT%',
                            'eFG%',
                            'TS%',
                            '%FGM Assisted')

#off
opp_fga_off = sum(stints.df$away_2pt_fga[home_rows_off_court]) +
              sum(stints.df$home_2pt_fga[away_rows_off_court]) +
              sum(stints.df$away_3pt_fga[home_rows_off_court]) +
              sum(stints.df$home_3pt_fga[away_rows_off_court])

opp_fgm_off = sum(stints.df$away_2pt_fgm[home_rows_off_court]) +
             sum(stints.df$home_2pt_fgm[away_rows_off_court]) +
             sum(stints.df$away_3pt_fgm[home_rows_off_court]) +
             sum(stints.df$home_3pt_fgm[away_rows_off_court])

`opp_3pa_off` = sum(stints.df$away_3pt_fga[home_rows_off_court]) +
                sum(stints.df$home_3pt_fga[away_rows_off_court])

`opp_3pm_off` = sum(stints.df$away_3pt_fgm[home_rows_off_court]) +
                sum(stints.df$home_3pt_fgm[away_rows_off_court])

opp_fta_off = sum(stints.df$away_fta[home_rows_off_court]) +
              sum(stints.df$home_fta[away_rows_off_court])

opp_ftm_off = sum(stints.df$away_ftm[home_rows_off_court]) +
              sum(stints.df$home_ftm[away_rows_off_court])

`opp_fg%_off` = round(100*(opp_fgm_off/opp_fga_off),2)
`opp_3p%_off` = round(100*(`opp_3pm_off`/`opp_3pa_off`),2)
`opp_ft%_off` = round(100*(opp_ftm_off/opp_fta_off),2)

opp_efg = round(100*((opp_fgm_off + 0.5*`opp_3pm_off`)/opp_fga_off),2)

opp_pts = sum(stints.df$away_pts[home_rows_off_court]) +
          sum(stints.df$home_pts[away_rows_off_court])

`opp_ts%` = round(100*(opp_pts/(2*(opp_fga_off + 0.44*opp_fta_off))),2)

`opp_%fgm_ast` = round(100*((sum(stints.df$away_assists[home_rows_off_court]) +
                             sum(stints.df$home_assists[away_rows_off_court]))/opp_fgm_off),2)

opp_shooting_off = data.frame(opp_pts,
                             opp_fga_off,
                             `opp_fg%_off`,
                             `opp_3pa_off`,
                             `opp_3p%_off`,
                             opp_fta_off,
                             `opp_ft%_off`,
                             opp_efg,
                             `opp_ts%`,
                             `opp_%fgm_ast`)

names(opp_shooting_off) = c('PTS',
                           'FGA',
                           'FG%',
                           '3PA',
                           '3P%',
                           'FTA',
                           'FT%',
                           'eFG%',
                           'TS%',
                           '%FGM Assisted')

#combine splits, add rownames
opp_shooting = rbind(opp_shooting_on,
                      opp_shooting_off)
#add points per shot (including free throws)
opp_shooting$PPS = round(opp_shooting$PTS/(opp_shooting$FGA+opp_shooting$FTA),2)
rownames(opp_shooting) = c(paste(selected_players, 'ON'),
                            paste(selected_players, 'OFF'))

#team playmaking
#on
tovs_on = sum(stints.df$home_tovs[home_rows_on_court]) +
          sum(stints.df$away_tovs[away_rows_on_court])

poss_on = sum(stints.df$home_possesions[home_rows_on_court]) +
          sum(stints.df$away_possesions[away_rows_on_court])

ast_on = sum(stints.df$home_assists[home_rows_on_court]) +
         sum(stints.df$away_assists[away_rows_on_court])

tov_rate_on = round(100*(tovs_on/poss_on),2)
ast_rate_on = round(100*(ast_on/poss_on),2)

team_pm_on = data.frame(poss_on,
                        tovs_on,
                        tov_rate_on,
                        ast_on,
                        ast_rate_on)

names(team_pm_on) = c('Possessions',
                      'TOV',
                      'TOV Rate',
                      'AST',
                      'AST Rate')

#off
tovs_off = sum(stints.df$home_tovs[home_rows_off_court]) +
           sum(stints.df$away_tovs[away_rows_off_court])

poss_off = sum(stints.df$home_possesions[home_rows_off_court]) +
           sum(stints.df$away_possesions[away_rows_off_court])

ast_off = sum(stints.df$home_assists[home_rows_off_court]) +
          sum(stints.df$away_assists[away_rows_off_court])

tov_rate_off = round(100*(tovs_off/poss_off),2)
ast_rate_off = round(100*(ast_off/poss_off),2)

team_pm_off = data.frame(poss_off,
                        tovs_off,
                        tov_rate_off,
                        ast_off,
                        ast_rate_off)

names(team_pm_off) = c('Possessions',
                      'TOV',
                      'TOV Rate',
                      'AST',
                      'AST Rate')

#combine playmaking splits and add rownames
team_playmaking = rbind(team_pm_on,
                        team_pm_off)

rownames(team_playmaking) = c(paste(selected_players, 'ON'),
                              paste(selected_players, 'OFF'))



#opponent playmaking
#on
opp_tovs_on = sum(stints.df$away_tovs[home_rows_on_court]) +
              sum(stints.df$home_tovs[away_rows_on_court])

opp_poss_on = sum(stints.df$away_possesions[home_rows_on_court]) +
              sum(stints.df$home_possesions[away_rows_on_court])

opp_ast_on = sum(stints.df$away_assists[home_rows_on_court]) +
             sum(stints.df$home_assists[away_rows_on_court])

opp_tov_rate_on = round(100*(opp_tovs_on/opp_poss_on),2)
opp_ast_rate_on = round(100*(opp_ast_on/opp_poss_on),2)

opp_pm_on = data.frame(opp_poss_on,
                       opp_tovs_on,
                       opp_tov_rate_on,
                       opp_ast_on,
                       opp_ast_rate_on)

names(opp_pm_on) = c('Possessions',
                     'TOV',
                     'TOV Rate',
                     'AST',
                     'AST Rate')

#off
opp_tovs_off = sum(stints.df$away_tovs[home_rows_off_court]) +
               sum(stints.df$home_tovs[away_rows_off_court])

opp_poss_off = sum(stints.df$away_possesions[home_rows_off_court]) +
           sum(stints.df$home_possesions[away_rows_off_court])

opp_ast_off = sum(stints.df$away_assists[home_rows_off_court]) +
          sum(stints.df$home_assists[away_rows_off_court])

opp_tov_rate_off = round(100*(opp_tovs_off/opp_poss_off),2)
opp_ast_rate_off = round(100*(opp_ast_off/opp_poss_off),2)

opp_pm_off = data.frame(opp_poss_off,
                        opp_tovs_off,
                        opp_tov_rate_off,
                        opp_ast_off,
                        opp_ast_rate_off)

names(opp_pm_off) = c('Possessions',
                      'TOV',
                      'TOV Rate',
                      'AST',
                      'AST Rate')

#combine playmaking splits and add rownames
opp_playmaking = rbind(opp_pm_on,
                        opp_pm_off)

rownames(opp_playmaking) = c(paste(selected_players, 'ON'),
                              paste(selected_players, 'OFF'))

#team rebs, blks, stls
#on
drebs_on = sum(stints.df$home_drebs[home_rows_on_court]) +
           sum(stints.df$away_drebs[away_rows_on_court])

orebs_on = sum(stints.df$home_orebs[home_rows_on_court]) +
           sum(stints.df$away_orebs[away_rows_on_court])

trebs_on = drebs_on + orebs_on

`drebs%_on` = round(100*(drebs_on/(sum(stints.df$away_orebs[home_rows_on_court]) +
                                   sum(stints.df$home_orebs[away_rows_on_court])+drebs_on)),2)

`orebs%_on` = round(100*(orebs_on/(sum(stints.df$away_drebs[home_rows_on_court]) +
                                   sum(stints.df$home_drebs[away_rows_on_court])+orebs_on)),2)

`trebs%_on` = round(100*(trebs_on/((sum(stints.df$away_orebs[home_rows_on_court]) +
                                    sum(stints.df$home_orebs[away_rows_on_court])+drebs_on) +
                                   (sum(stints.df$away_drebs[home_rows_on_court]) +
                                    sum(stints.df$home_drebs[away_rows_on_court])+orebs_on))),2)

stls_on = sum(stints.df$home_steals[home_rows_on_court]) +
          sum(stints.df$away_steals[away_rows_on_court])

blks_on = sum(stints.df$home_blocks[home_rows_on_court]) +
          sum(stints.df$away_blocks[away_rows_on_court])

stl_rate_on = round(100*(stls_on/poss_on),2)

blk_rate_on = round(100*(blks_on/poss_on),2)

team_box_defensive_on = data.frame(poss_on,
                                   drebs_on,
                                   `drebs%_on`,
                                   orebs_on,
                                   `orebs%_on`,
                                   stls_on,
                                   stl_rate_on,
                                   blks_on,
                                   blk_rate_on)

names(team_box_defensive_on) = c('Possessions',
                                 'DRB',
                                 'DRB%',
                                 'ORB',
                                 'ORB%',
                                 'STL',
                                 'STL Rate',
                                 'BLK',
                                 'BLK Rate')

#off
drebs_off = sum(stints.df$home_drebs[home_rows_off_court]) +
            sum(stints.df$away_drebs[away_rows_off_court])

orebs_off = sum(stints.df$home_orebs[home_rows_off_court]) +
            sum(stints.df$away_orebs[away_rows_off_court])

trebs_off = drebs_off + orebs_off

`drebs%_off` = round(100*(drebs_off/(sum(stints.df$away_orebs[home_rows_off_court]) +
                                    sum(stints.df$home_orebs[away_rows_off_court])+drebs_off)),2)

`orebs%_off` = round(100*(orebs_off/(sum(stints.df$away_drebs[home_rows_off_court]) +
                                     sum(stints.df$home_drebs[away_rows_off_court])+orebs_off)),2)

`trebs%_off` = round(100*(trebs_off/((sum(stints.df$away_orebs[home_rows_off_court]) +
                                      sum(stints.df$home_orebs[away_rows_off_court])+drebs_off) +
                                     (sum(stints.df$away_drebs[home_rows_off_court]) +
                                      sum(stints.df$home_drebs[away_rows_off_court])+orebs_off))),2)

stls_off = sum(stints.df$home_steals[home_rows_off_court]) +
           sum(stints.df$away_steals[away_rows_off_court])

blks_off = sum(stints.df$home_blocks[home_rows_off_court]) +
          sum(stints.df$away_blocks[away_rows_off_court])

stl_rate_off = round(100*(stls_off/poss_off),2)

blk_rate_off = round(100*(blks_off/poss_off),2)

team_box_defensive_off = data.frame(poss_off,
                                    drebs_off,
                                   `drebs%_off`,
                                   orebs_off,
                                   `orebs%_off`,
                                   stls_off,
                                   stl_rate_off,
                                   blks_off,
                                   blk_rate_off)

names(team_box_defensive_off) = c('Possessions',
                                 'DRB',
                                 'DRB%',
                                 'ORB',
                                 'ORB%',
                                 'STL',
                                 'STL Rate',
                                 'BLK',
                                 'BLK Rate')

#combine boxscore defense splits and add rownames
team_box_def = rbind(team_box_defensive_on,
                     team_box_defensive_off)

rownames(team_box_def) = c(paste(selected_players, 'ON'),
                              paste(selected_players, 'OFF'))



#opponent rebs, blks, stls
#on
opp_drebs_on = sum(stints.df$away_drebs[home_rows_on_court]) +
               sum(stints.df$home_drebs[away_rows_on_court])

opp_orebs_on = sum(stints.df$away_orebs[home_rows_on_court]) +
               sum(stints.df$home_orebs[away_rows_on_court])

opp_trebs_on = opp_drebs_on + opp_orebs_on

`opp_drebs%_on` = round(100*(opp_drebs_on/(sum(stints.df$home_orebs[home_rows_on_court]) +
                                           sum(stints.df$away_orebs[away_rows_on_court])+opp_drebs_on)),2)

`opp_orebs%_on` = round(100*(opp_orebs_on/(sum(stints.df$home_drebs[home_rows_on_court]) +
                                           sum(stints.df$away_drebs[away_rows_on_court])+opp_orebs_on)),2)

`opp_trebs%_on` = round(100*(opp_trebs_on/((sum(stints.df$home_orebs[home_rows_on_court]) +
                                            sum(stints.df$away_orebs[away_rows_on_court])+opp_drebs_on) +
                                           (sum(stints.df$home_drebs[home_rows_on_court]) +
                                            sum(stints.df$away_drebs[away_rows_on_court])+opp_orebs_on))),2)

opp_stls_on = sum(stints.df$away_steals[home_rows_on_court]) +
              sum(stints.df$home_steals[away_rows_on_court])

opp_blks_on = sum(stints.df$away_blocks[home_rows_on_court]) +
              sum(stints.df$home_blocks[away_rows_on_court])

opp_stl_rate_on = round(100*(opp_stls_on/opp_poss_on),2)

opp_blk_rate_on = round(100*(opp_blks_on/opp_poss_on),2)

opp_box_defensive_on = data.frame(opp_poss_on,
                                   opp_drebs_on,
                                   `opp_drebs%_on`,
                                   opp_orebs_on,
                                   `opp_orebs%_on`,
                                   opp_stls_on,
                                   opp_stl_rate_on,
                                   opp_blks_on,
                                   opp_blk_rate_on)

names(opp_box_defensive_on) = c('Possessions',
                                 'DRB',
                                 'DRB%',
                                 'ORB',
                                 'ORB%',
                                 'STL',
                                 'STL Rate',
                                 'BLK',
                                 'BLK Rate')


#off
opp_drebs_off = sum(stints.df$away_drebs[home_rows_off_court]) +
                sum(stints.df$home_drebs[away_rows_off_court])

opp_orebs_off = sum(stints.df$away_orebs[home_rows_off_court]) +
                sum(stints.df$home_orebs[away_rows_off_court])

opp_trebs_off = opp_drebs_off + opp_orebs_off

`opp_drebs%_off` = round(100*(opp_drebs_off/(sum(stints.df$home_orebs[home_rows_off_court]) +
                                             sum(stints.df$away_orebs[away_rows_off_court])+opp_drebs_off)),2)

`opp_orebs%_off` = round(100*(opp_orebs_off/(sum(stints.df$home_drebs[home_rows_off_court]) +
                                             sum(stints.df$away_drebs[away_rows_off_court])+opp_orebs_off)),2)

`opp_trebs%_off` = round(100*(opp_trebs_off/((sum(stints.df$home_orebs[home_rows_off_court]) +
                                              sum(stints.df$away_orebs[away_rows_off_court])+opp_drebs_off) +
                                             (sum(stints.df$home_drebs[home_rows_off_court]) +
                                              sum(stints.df$away_drebs[away_rows_off_court])+opp_orebs_off))),2)

opp_stls_off = sum(stints.df$away_steals[home_rows_off_court]) +
               sum(stints.df$home_steals[away_rows_off_court])

opp_blks_off = sum(stints.df$away_blocks[home_rows_off_court]) +
               sum(stints.df$home_blocks[away_rows_off_court])

opp_stl_rate_off = round(100*(opp_stls_off/opp_poss_off),2)

opp_blk_rate_off = round(100*(opp_blks_off/opp_poss_off),2)

opp_box_defensive_off = data.frame(opp_poss_off,
                                  opp_drebs_off,
                                  `opp_drebs%_off`,
                                  opp_orebs_off,
                                  `opp_orebs%_off`,
                                  opp_stls_off,
                                  opp_stl_rate_off,
                                  opp_blks_off,
                                  opp_blk_rate_off)

names(opp_box_defensive_off) = c('Possessions',
                                'DRB',
                                'DRB%',
                                'ORB',
                                'ORB%',
                                'STL',
                                'STL Rate',
                                'BLK',
                                'BLK Rate')


#combine boxscore defense splits and add rownames
opp_box_def = rbind(opp_box_defensive_on,
                    opp_box_defensive_off)

rownames(opp_box_def) = c(paste(selected_players, 'ON'),
                           paste(selected_players, 'OFF'))

#create list with all splits
on_off_list = list(Ratings = ratings,
                  `Team Shooting` = team_shooting,
                  `Opponent Shooting` = opp_shooting,
                  `Team Playmaking` = team_playmaking,
                  `Opponent Playmaking` = opp_playmaking,
                  `Team Boxscore Defensive` = team_box_def,
                  `Opponent Boxscore Defensive` = opp_box_def)

print(on_off_list)
}
