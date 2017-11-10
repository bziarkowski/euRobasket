#'Download play by play from fibalivestats.com using gameid
#'
#'@param gameid numeric, game id from fibalivestats.com
#'
#'@return data.frame with play by play data
#'
#'@seealso \code{\link[euRobasket]{get_stints_fibalivestats}} \code{\link[euRobasket]{get_raw_pbp_livefibaeurope}}
#'
#'
#'@examples
#'#Download play by play for game 412182
#'get_raw_pbp_fibalivestats(412182)
#'

get_raw_pbp_fibalivestats = function(gameid) {

  #define url for play by play

  url = paste('http://www.fibalivestats.com/data/',gameid,'/data.json')
  dat = fromJSON(url)

  #extract play by play from json

  pbp = dat$pbp
  pbp = pbp[,1:15]

  #change period id of OT

  pbp$period[pbp$periodType == 'OVERTIME'] = 5

  #add teams names

  pbp$team_name = ''
  pbp$team_short_name = ''

  pbp$team_name[pbp$tno == 1] = dat$tm$`1`$name
  pbp$team_name[pbp$tno == 2] = dat$tm$`2`$name

  pbp$team_short_name[pbp$tno == 1] = dat$tm$`1`$shortName
  pbp$team_short_name[pbp$tno == 2] = dat$tm$`2`$shortName

  #remove numbers from players names
  pbp$player = gsub('[0-9]', '', pbp$player)
  pbp$player = gsub(', ','', pbp$player)

  #arrange play by play

  pbp = arrange(pbp, period, desc(gt))

  pbp$firstName = NULL
  pbp$shirtNumber = NULL

  return(pbp)
}
