#' Download boxscores from realgm.com for given period of time
#'
#' @param start_date Date in format YYYY-MM-DD
#' @param end_date Date in format YYYY-MM-DD
#' @param league name of league, all names can be found using function
#' get_realgm_league_names()
#'
#' @return list of box scores
#'
#' @examples
#'
#' #Load box scores from Spanish ACB
#' get_boxscores_realgm('2017-10-01', '2017-10-08', 'Spanish ACB')
#'
#' #Load box scores from Austrian League
#' #Find exact name of the league using get_realgm_league_names()
#' get_realgm_league_names()
#' get_boxscores_realgm('2016-01-12', '2016-09-11', 'Austrian A Bundesliga')

get_boxscores_realgm = function(start_date, end_date, league) {

#convert start_date and end_date to Date object and create date sequence
start_date = as.Date(start_date)
end_date = as.Date(end_date)
date_seq = seq.Date(start_date, end_date, 1)

#get id of the choosen league from euRobasket::league_ids
ids = euRobasket::leagues_ids
league_id = ids$id[ids$league_name == league]

#extract boxscores hrefs from given days

hrefs = lapply(1:length(date_seq), function(x) {
date = date_seq[x]
url = paste('https://basketball.realgm.com/international/schedules/',date,'/', league_id, sep = '')
dat = htmlParse(readLines(url))
dat = xpathSApply(dat, '//@href')
links = dat[grepl('boxscore', dat)]
if(length(links)>0) {
links = paste('https://realgm.com', links, sep = '')
}
return(links)
})

hrefs = do.call(c, hrefs)

#download boxcores from given days

boxscores = lapply(1:length(hrefs), function(x) {
       html = htmlParse(readLines(hrefs[x]))
       html_tab = readHTMLTable(html)
       box_home = html_tab[[5]]
       box_away = html_tab[[4]]
       box_home$team_name_long = xpathSApply(html, '//h2', xmlValue, trim = TRUE)[[3]]
       box_away$team_name_long = xpathSApply(html, '//h2', xmlValue, trim = TRUE)[[2]]
       box_home$team_name_short = html_tab[[2]][2,1]
       box_away$team_name_short = html_tab[[2]][1,1]
       boxscore = rbind(box_home, box_away)

       #separate fgs, 3fgs and fts using tidyr::separate and dplyr
       boxscore = boxscore %>%
         separate(`FGM-A`, c('FGM', 'FGA'), "-") %>%
         separate(`3PM-A`, c('3PM', '3PA'), "-") %>%
         separate(`FTM-A`, c('FTM', 'FTA'), "-")

      #add date

      match_date = gsub('.*boxscore/','', hrefs[x])
      match_date = gsub('/.*','', match_date)

      boxscore$match_date = match_date

      #convert minutes to numeric

      minutes = separate(boxscore,Min, c('m', 's'), ':')[,5:6]
      minutes = round(((as.numeric(minutes$m)) + (as.numeric(minutes$s)/60)),2)
      boxscore$Min = minutes

      #order columns and rename them

      boxscore = boxscore[,c(2,22, 23, 24, 3:11, 13:21)]

      colnames(boxscore) = c('player',
                             'team_name_long',
                             'team_name_short',
                             'match_date',
                             'status',
                             'position',
                             'minutes',
                             'fgm',
                             'fga',
                             '3pm',
                             '3pa',
                             'ftm',
                             'fta',
                             'off_rebounds',
                             'def_rebounds',
                             'tot_rebounds',
                             'assists',
                             'fouls',
                             'steals',
                             'turnovers',
                             'blocks',
                             'points')

      #add unique id for boxscore
      boxscore$boxscore_id = gsub('.*/','', hrefs[x])

      return(boxscore)
})


return(boxscores)
}
