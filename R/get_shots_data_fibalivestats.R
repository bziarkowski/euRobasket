#'Load shots data from fibalivestats.com with gameid
#'
#'@param gameid game ID from fibalivestats.com, numeric
#'
#'@return data frame with shots
#'
#'@examples
#'#Load shots from game 9863
#'get_shots_data_fibalivestats(9863)
#'
#'@seealso \code{\link[euRobasket]{get_shots_data_livefibaeurope}}
#'


get_shots_data_fibalivestats = function(gameid) {


#Load from fibalivestats.com
url = paste('http://www.fibalivestats.com/data/',gameid,'/data.json', sep = '')
dat = fromJSON(url)

#extract shots

shots_home = dat$tm$`1`$shot
shots_home$team = 'home'

shots_away = dat$tm$`2`$shot
shots_away$team = 'away'

x_y = rbind(shots_home,
            shots_away)

#remove numbers from players names
x_y$player = gsub('[0-9]', '', x_y$player)
x_y$player = gsub(', ','', x_y$player)



#convert shot coordinates

shots1 = x_y[x_y$x<=50,]
shots2 = x_y[x_y$x>50,]

shots2$x = 50-(shots2$x-50)
shots1$y = 50-(shots1$y-50)

shots = rbind(shots1, shots2)

shots$new_x = shots$y
shots$new_y = shots$x

shots = shots[, c('r', 'per', 'new_x', 'new_y', 'player', 'team')]


shots$new_x = shots$new_x*15.5
shots$new_y = shots$new_y*26.5

#change column names

colnames(shots) = c('outcome','quarter', 'x', 'y', 'player','team')

#convert column `outcome` to factor

shots$outcome = factor(shots$outcome,
                       levels = c(1,0),
                       labels = c('made', 'missed'))

#add team names
shots$team[shots$team=='home'] = dat$tm$`1`$shortName
shots$team[shots$team=='away'] = dat$tm$`2`$shortName

return(shots)

}

