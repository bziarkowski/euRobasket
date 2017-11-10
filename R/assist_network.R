#'Visualize assist networks for teams
#'
#'This function lets you visualize assisst network for one team.
#'You need to use assists data downloaded with get_assists_fibalivestats() or get_assists_livefibaeurope().
#'
#'@param assist.df data.frame, assists downloaded with get_assists_fibalivestats() or get_assists_livefibaeurope().
#'This function works only with data for one team. If you add assists for more than
#'one team this function will choose the team with most assists.
#'
#'@param title character, assist network title, 'Assist Network' set by default
#'
#'@param max.players numeric, maximum number of players used to visualize assists network
#'
#'@seealso \code{\link[euRobasket]{get_assists_fibalivestats}}, \code{\link[euRobasket]{get_assists_livefibaeurope}}
#'
#'@examples
#'
#'#get example assists data from argentinian league season 2015/16 using data()
#'#it will be stored in arg_assists
#'data("arg_assists")
#'
#'#subset assists for Peñarol
#'assists = arg_assists[arg_assists$team == 'PEÑAROL',]
#'
#'#plot assist network, change title
#'assist_network(assist.df = assists, title = 'Peñarol Assist Network')
#'
#'

assist_network = function(assist.df, title = 'Assist Network', max.players = 10) {

#check if max.players is bigger than 1

if(!max.players > 1) {
  stop('Argument max.players needs to be bigger than 1!')
}

#check if only assist.df contains only assists for one team

if(length(unique(assist.df$team)) > 1) {

  #get number of assists for teams

  n_assists = assist.df %>%
    group_by(team) %>%
    summarise(n_assists = n()) %>%
    arrange(desc(n_assists))

  #select team with the most assists

  selected_team = n_assists$team[1]
  assist.df = assist.df[assist.df$team == selected_team,]


  warning(paste('Data frame assist.df contains assists for more than one team.',
                '\n',
                'Choosing team with the most assists:', selected_team,
                '\n',
                'For proper working of this function assist.df needs to contain assists data for only one team.',
                '\n'))
}

#check if number of assists is bigger than 20, if not generate warning message
if(!nrow(assist.df) > 20) {
  warning('Small number of assists in assist.df.
          \n For proper working of this function assist.df should contain more than 20 assists')
}



#create nodes
links_dat = assist.df[,c('assist', 'shot')]

#convert to characters
links_dat[,1] = as.character(links_dat[,1])
links_dat[,2] = as.character(links_dat[,2])

nodes = paste(links_dat[,1], links_dat[,2])

players = unique(c(links_dat[,1], links_dat[,2]))

actions = data.frame()
for(i in 1:length(players)) {
  player = players[i]
  player_actions = length(grep(player, nodes))
  actions[i,1] = player
  actions[i,2] = player_actions
}

names(actions) = c('id', 'actions')

#remove outliers

actions = actions[actions$actions > quantile(actions$actions, 0.05),]

#select number of players defined in max.players

actions = arrange(actions, desc(actions))[1:max.players,]
actions = actions[!is.na(actions$id),]


players_to_select = actions$id


#create links

links = links_dat %>%
  group_by(assist, shot) %>%
  summarise(passes = n())
names(links) = c('from', 'to', 'weight')

links = links[links$from %in% players_to_select & links$to %in% players_to_select,]

#save player names

set.seed(10)
names_long_short =  suppressWarnings(data.frame(long = actions$id,
                        short =  paste(gsub(' .*', '',actions$id), as.character(abbreviate(gsub('.* ', '',actions$id), minlength = 2)))))

#abbreviate names
#multiple gsub function from stackoverflow.com/questions/15253954/replace-multiple-arguments-with-gsub/15254254
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}

links$from = mgsub(names_long_short$long, names_long_short$short, links$from)
links$to = mgsub(names_long_short$long, names_long_short$short, links$to)
actions$id = mgsub(names_long_short$long, names_long_short$short, actions$id)

#create net

net = graph_from_data_frame(d = links, vertices = actions)

#edit net properties

#vertices size
V_size = V(net)$actions
V_size = ((V_size - min(V_size)) / (max(V_size) - min(V_size)))
V_size[V_size == 0] = 0.001

V(net)$size = V_size*20

#vertices color
V(net)$label.color <- "#2b2e4a"
V(net)$color = '#e84545'
V(net)$frame.color = '#e84545'

#vertices label cex
V(net)$label.cex = 0.7

#edges width
E_width = E(net)$weight
E_width = ((E_width - min(E_width)) / (max(E_width) - min(E_width)))
E_width[E_width == 0] = 0.001

E(net)$width <- E_width*10

#edges arrow size
E(net)$arrow.size <- .1

#edges color
E(net)$color <- "#53354a"

#edges curved
E(net)$curved = 0.2

#define layout
l = layout_with_fr(net)

#plot network
par(mar = c(0,0,0,0), family = 'Arial', font = 2, oma = c(3,1,1,1))
plot(net, layout = l)
title(title, line = -.75, col.main = '#53354a')
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

#define ncols for legend
legend_ncol = round(nrow(names_long_short)/2)+1

#add legend
suppressWarnings(legend("bottom", sort(paste(names_long_short$short, '=' ,names_long_short$long)), cex = 0.7,
       bty = 'n', text.col = '#53354a', ncol = legend_ncol, text.width=c(0.25,0.25)))

}
