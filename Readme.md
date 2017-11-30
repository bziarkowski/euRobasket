# euRobasket

euRobasket is an R package which helps download, prepare and analyze data from
European basketball leagues. In current version data sources include: realgm.com,
fibalivestats.com, live.fibaeurope.com.


# Installation

```R
devtools::install_github('bziarkowski/euRobasket')
library(euRobasket)
```

# Play by play functions
With euRobasket you can download play by play data from fibalivestats.com and live.fibaeurope.com and prepare it for further analysis.

This package makes play by play data easier to analyze by converting it to stints (stint is a period of time that the same set of ten players are on the court). But you can also download raw play by play.

```R
#download raw play by play
get_raw_pbp_fibalivestats(742430)
get_raw_pbp_livefibaeurope(108510)

#get stints from play by play
get_stints_fibalivestats(742430)
get_stints_livefibaeurope(108510)
```

With stints you can calculate lineup stats and on/off splits.
```R
#calculate lineups stats from single game with lineup_stats()
get_stints_fibalivestats(742430) %>% 
lineups_stats()

#calculate on/off splits for Ł. Koszarek and J. Florence from single game with on_off_splits()
get_stints_fibalivestats(742430) %>% 
on_off_splits(players = c('Ł. Koszarek', 'J. Florence'))

```

# Getting the ids for fibalivestats.com and live.fibaeurope.com

Ids for fibalivestats.com and live.fibaeurope.com are available with this package.
You can get them by using `data()`

```R
#fibalivestats.com
data("fibalivestats_matches_ids")

#live.fibaeurope.com
data("livefibaeurope_matches_ids")
```

# Download stats tables from realgm.com

Stats tables with teams or players stats. You can download different types of
stats such as: Totals, Advanced Stats, Averages, Per48, Per40, Per36, Miscellaneous.

```R
#get available realgm.com leagues
get_realgm_league_names()

#download teams stats
get_team_stats_realgm(league = 'Spanish ACB', season = 2017, type = 'Totals')
get_team_stats_realgm(league = 'Polish TBL', season = 2014, type = 'Advanced_Stats', split = 'Team_Starters')

#download players stats
get_players_stats_realgm(league = 'Israeli BSL', season = 2014, type = 'Advanced_Stats')
get_players_stats_realgm(league = 'German BBL', season = 2016, type = 'Totals', position = 'PG')
```

# Download scores and box scores from realgm.com
Scores and box scores from european league for given period of time.
Date format is YYYY-MM-DD.

```R
#download scores
get_scores_realgm('2017-10-01', '2017-11-01', league = 'Polish TBL')
get_scores_realgm('2015-09-01', '2016-02-01', league = 'Danish Basketligaen')

#download box scores
get_boxscores_realgm(start_date = '2016-10-01', end_date = '2016-10-02', league = 'Turkish TBL')

```
# Download and visualize shots
euRobasket lets you download and visualize shot data for various leagues from fibalivestats.com and live.fibaeurope.com. 

Download data from single games with game id.
```r
#from fibalivestats.com
get_shots_data_fibalivestats(742430)

#from live.fibaeurope.com
get_shots_data_fibalivestats(108510)

```
Visualize shots

```R
#one chart from single game with shot_chart()
get_shots_data_fibalivestats(742430) %>%
dplyr::filter(player == 'F. Matczak') %>%
shot_chart(type = 'Scatter')
```
You can also create dashboard with shots.
```R
#download shots from multiple games with for loop
shots = data.frame()
for(i in c(742430, 766471)) {
shots = rbind(shots, get_shots_data_fibalivestats(i))  
}

#create dashboard
shots_dashboard(shots)
```

# Assists
Download assists and create assists networks for teams

```R
#download assists
get_assists_fibalivestats(742430) %>%
assist_network()
```





