#'Create dashboard to visualize shots
#'
#'This function creates dashboard from shots data loaded with get_shots_data_livefibaeurope()
#'or get_shots_data_fibalivestats(). It containts two type of shot charts: scatter and heatmap.
#'It only runs locally by default, but you can use save = TRUE to write app to directory and then
#'post it on shinyapps.io.
#'
#'@param shots_df Shots data in data.frame format. It can be downloaded using get_shots_data_livefibaeurope()
#'or get_shots_data_fibalivestats()
#'
#'@param dashboardTitle character, title of dashboard, 'Shots' set by default
#'
#'@param save logical, FALSE set by default, use save = TRUE if you want to save app in
#'new folder in working directory. It can be then posted on shinyapps.io
#'
#'@param app_name character, 'shots_dashboard' set by default,
#'name of the directory where the application will be saved
#'
#'
#'@examples
#'#create shots dashboard for Basketball Champions League shots example
#'#get data using data(), it will be stored in cl_shots
#'data("cl_shots")
#'
#'#create dashboard, run locally
#'shots_dashboard(shots_df = cl_shots, dashboardTitle = 'BCL Shots')
#'
#'#create dashboard and save it in working directory
#'shots_dashboard(shots_df = cl_shots, dashboardTitle = 'BCL Shots', save = TRUE, app_name = 'BCL_Dashboard')


shots_dashboard = function(shots_df, dashboardTitle = 'Shots', save = FALSE, app_name = 'shots_dashboard') {

#function for shiny dashboard header

  mydashboardHeader <- function(..., title = dashboardTitle, disable = FALSE,title.navbar=NULL, .list = NULL) {
    items <- c(list(...), .list)

    tags$header(class = "main-header",
                style = if (disable) "display: none;",
                span(class = "logo", title),
                tags$nav(class = "navbar navbar-static-top", role = "navigation",

                         span(shiny::icon("bars"), style = "display:none;"),

                         title.navbar,
                         div(class = "navbar-custom-menu",
                             tags$ul(class = "nav navbar-nav",
                                     items
                             ))))
  }


#load package logo

logo = readPNG(system.file('data', 'white_logo.png', package = 'euRobasket'))

#save logo directory
logo_dir = system.file('data', 'white_logo.png', package = 'euRobasket')

#load basketball court for scatter

scatter = readPNG(system.file('data', 'scatter.png', package = 'euRobasket'))
scatter = rasterGrob(scatter, width = unit(1.0, 'npc'), height = unit(1.0, 'npc'))

#load basketball court for heatmap

heatmap = readPNG(system.file('data', 'heatmap.png', package = 'euRobasket'))
heatmap = rasterGrob(heatmap, width = unit(1.0, 'npc'), height = unit(1.0, 'npc'))


dat = shots_df
players = unique(paste(dat$player,' ','(',dat$team, ')', sep = ''))


#app UI

ui_run = dashboardPage(skin = 'yellow',
  mydashboardHeader(
                  tags$li(class = "dropdown",
                          tags$a(href="https://github.com/bziarkowski/euRobasket", target="_blank",
                                 tags$b('euRobasket')
                          ))),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Shot charts', tabName = 'shotcharts', icon = icon('area-chart')))),
  dashboardBody(
    tags$style(HTML("
                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#2c3e50
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#2c3e50;
                    border-left-color:#2c3e50;
                    border-right-color:#2c3e50;
                    border-top-color:#2c3e50;
                    }
                    ")),

  fluidRow(
    h1(' ', style = 'margin-top:10px')
  ),

  fluidRow(
           box(title = 'Shot chart', width = 9, solidHeader = TRUE, status = 'primary',
               plotOutput('shot_chart', height = '508px')
               ),

           box(title = tagList(shiny::icon('gear'), 'Options'), status = 'warning', solidHeader = TRUE,
              'Choose a player and type of shot chart', width = 3,
               selectInput('player', 'Player',
                           choices = players, selected = players[1]),
              radioButtons('chart_type', 'Chart type',
                          choices = c('Scatter', 'Heatmap'), selected = 'Scatter')),

           box(title = 'Shooting by zone', status = 'primary', solidHeader = TRUE,
               width = 3,
               tableOutput('shot_tab')
               )
           )
  )
)

#app Server


server_run = function(input, output) {

  #function for preparing shots by zone table

  get_shots_zone_tab = reactive({

    player = input$player
    name = gsub(' [(].*', '', player)

    team = gsub('.*[(]', '', player)
    team = gsub('[)]', '', team)

    a = dat[dat$player == name & dat$team == team,]

    #get shots in the paint
    in_the_paint <- subset(a, a$x >= 560 & a$x <= 950 & a$y >= 90 & a$y <= 400)
    in_the_paint_fg = round(((nrow(in_the_paint[in_the_paint$outcome == 'made',]) / nrow(in_the_paint))*100),1)
    in_the_paint_fg[in_the_paint_fg=='NaN'] = 0

    #subset mid-range shots
    midrange1 <- subset(a, a$x >= 100 & a$x <= 560 & a$y >= 90 & a$y <= 240)
    midrange2 <- subset(a, a$x >= 115 & a$x <= 560 & a$y >= 240 & a$y <= 320)
    midrange3 <- subset(a, a$x >= 130 & a$x <= 560 & a$y >= 320 & a$y <= 390)
    midrange4 <- subset(a, a$x >= 150 & a$x <= 560 & a$y >= 390 & a$y <= 460)
    midrange5 <- subset(a, a$x >= 185 & a$x <= 560 & a$y >= 460 & a$y <= 500)
    midrange6 <- subset(a, a$x >= 215 & a$x <= 560 & a$y >= 500 & a$y <= 570)
    midrange7 <- subset(a, a$x >= 250 & a$x <= 560 & a$y >= 570 & a$y <= 605)
    midrange8 <- subset(a, a$x >= 280 & a$x <= 560 & a$y >= 605 & a$y <= 630)
    midrange9 <- subset(a, a$x >= 310 & a$x <= 560 & a$y >= 630 & a$y <= 655)
    midrange10 <- subset(a, a$x >= 340 & a$x <= 560 & a$y >= 655 & a$y <= 680)
    midrange11 <- subset(a, a$x >= 370 & a$x <= 560 & a$y >= 680 & a$y <= 705)
    midrange12 <- subset(a, a$x >= 400 & a$x <= 560 & a$y >= 705 & a$y <= 730)
    midrange13 <- subset(a, a$x >= 430 & a$x <= 560 & a$y >= 730 & a$y <= 745)
    midrange14 <- subset(a, a$x >= 460 & a$x <= 560 & a$y >= 745 & a$y <= 760)
    midrange15 <- subset(a, a$x >= 490 & a$x <= 560 & a$y >= 760 & a$y <= 775)
    midrange16 <- subset(a, a$x >= 515 & a$x <= 560 & a$y >= 775 & a$y <= 785)
    midrange17 <- subset(a, a$x >= 560 & a$x <= 950 & a$y >= 401 & a$y <= 540)
    midrange18 <- subset(a, a$x >= 560 & a$x <= 950 & a$y >= 540 & a$y <= 780)
    midrange19 <- subset(a, a$x >= 950 & a$x <= 1410 & a$y >= 80 & a$y <= 240)
    midrange20 <- subset(a, a$x >= 950 & a$x <= 1395 & a$y >= 240 & a$y <= 320)
    midrange21 <- subset(a, a$x >= 950 & a$x <= 1380 & a$y >= 320 & a$y <= 390)
    midrange22 <- subset(a, a$x >= 950 & a$x <= 1360 & a$y >= 390 & a$y <= 460)
    midrange23 <- subset(a, a$x >= 950 & a$x <= 1325 & a$y >= 460 & a$y <= 500)
    midrange24 <- subset(a, a$x >= 950 & a$x <= 1295 & a$y >= 500 & a$y <= 570)
    midrange25 <- subset(a, a$x >= 950 & a$x <= 1260 & a$y >= 570 & a$y <= 605)
    midrange26 <- subset(a, a$x >= 950 & a$x <= 1230 & a$y >= 605 & a$y <= 630)
    midrange27 <- subset(a, a$x >= 950 & a$x <= 1200 & a$y >= 630 & a$y <= 655)
    midrange28 <- subset(a, a$x >= 950 & a$x <= 1170 & a$y >= 655 & a$y <= 680)
    midrange29 <- subset(a, a$x >= 950 & a$x <= 1140 & a$y >= 680 & a$y <= 705)
    midrange30 <- subset(a, a$x >= 950 & a$x <= 1110 & a$y >= 705 & a$y <= 730)
    midrange31 <- subset(a, a$x >= 950 & a$x <= 1080 & a$y >= 730 & a$y <= 745)
    midrange32 <- subset(a, a$x >= 950 & a$x <= 1050 & a$y >= 745 & a$y <= 760)
    midrange33 <- subset(a, a$x >= 950 & a$x <= 1020 & a$y >= 760 & a$y <= 775)
    midrange34 <- subset(a, a$x >= 950 & a$x <= 995 & a$y >= 775 & a$y <= 785)
    midrange_df = rbind(midrange1, midrange2, midrange3, midrange4, midrange5,
                        midrange6, midrange7, midrange8, midrange9, midrange10,
                        midrange11, midrange12, midrange13, midrange14, midrange15,
                        midrange16, midrange17, midrange18, midrange19, midrange20,
                        midrange21, midrange22, midrange23, midrange24, midrange25,
                        midrange26, midrange27, midrange28, midrange29, midrange30,
                        midrange31, midrange32, midrange33, midrange34)
    midrange_fg = round(((nrow(midrange_df[midrange_df$outcome == 'made',]) / nrow(midrange_df))*100),1)
    midrange_fg[midrange_fg=='NaN'] = 0

    #subset right corner 3s
    right_corner_3 <- subset(a, a$x >= -60 & a$x <= 90 & a$y >= -10 & a$y <= 300)
    right_corner_3_fg = round(((nrow(right_corner_3[right_corner_3$outcome == 'made',]) / nrow(right_corner_3))*100),1)
    right_corner_3_fg[right_corner_3_fg=='NaN'] = 0

    #subset left corner 3s
    left_corner_3 <- subset(a, a$x >= 1420 & a$x <= 1700 & a$y >= -10 & a$y <= 300)
    left_corner_3_fg = round(((nrow(left_corner_3[left_corner_3$outcome == 'made',]) / nrow(left_corner_3))*100),1)
    left_corner_3_fg[left_corner_3_fg=='NaN'] = 0

    #subset above the break 3s

    above_the_break1 <- subset(a, a$x >= 1420 & a$x <= 1700 & a$y >= 300 & a$y <= 320)
    above_the_break2 <- subset(a, a$x >= 1390 & a$x <= 1700 & a$y >= 320 & a$y <= 390)
    above_the_break3 <- subset(a, a$x >= 1370 & a$x <= 1700 & a$y >= 390 & a$y <= 460)
    above_the_break4 <- subset(a, a$x >= 1335 & a$x <= 1700 & a$y >= 460 & a$y <= 500)
    above_the_break5 <- subset(a, a$x >= 1305 & a$x <= 1700 & a$y >= 500 & a$y <= 570)
    above_the_break6 <- subset(a, a$x >= 1270 & a$x <= 1700 & a$y >= 570 & a$y <= 605)
    above_the_break7 <- subset(a, a$x >= 1240 & a$x <= 1700 & a$y >= 605 & a$y <= 630)
    above_the_break8 <- subset(a, a$x >= 1210 & a$x <= 1700 & a$y >= 630 & a$y <= 655)
    above_the_break9 <- subset(a, a$x >= 1180 & a$x <= 1700 & a$y >= 655 & a$y <= 680)
    above_the_break10 <- subset(a, a$x >= 1150 & a$x <= 1700 & a$y >= 680 & a$y <= 705)
    above_the_break11 <- subset(a, a$x >= 1120 & a$x <= 1700 & a$y >= 705 & a$y <= 730)
    above_the_break12 <- subset(a, a$x >= 1090 & a$x <= 1700 & a$y >= 730 & a$y <= 745)
    above_the_break13 <- subset(a, a$x >= 1060 & a$x <= 1700 & a$y >= 745 & a$y <= 760)
    above_the_break14 <- subset(a, a$x >= 1030 & a$x <= 1700 & a$y >= 760 & a$y <= 775)
    above_the_break15 <- subset(a, a$x >= 1005 & a$x <= 1700 & a$y >= 775 & a$y <= 785)
    above_the_break16 <- subset(a, a$x >= 980 & a$x <= 1700 & a$y >= 785 & a$y <= 795)
    above_the_break17 <- subset(a, a$x >= 955 & a$x <= 1700 & a$y >= 795 & a$y <= 805)
    above_the_break18 <- subset(a, a$x >= 930 & a$x <= 1700 & a$y >= 805 & a$y <= 815)
    above_the_break19 <- subset(a, a$x >= 905 & a$x <= 1700 & a$y >= 815 & a$y <= 825)
    above_the_break20 <- subset(a, a$x >= -10 & a$x <= 90 & a$y >= 300 & a$y <= 320)
    above_the_break21 <- subset(a, a$x >= -10 & a$x <= 105 & a$y >= 320 & a$y <= 390)
    above_the_break22 <- subset(a, a$x >= -10 & a$x <= 135 & a$y >= 390 & a$y <= 460)
    above_the_break23 <- subset(a, a$x >= -10 & a$x <= 170 & a$y >= 460 & a$y <= 500)
    above_the_break24 <- subset(a, a$x >= -10 & a$x <= 200 & a$y >= 500 & a$y <= 570)
    above_the_break25 <- subset(a, a$x >= -10 & a$x <= 235 & a$y >= 570 & a$y <= 605)
    above_the_break26 <- subset(a, a$x >= -10 & a$x <= 265 & a$y >= 605 & a$y <= 630)
    above_the_break27 <- subset(a, a$x >= -10 & a$x <= 295 & a$y >= 630 & a$y <= 655)
    above_the_break28 <- subset(a, a$x >= -10 & a$x <= 325 & a$y >= 655 & a$y <= 680)
    above_the_break29 <- subset(a, a$x >= -10 & a$x <= 355 & a$y >= 680 & a$y <= 705)
    above_the_break30 <- subset(a, a$x >= -10 & a$x <= 385 & a$y >= 705 & a$y <= 730)
    above_the_break31 <- subset(a, a$x >= -10 & a$x <= 415 & a$y >= 730 & a$y <= 745)
    above_the_break32 <- subset(a, a$x >= -10 & a$x <= 445 & a$y >= 745 & a$y <= 760)
    above_the_break33 <- subset(a, a$x >= -10 & a$x <= 475 & a$y >= 760 & a$y <= 775)
    above_the_break34 <- subset(a, a$x >= -10 & a$x <= 500 & a$y >= 775 & a$y <= 785)
    above_the_break35 <- subset(a, a$x >= -10 & a$x <= 525 & a$y >= 785 & a$y <= 795)
    above_the_break36 <- subset(a, a$x >= -10 & a$x <= 550 & a$y >= 795 & a$y <= 805)
    above_the_break37 <- subset(a, a$x >= -10 & a$x <= 575 & a$y >= 805 & a$y <= 815)
    above_the_break38 <- subset(a, a$x >= -10 & a$x <= 600 & a$y >= 815 & a$y <= 825)
    above_the_break39 <- subset(a, a$x >= -10 & a$x <= 1700 & a$y >= 825 & a$y <= 845)
    above_the_break40 <- subset(a, a$x >= -10 & a$x <= 1700 & a$y >= 845 & a$y <= 955)
    above_the_break_df = rbind(above_the_break1, above_the_break2, above_the_break3, above_the_break4, above_the_break5,
                               above_the_break6, above_the_break7, above_the_break8, above_the_break9, above_the_break10,
                               above_the_break11, above_the_break12, above_the_break13, above_the_break14, above_the_break15,
                               above_the_break16, above_the_break17, above_the_break18, above_the_break19, above_the_break20,
                               above_the_break21, above_the_break22, above_the_break23, above_the_break24, above_the_break25,
                               above_the_break26, above_the_break27, above_the_break28, above_the_break29, above_the_break30,
                               above_the_break31, above_the_break32, above_the_break33, above_the_break34, above_the_break35,
                               above_the_break36, above_the_break37, above_the_break38, above_the_break39, above_the_break40)

    above_the_break_fg = round(((nrow(above_the_break_df[above_the_break_df$outcome == 'made',]) / nrow(above_the_break_df))*100),1)
    above_the_break_fg[above_the_break_fg=='NaN'] = 0

    shots_by_zone_df = data.frame(Zone = c('In the Paint',
                                           'Mid-range',
                                           'Above the break 3',
                                           'Left Corner 3',
                                           'Right Corner 3'),
                                  FGP = c(in_the_paint_fg,
                                          midrange_fg,
                                          above_the_break_fg,
                                          left_corner_3_fg,
                                          right_corner_3_fg))

    shots_by_zone_df$FGP = as.integer(shots_by_zone_df$FGP)
    names(shots_by_zone_df)[2] = 'FG%'
    return(shots_by_zone_df)
  })

  #render shots by zone table

  output$shot_tab = renderTable({
    tab = get_shots_zone_tab()
    tab
  }, options = list(dom = 't'))


  #render shot chart

  output$shot_chart = renderPlot({

  player = input$player

  #get selected player name and team name

  name = gsub(' [(].*', '', player)
  team = gsub('.*[(]', '', player)
  team = gsub('[)]', '', team)

  #subset shots

  shots = dat[dat$player == name & dat$team == team,]
  names(shots)[1] = 'Outcome'

  #define number of bins for heatmap

  if(nrow(shots)<=5){
    selected_bins = 6
  } else if(nrow(shots)>5 & nrow(shots)<=30) {
    selected_bins = 8
  } else if(nrow(shots)>30) {
    selected_bins = 10
  }

  #create data.frame for geom_text
  text_df = data.frame(x = 755,
                       y = c(1060, 985),
                       lab = c(name, team))

  #create plots

  #Scatter

  if(input$chart_type == 'Scatter') {

  plot = ggplot() +
    annotation_custom(scatter, -60, 1570, -10, 1150) +
    xlim(-60, 1570) +
    ylim(-10, 1150) +
    geom_point(data = shots, aes(x,y, colour = Outcome), size = 6, alpha = 0.7) +
    geom_text(data = text_df, aes(x,y,label = lab), size = 7, colour = 'white', family = 'AvantGarde',fontface = 'plain') +
    scale_color_manual(values = c('#27ae60', '#e74c3c')) +
    guides(colour = FALSE) +
    annotation_custom(rasterGrob(logo),
                      xmin = 1200, xmax = 1500, ymin = 950, ymax = 1150) +
    theme(line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.spacing = element_blank(),
          legend.position = 'bottom',
          legend.background = element_blank())

  print(plot)

  #Heatmap

  } else if(input$chart_type == 'Heatmap') {

    plot = ggplot(data = shots, aes(x,y)) +
      annotation_custom(heatmap, -60, 1570, -10, 1150) +
      xlim(-60, 1570) +
      ylim(-10, 1150) +
      stat_density2d(aes(alpha =..level.., fill = ..level..), geom="polygon", bins = selected_bins, n = 500) +
      scale_fill_gradient(low = '#FAFAFA', high = '#FFC107') +
      scale_alpha(range = c(0, 0.8)) +
      guides(alpha = FALSE, fill = FALSE) +
      geom_text(data = text_df, aes(x,y,label = lab), size = 7, colour = 'white', family = 'AvantGarde',fontface = 'plain') +
      annotation_custom(rasterGrob(logo),
                        xmin = 1200, xmax = 1500, ymin = 950, ymax = 1150) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            panel.background = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            panel.spacing = element_blank(),
            legend.position = 'bottom',
            legend.background = element_blank())
    print(plot)
  }

  })


}

#run or save app

if(save == FALSE) {

shinyApp(ui_run, server_run)

} else if(save == TRUE) {

  #create directory for app using app_name
  dir.create(app_name)

  #save images
  file.copy(from = system.file('data', 'white_logo.png', package = 'euRobasket'), to = paste(app_name, '/', 'logo.png', sep = ''))
  file.copy(from = system.file('data', 'scatter.png', package = 'euRobasket'), to = paste(app_name, '/', 'field.png', sep = ''))
  file.copy(from = system.file('data', 'heatmap.png', package = 'euRobasket'), to = paste(app_name, '/', 'field_heat.png', sep = ''))

  #save shots
  saveRDS(dat, paste(app_name, '/', 'shots.RDS', sep = ''))

  #create ui and save it


  ui = function() {

    #load required libraries
    library(shiny)
    library(shinydashboard)
    library(png)
    library(grid)
    library(ggplot2)


    #function for shiny dashboard header
    mydashboardHeader <- function(..., title = 'Shots Dashboard', disable = FALSE,title.navbar=NULL, .list = NULL) {
      items <- c(list(...), .list)

      tags$header(class = "main-header",
                  style = if (disable) "display: none;",
                  span(class = "logo", title),
                  tags$nav(class = "navbar navbar-static-top", role = "navigation",

                           span(shiny::icon("bars"), style = "display:none;"),

                           title.navbar,
                           div(class = "navbar-custom-menu",
                               tags$ul(class = "nav navbar-nav",
                                       items
                               ))))
    }

    #load logo
    logo = readPNG('logo.png')

    #load shots

    dat = readRDS('shots.RDS')
    players = unique(paste(dat$player,' ','(',dat$team, ')', sep = ''))

    dashboardPage(skin = 'yellow',
                  mydashboardHeader(
                    tags$li(class = 'dropdown',
                            tags$a(href='https://github.com/bziarkowski/euRobasket', target='_blank',
                                   tags$b('euRobasket')
                            ))),
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem('Shot charts', tabName = 'shotcharts', icon = icon('area-chart')))),
                  dashboardBody(
                    tags$style(HTML('
                                    .box.box-solid.box-primary>.box-header {
                                    color:#fff;
                                    background:#2c3e50
                                    }

                                    .box.box-solid.box-primary{
                                    border-bottom-color:#2c3e50;
                                    border-left-color:#2c3e50;
                                    border-right-color:#2c3e50;
                                    border-top-color:#2c3e50;
                                    }
                                    ')),

                    fluidRow(
                      h1(' ', style = 'margin-top:10px')
                    ),

                    fluidRow(
                      box(title = 'Shot chart', width = 9, solidHeader = TRUE, status = 'primary',
                          plotOutput('shot_chart', height = '508px')
                      ),

                      box(title = tagList(shiny::icon('gear'), 'Options'), status = 'warning', solidHeader = TRUE,
                          'Choose a player and type of shot chart', width = 3,
                          selectInput('player', 'Player',
                                      choices = players, selected = players[1]),
                          radioButtons('chart_type', 'Chart type',
                                       choices = c('Scatter', 'Heatmap'), selected = 'Scatter')),

                      box(title = 'Shooting by zone', status = 'primary', solidHeader = TRUE,
                          width = 3,
                          tableOutput('shot_tab')
                      )
                    )
                    )
                    )
}

  dput(ui, paste(app_name, '/', 'ui.R', sep = ''))

  #create and save server.R

  server = function(input, output) {

    #load required libraries
    library(shiny)
    library(shinydashboard)
    library(png)
    library(grid)
    library(ggplot2)

    #read images

    field = readPNG('field.png')
    field = rasterGrob(field, width = unit(1.0, 'npc'), height = unit(1.0, 'npc'))

    field_heat = readPNG('field_heat.png')
    field_heat = rasterGrob(field_heat, width = unit(1.0, 'npc'), height = unit(1.0, 'npc'))


    logo = readPNG('logo.png')


    #load shots

    dat = readRDS('shots.RDS')
    players = unique(paste(dat$player,' ','(',dat$team, ')', sep = ''))

    #function for preparing shots by zone table

    get_shots_zone_tab = reactive({

      player = input$player
      name = gsub(' [(].*', '', player)

      team = gsub('.*[(]', '', player)
      team = gsub('[)]', '', team)

      a = dat[dat$player == name & dat$team == team,]

      #get shots in the paint
      in_the_paint <- subset(a, a$x >= 560 & a$x <= 950 & a$y >= 90 & a$y <= 400)
      in_the_paint_fg = round(((nrow(in_the_paint[in_the_paint$outcome == 'made',]) / nrow(in_the_paint))*100),1)
      in_the_paint_fg[in_the_paint_fg=='NaN'] = 0

      #subset mid-range shots
      midrange1 <- subset(a, a$x >= 100 & a$x <= 560 & a$y >= 90 & a$y <= 240)
      midrange2 <- subset(a, a$x >= 115 & a$x <= 560 & a$y >= 240 & a$y <= 320)
      midrange3 <- subset(a, a$x >= 130 & a$x <= 560 & a$y >= 320 & a$y <= 390)
      midrange4 <- subset(a, a$x >= 150 & a$x <= 560 & a$y >= 390 & a$y <= 460)
      midrange5 <- subset(a, a$x >= 185 & a$x <= 560 & a$y >= 460 & a$y <= 500)
      midrange6 <- subset(a, a$x >= 215 & a$x <= 560 & a$y >= 500 & a$y <= 570)
      midrange7 <- subset(a, a$x >= 250 & a$x <= 560 & a$y >= 570 & a$y <= 605)
      midrange8 <- subset(a, a$x >= 280 & a$x <= 560 & a$y >= 605 & a$y <= 630)
      midrange9 <- subset(a, a$x >= 310 & a$x <= 560 & a$y >= 630 & a$y <= 655)
      midrange10 <- subset(a, a$x >= 340 & a$x <= 560 & a$y >= 655 & a$y <= 680)
      midrange11 <- subset(a, a$x >= 370 & a$x <= 560 & a$y >= 680 & a$y <= 705)
      midrange12 <- subset(a, a$x >= 400 & a$x <= 560 & a$y >= 705 & a$y <= 730)
      midrange13 <- subset(a, a$x >= 430 & a$x <= 560 & a$y >= 730 & a$y <= 745)
      midrange14 <- subset(a, a$x >= 460 & a$x <= 560 & a$y >= 745 & a$y <= 760)
      midrange15 <- subset(a, a$x >= 490 & a$x <= 560 & a$y >= 760 & a$y <= 775)
      midrange16 <- subset(a, a$x >= 515 & a$x <= 560 & a$y >= 775 & a$y <= 785)
      midrange17 <- subset(a, a$x >= 560 & a$x <= 950 & a$y >= 401 & a$y <= 540)
      midrange18 <- subset(a, a$x >= 560 & a$x <= 950 & a$y >= 540 & a$y <= 780)
      midrange19 <- subset(a, a$x >= 950 & a$x <= 1410 & a$y >= 80 & a$y <= 240)
      midrange20 <- subset(a, a$x >= 950 & a$x <= 1395 & a$y >= 240 & a$y <= 320)
      midrange21 <- subset(a, a$x >= 950 & a$x <= 1380 & a$y >= 320 & a$y <= 390)
      midrange22 <- subset(a, a$x >= 950 & a$x <= 1360 & a$y >= 390 & a$y <= 460)
      midrange23 <- subset(a, a$x >= 950 & a$x <= 1325 & a$y >= 460 & a$y <= 500)
      midrange24 <- subset(a, a$x >= 950 & a$x <= 1295 & a$y >= 500 & a$y <= 570)
      midrange25 <- subset(a, a$x >= 950 & a$x <= 1260 & a$y >= 570 & a$y <= 605)
      midrange26 <- subset(a, a$x >= 950 & a$x <= 1230 & a$y >= 605 & a$y <= 630)
      midrange27 <- subset(a, a$x >= 950 & a$x <= 1200 & a$y >= 630 & a$y <= 655)
      midrange28 <- subset(a, a$x >= 950 & a$x <= 1170 & a$y >= 655 & a$y <= 680)
      midrange29 <- subset(a, a$x >= 950 & a$x <= 1140 & a$y >= 680 & a$y <= 705)
      midrange30 <- subset(a, a$x >= 950 & a$x <= 1110 & a$y >= 705 & a$y <= 730)
      midrange31 <- subset(a, a$x >= 950 & a$x <= 1080 & a$y >= 730 & a$y <= 745)
      midrange32 <- subset(a, a$x >= 950 & a$x <= 1050 & a$y >= 745 & a$y <= 760)
      midrange33 <- subset(a, a$x >= 950 & a$x <= 1020 & a$y >= 760 & a$y <= 775)
      midrange34 <- subset(a, a$x >= 950 & a$x <= 995 & a$y >= 775 & a$y <= 785)
      midrange_df = rbind(midrange1, midrange2, midrange3, midrange4, midrange5,
                          midrange6, midrange7, midrange8, midrange9, midrange10,
                          midrange11, midrange12, midrange13, midrange14, midrange15,
                          midrange16, midrange17, midrange18, midrange19, midrange20,
                          midrange21, midrange22, midrange23, midrange24, midrange25,
                          midrange26, midrange27, midrange28, midrange29, midrange30,
                          midrange31, midrange32, midrange33, midrange34)
      midrange_fg = round(((nrow(midrange_df[midrange_df$outcome == 'made',]) / nrow(midrange_df))*100),1)
      midrange_fg[midrange_fg=='NaN'] = 0

      #subset right corner 3s
      right_corner_3 <- subset(a, a$x >= -60 & a$x <= 90 & a$y >= -10 & a$y <= 300)
      right_corner_3_fg = round(((nrow(right_corner_3[right_corner_3$outcome == 'made',]) / nrow(right_corner_3))*100),1)
      right_corner_3_fg[right_corner_3_fg=='NaN'] = 0

      #subset left corner 3s
      left_corner_3 <- subset(a, a$x >= 1420 & a$x <= 1700 & a$y >= -10 & a$y <= 300)
      left_corner_3_fg = round(((nrow(left_corner_3[left_corner_3$outcome == 'made',]) / nrow(left_corner_3))*100),1)
      left_corner_3_fg[left_corner_3_fg=='NaN'] = 0

      #subset above the break 3s

      above_the_break1 <- subset(a, a$x >= 1420 & a$x <= 1700 & a$y >= 300 & a$y <= 320)
      above_the_break2 <- subset(a, a$x >= 1390 & a$x <= 1700 & a$y >= 320 & a$y <= 390)
      above_the_break3 <- subset(a, a$x >= 1370 & a$x <= 1700 & a$y >= 390 & a$y <= 460)
      above_the_break4 <- subset(a, a$x >= 1335 & a$x <= 1700 & a$y >= 460 & a$y <= 500)
      above_the_break5 <- subset(a, a$x >= 1305 & a$x <= 1700 & a$y >= 500 & a$y <= 570)
      above_the_break6 <- subset(a, a$x >= 1270 & a$x <= 1700 & a$y >= 570 & a$y <= 605)
      above_the_break7 <- subset(a, a$x >= 1240 & a$x <= 1700 & a$y >= 605 & a$y <= 630)
      above_the_break8 <- subset(a, a$x >= 1210 & a$x <= 1700 & a$y >= 630 & a$y <= 655)
      above_the_break9 <- subset(a, a$x >= 1180 & a$x <= 1700 & a$y >= 655 & a$y <= 680)
      above_the_break10 <- subset(a, a$x >= 1150 & a$x <= 1700 & a$y >= 680 & a$y <= 705)
      above_the_break11 <- subset(a, a$x >= 1120 & a$x <= 1700 & a$y >= 705 & a$y <= 730)
      above_the_break12 <- subset(a, a$x >= 1090 & a$x <= 1700 & a$y >= 730 & a$y <= 745)
      above_the_break13 <- subset(a, a$x >= 1060 & a$x <= 1700 & a$y >= 745 & a$y <= 760)
      above_the_break14 <- subset(a, a$x >= 1030 & a$x <= 1700 & a$y >= 760 & a$y <= 775)
      above_the_break15 <- subset(a, a$x >= 1005 & a$x <= 1700 & a$y >= 775 & a$y <= 785)
      above_the_break16 <- subset(a, a$x >= 980 & a$x <= 1700 & a$y >= 785 & a$y <= 795)
      above_the_break17 <- subset(a, a$x >= 955 & a$x <= 1700 & a$y >= 795 & a$y <= 805)
      above_the_break18 <- subset(a, a$x >= 930 & a$x <= 1700 & a$y >= 805 & a$y <= 815)
      above_the_break19 <- subset(a, a$x >= 905 & a$x <= 1700 & a$y >= 815 & a$y <= 825)
      above_the_break20 <- subset(a, a$x >= -10 & a$x <= 90 & a$y >= 300 & a$y <= 320)
      above_the_break21 <- subset(a, a$x >= -10 & a$x <= 105 & a$y >= 320 & a$y <= 390)
      above_the_break22 <- subset(a, a$x >= -10 & a$x <= 135 & a$y >= 390 & a$y <= 460)
      above_the_break23 <- subset(a, a$x >= -10 & a$x <= 170 & a$y >= 460 & a$y <= 500)
      above_the_break24 <- subset(a, a$x >= -10 & a$x <= 200 & a$y >= 500 & a$y <= 570)
      above_the_break25 <- subset(a, a$x >= -10 & a$x <= 235 & a$y >= 570 & a$y <= 605)
      above_the_break26 <- subset(a, a$x >= -10 & a$x <= 265 & a$y >= 605 & a$y <= 630)
      above_the_break27 <- subset(a, a$x >= -10 & a$x <= 295 & a$y >= 630 & a$y <= 655)
      above_the_break28 <- subset(a, a$x >= -10 & a$x <= 325 & a$y >= 655 & a$y <= 680)
      above_the_break29 <- subset(a, a$x >= -10 & a$x <= 355 & a$y >= 680 & a$y <= 705)
      above_the_break30 <- subset(a, a$x >= -10 & a$x <= 385 & a$y >= 705 & a$y <= 730)
      above_the_break31 <- subset(a, a$x >= -10 & a$x <= 415 & a$y >= 730 & a$y <= 745)
      above_the_break32 <- subset(a, a$x >= -10 & a$x <= 445 & a$y >= 745 & a$y <= 760)
      above_the_break33 <- subset(a, a$x >= -10 & a$x <= 475 & a$y >= 760 & a$y <= 775)
      above_the_break34 <- subset(a, a$x >= -10 & a$x <= 500 & a$y >= 775 & a$y <= 785)
      above_the_break35 <- subset(a, a$x >= -10 & a$x <= 525 & a$y >= 785 & a$y <= 795)
      above_the_break36 <- subset(a, a$x >= -10 & a$x <= 550 & a$y >= 795 & a$y <= 805)
      above_the_break37 <- subset(a, a$x >= -10 & a$x <= 575 & a$y >= 805 & a$y <= 815)
      above_the_break38 <- subset(a, a$x >= -10 & a$x <= 600 & a$y >= 815 & a$y <= 825)
      above_the_break39 <- subset(a, a$x >= -10 & a$x <= 1700 & a$y >= 825 & a$y <= 845)
      above_the_break40 <- subset(a, a$x >= -10 & a$x <= 1700 & a$y >= 845 & a$y <= 955)
      above_the_break_df = rbind(above_the_break1, above_the_break2, above_the_break3, above_the_break4, above_the_break5,
                                 above_the_break6, above_the_break7, above_the_break8, above_the_break9, above_the_break10,
                                 above_the_break11, above_the_break12, above_the_break13, above_the_break14, above_the_break15,
                                 above_the_break16, above_the_break17, above_the_break18, above_the_break19, above_the_break20,
                                 above_the_break21, above_the_break22, above_the_break23, above_the_break24, above_the_break25,
                                 above_the_break26, above_the_break27, above_the_break28, above_the_break29, above_the_break30,
                                 above_the_break31, above_the_break32, above_the_break33, above_the_break34, above_the_break35,
                                 above_the_break36, above_the_break37, above_the_break38, above_the_break39, above_the_break40)

      above_the_break_fg = round(((nrow(above_the_break_df[above_the_break_df$outcome == 'made',]) / nrow(above_the_break_df))*100),1)
      above_the_break_fg[above_the_break_fg=='NaN'] = 0

      shots_by_zone_df = data.frame(Zone = c('In the Paint',
                                             'Mid-range',
                                             'Above the break 3',
                                             'Left Corner 3',
                                             'Right Corner 3'),
                                    FGP = c(in_the_paint_fg,
                                            midrange_fg,
                                            above_the_break_fg,
                                            left_corner_3_fg,
                                            right_corner_3_fg))
      shots_by_zone_df$FGP = as.integer(shots_by_zone_df$FGP)
      names(shots_by_zone_df)[2] = 'FG%'
      return(shots_by_zone_df)
    })

    #render shots by zone table

    output$shot_tab = renderTable({
      tab = get_shots_zone_tab()
      tab
    }, options = list(dom = 't'))


    #render shot chart

    output$shot_chart = renderPlot({

      player = input$player

      #get selected player name and team name

      name = gsub(' [(].*', '', player)
      team = gsub('.*[(]', '', player)
      team = gsub('[)]', '', team)

      #subset shots

      shots = dat[dat$player == name & dat$team == team,]
      names(shots)[1] = 'Outcome'

      #define number of bins for heatmap

      if(nrow(shots<=5)){
        selected_bins = 6
      } else if(nrow(shots)>5 & nrow(shots)<=30) {
        selected_bins = 8
      } else if(nrow(shots)>30) {
        selected_bins = 10
      }

      #create data.frame for geom_text
      text_df = data.frame(x = 755,
                           y = c(1060, 985),
                           lab = c(name, team))

      #create plots

      #Scatter

      if(input$chart_type == 'Scatter') {

        plot = ggplot() +
          annotation_custom(field, -60, 1570, -10, 1150) +
          xlim(-60, 1570) +
          ylim(-10, 1150) +
          geom_point(data = shots, aes(x,y, colour = Outcome), size = 6, alpha = 0.7) +
          geom_text(data = text_df, aes(x,y,label = lab), size = 7, colour = 'white', family = 'AvantGarde',fontface = 'plain') +
          scale_color_manual(values = c('#27ae60', '#e74c3c')) +
          guides(colour = FALSE) +
          annotation_custom(rasterGrob(logo),
                            xmin = 1200, xmax = 1500, ymin = 950, ymax = 1150) +
          theme(line = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                panel.background = element_blank(),
                axis.text.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                strip.background = element_blank(),
                panel.spacing = element_blank(),
                legend.position = 'bottom',
                legend.background = element_blank())

        print(plot)

        #Heatmap

      } else if(input$chart_type == 'Heatmap') {

        plot = ggplot(data = shots, aes(x,y)) +
          annotation_custom(field_heat, -60, 1570, -10, 1150) +
          xlim(-60, 1570) +
          ylim(-10, 1150) +
          stat_density2d(aes(alpha =..level.., fill = ..level..), geom="polygon", bins = selected_bins, n = 500) +
          scale_fill_gradient(low = '#FAFAFA', high = '#FFC107') +
          scale_alpha(range = c(0, 0.8)) +
          guides(alpha = FALSE, fill = FALSE) +
          geom_text(data = text_df, aes(x,y,label = lab), size = 7, colour = 'white', family = 'AvantGarde',fontface = 'plain') +
          annotation_custom(rasterGrob(logo),
                            xmin = 1200, xmax = 1500, ymin = 950, ymax = 1150) +
          theme(line = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                panel.background = element_blank(),
                axis.text.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                strip.background = element_blank(),
                panel.spacing = element_blank(),
                legend.position = 'bottom',
                legend.background = element_blank())
        print(plot)
      }

    })


  }

  dput(server, paste(app_name, '/', 'server.R', sep = ''))

  #run saved app

  shinyApp(ui = ui_run, server = server_run)

}

}

