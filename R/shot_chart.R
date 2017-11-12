#'Visualize shots data
#'
#'Create scatter charts or heatmaps from shots data download with get_shots_data_fibalivestats()
#'or get_shots_data_livefibaeurope(). This function creates one shot chart from shots data.
#'If you want to get multiple shot charts use shots_dashboard()
#'
#'@param shots_df data.frame, shots data downloaded with get_shots_data_fibalivestats() or get_shots_data_livefibaeurope()
#'
#'@param title character, shot chart title, 'Shot chart' set by default
#'
#'@param type character, type of shot chart to create, use 'Scatter' for scatter plot
#'or 'Heatmap' for heatmap
#'
#'@seealso  \code{\link[euRobasket]{shots_dashboard}}
#'
#'@examples
#'#get heatmap for M. Ponitka from Basketball Champions League shots example
#'#get data using data(), it will be stored in cl_shots
#'data("cl_shots")
#'
#'#subset M. PONITKA shots
#'shots = cl_shots[cl_shots$player == 'M. PONITKA',]
#'
#'#plot heatmap
#'shot_chart(shots, title = 'M. Ponitka', type = 'Heatmap')




shot_chart = function(shots_df, title = 'Shot chart', type = 'Scatter') {
  
  dat = shots_df
  
  #define size of title, stop if title is too long
  
  title_length = length(strsplit(title, split = '')[[1]])
  
  if(title_length <= 40) {
    title_size = 7
  } else if(title_length > 40 & title_length <= 45) {
    title_size = 6
  } else if(title_length > 45 & title_length <= 50) {
    title_size = 5
  } else if(title_length > 50 & title_length <= 65) {
    title_size = 4
  } else if(title_length > 65) {
    stop('Title is too long.')
  }
  
  #create data frame with title
  
  text_df = data.frame(x = 755,
                       y = 1060,
                       lab = title)
  
  #load package logo
  
  
  logo = readPNG(system.file('data', 'white_logo.png', package = 'euRobasket'))
  
  #create scatter plot
  
  if(type == 'Scatter') {
    
    #load court
    
    scatter = readPNG(system.file('data', 'scatter.png', package = 'euRobasket'))
    scatter = rasterGrob(scatter, width = unit(1.0, 'npc'), height = unit(1.0, 'npc'))
    
    #create scatter plot
    
    plot = ggplot() +
      annotation_custom(scatter, -60, 1570, -10, 1150) +
      xlim(-60, 1570) +
      ylim(-10, 1150) +
      geom_point(data = dat, aes(x,y, colour = outcome), size = 6, alpha = 0.7) +
      geom_text(data = text_df, aes(x,y,label = lab), size = title_size, colour = 'white', family = 'AvantGarde',fontface = 'plain') +
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
    
  } else if(type == 'Heatmap') {
    
    #load court
    
    heatmap = readPNG(system.file('data', 'heatmap.png', package = 'euRobasket'))
    heatmap = rasterGrob(heatmap, width = unit(1.0, 'npc'), height = unit(1.0, 'npc'))
    
    #define number of bins for heatmap
    
    if(nrow(dat)<=5){
      selected_bins = 6
    } else if(nrow(dat)>5 & nrow(dat)<=30) {
      selected_bins = 8
    } else if(nrow(dat)>30) {
      selected_bins = 10
    }
    
    #create heatmap
    
    plot = ggplot(data = dat, aes(x,y)) +
      annotation_custom(heatmap, -60, 1570, -10, 1150) +
      xlim(-60, 1570) +
      ylim(-10, 1150) +
      stat_density2d(aes(alpha =..level.., fill = ..level..), geom="polygon", bins = selected_bins, n = 500) +
      scale_fill_gradient(low = '#FAFAFA', high = '#FFC107') +
      scale_alpha(range = c(0, 0.8)) +
      guides(alpha = FALSE, fill = FALSE) +
      geom_text(data = text_df, aes(x,y,label = lab), size = title_size, colour = 'white', family = 'AvantGarde',fontface = 'plain') +
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
  
}
