suppressWarnings(suppressMessages({
  library(shiny)
  library(glue)
  library(RSQLite)
  library(shinydashboard)
  #library(shinydashboardPlus)
  library(shinymanager)
  library(shinyalert)
  library(shinycssloaders)
  library(shinyWidgets)
  library(DT)
  library(reactable)
  library(reactablefmtr)
  library(plyr)
  library(dplyr)
  library(tidyverse)
  library(tidyr)
  library(utils)
  library(stats)
  library(rvest)
  library(GeomMLBStadiums)
  library(ggplot2)
  library(ggpubr)
  library(grDevices)
  library(gridExtra)
  library(plotly)
  library(openxlsx)
  library(janitor)
  library(baseballr)
  library(ggh4x)
  library(ggimage)
  # library(tonybaseball)
  #library(eeptools) 
}))

db <- dbConnect(SQLite(), "frontier_league.sqlite")

# db <- dbConnect(RMySQL::MySQL(), dbname = "frontier_league", host = "frontier-league.czcooiea00wp.us-east-2.rds.amazonaws.com",
#                 port = 3306, user = "admin", password = "boomers25")
# tonybaseball::pitch_type_colors
pitch_type_colors <- c('FB' = 'red', 'Fastball' = 'red',  '4S' = '#cd0000', 'FourSeamFastBall' = '#cd0000',
                       '2S' = '#FF5404', 'TwoSeamFastBall' = '#FF5404', 'SI' = '#a34700', 'Sinker' = '#a34700',
                       'CT' = '#dcba01',  'Cutter' = '#dcba01',
                       'CB' = 'darkgreen', 'Curveball' = 'darkgreen', 'SL'='#33A8FF', 'Slider'='#33A8FF',
                       'Sweeper' = '#55ccab','SW' = '#55ccab',
                       'CH'='violet', 'Changeup'='violet', 'ChangeUp'='violet', 'ChangeUp' = 'violet',
                       'OT' = 'black', 'Other' = 'black',
                       'SPL' = '#4F1567', 'Splitter' = '#4F1567','KN' = '#4d4d4d', 'Knuckleball' = '#4d4d4d')


pitch_colors = data.frame(TaggedPitchType = c("Fastball", "FourSeamFastBall", "TwoSeamFastBall", "Sinker", "Cutter", 
                                              "Curveball", "Slider", "Sweeper",
                                              "Changeup", "Splitter", "Knuckleball", "Other"),
                          PitchCode = c('FB', '4S', '2S', 'SI', 'CT', 'CB', 'SL', 'SW', 'CH', 'SPL', 'KN', 'OT'),
                          Color = c('red', "#cd0000", "#FF5404", '#a34700', '#dcba01', 
                                    'darkgreen', '#33A8FF', "#55ccab",
                                    'violet',  '#4F1567',  '#4d4d4d',  'black'))


pitch_type_factor_and_recode <- function(data){
  data <- data %>%
    mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'FourSeamFastBall', 'TwoSeamFastBall', 'Sinker', 'Cutter',
                                                                'Curveball', 'Slider', 'Sweeper', 'Changeup', 'ChangeUp', 'Splitter', 'Knuckleball', 'Other') ),
           TaggedPitchType = dplyr::recode(TaggedPitchType, Fastball = "FB", FourSeamFastBall = '4S', TwoSeamFastBall = '2S',
                                           Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                           Cutter = 'CT', Changeup = 'CH', ChangeUp = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL', Sweeper = 'SW'  ) )
  
  return(data)
}

pitch_type_factor <- function(data){
  data <- data %>%
    mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'FourSeamFastBall', 'TwoSeamFastBall', 'Sinker', 'Cutter',
                                                                'Curveball', 'Slider', 'Sweeper', 'Changeup', 'ChangeUp', 'Splitter', 'Knuckleball', 'Other') ),
    )
  
  return(data)
}

pitch_type_recode <- function(data){
  data <- data %>%
    mutate(TaggedPitchType = dplyr::recode(TaggedPitchType, Fastball = "FB", FourSeamFastBall = '4S', TwoSeamFastBall = '2S',
                                           Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                           Cutter = 'CT', Changeup = 'CH', ChangeUp = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL', Sweeper = 'SW'  )
    )
  
  return(data)
}


pitch_data_lg_avg <- dpitch_data_lg_avg <- dbGetQuery(db, "
  SELECT *
  FROM metrics_pitch_league_averages")

arm_angle_categories <- function(df) {
  bins <- c(0, 30, 60, 90, 120, 180)
  # bins <- c(90, 60, 30, 0, -30, -90)
  
  labels <- c('Overhand', 'High Three-Quarters', 'Low Three-Quarters', 'Sidearm', 'Submarine')
  
  df <- df %>%
    mutate(
      arm_angle_type = cut(arm_angle, breaks = bins, labels = labels, right = FALSE)
    )
  
  return(df)
}
{
  # code for the pitcher's mound in the ARM ANGLE plot
  df <- data.frame(x = 0.5, y = 0)
  theta <- seq(0, pi, length.out = 100)  # Change the range to create a semi-circle that is right-side up for the mound!
  r <- 40  # The horizontal range from -40 to 40 for the mound!
  # Calculate the x and y coordinates
  mound <- data.frame(
    x = r * cos(theta),
    y = 4 * sin(theta)
  )
}
fl_avg_p <- dbGetQuery(db, "SELECT * FROM metrics_pitch_league_averages ") %>%
  pitch_type_factor_and_recode()%>%
  arrange(TaggedPitchType)%>%
  rename('Pitch' = TaggedPitchType)


hot_cold <- c('#242766', '#4e55d9',  '#babeff',  "#ebebeb", '#ffbabf', '#bd3e47', "#780008")
higher_lower <- c('#C3B700', '#E7DE55', '#F8FFC1', "#ebebeb", '#E6FFE8', '#A2CEA6', '#00840D')

get_quantiles <- function(pitch_type, quantiles_summary, velo_or_spin) {
  quantiles_summary %>%
    filter(TaggedPitchType == pitch_type) %>%
    select(starts_with(velo_or_spin)) %>%
    unlist() %>%
    as.numeric()
}

colors_df_ex <- t(data.frame(hot_cold, higher_lower)) %>%
  as.data.frame() %>%
  mutate(Color_code = c('better or worse', 'higher or lower'), .before = 1) %>%
  mutate(V1 = '10 %ile or less',
         V2 = '10 to 30 %ile',
         V3 = '30 %ile to 45 %ile',
         V4 = '45 %ile to 55 %ile',
         V5 = '55 %ile to 70 %ile',
         V6 = '70 %ile to 90 %ile',
         V7 = '90 %ile or higher'
  )




pitcher_plot <- function(pitcher_name, plot_type, pitch_data_df) {
  
  
  
  if(pitcher_name %in% pitch_data_df$Pitcher){
    # INDIVIDUAL PITCHER PITCH DATA -----
    p <- pitch_data_df %>%
      filter(Pitcher == pitcher_name) %>%
      filter(TaggedPitchType != '' | is.na(TaggedPitchType)) %>% 
      pitch_type_factor_and_recode()
    
    
    # INDIVIDUAL PITCHER AVERAGE PITCH METRICS ----
    p_mean <- suppressMessages(
      pitch_data_df %>%
        filter(Pitcher == pitcher_name) %>%
        filter(TaggedPitchType != '' | is.na(TaggedPitchType)) %>% 
        pitch_type_factor_and_recode() %>%
        group_by(Pitcher, PitcherThrows, TaggedPitchType) %>%
        summarise(across(c(InducedVertBreak, HorzBreak, SpinAxis), ~ mean(.,na.rm = T)),
                  usage = n()) %>%
        mutate(usage = round(usage / sum(usage),3)*100,
               scaled_usage = (usage - min(usage)) / (max(usage) - min(usage)) * (40 - 20) + 20)
    )
    
    # LEAGUE AVERAGE PITCH METRICS MATCHING INDIVIDUAL PITCHERS ARSENAL----
    p_lg <- pitch_data_lg_avg %>%
      filter(PitcherThrows %in% p_mean$PitcherThrows) %>%
      filter(TaggedPitchType %in% p_mean$TaggedPitchType)
    
    # INDIVIDUAL PITCHER AVERAGE ARM ANGLE AND RELEASE DATA----
    p_arm <- suppressMessages(
      pitch_data_df %>%
        filter(Pitcher == pitcher_name) %>%
        group_by(Pitcher, PitcherThrows) %>%
        summarise(PitcherTeam = paste(unique(PitcherTeam), collapse = ', '),
                  height_inches = mean(height_inches, na.rm = T),
                  shoulder_pos = mean(shoulder_pos, na.rm = T),
                  release_pos_x = median(RelSide * 12, na.rm = T),
                  release_pos_z = median(RelHeight * 12, na.rm = T),
                  arm_angle = mean(arm_angle, na.rm = T),
                  arm_angle_savant = mean(arm_angle_savant, na.rm = T)
        ) %>%
        arm_angle_categories() %>%
        # This is to scale the arm angle line/point to fit into the savant and movement plots
        mutate(relx = case_when(
          release_pos_x > 20 ~ 20,
          release_pos_z > 20 ~ 20 * (release_pos_x / (release_pos_z - shoulder_pos)),
          TRUE ~ release_pos_x
        ),
        relz = case_when(
          release_pos_x > 20 ~ 20 * ((release_pos_z - shoulder_pos) / release_pos_x),
          release_pos_z > 20 ~ 20,
          TRUE ~ release_pos_z
        ),
        arm_path = 'Arm Path'
        ) %>%
        mutate(arm_length = height_inches * .39, # Average arm length is roughlt 39% of height
               slope = (release_pos_z - shoulder_pos) / (release_pos_x - 0),
               arm_dist = sqrt((release_pos_x - 0)^2 + (release_pos_z - shoulder_pos)^2),
               arm_scale = arm_length / arm_dist,
               should_x = case_when(
                 arm_angle_savant >= 40 ~ 0,
                 between(arm_angle_savant, 10, 40)  ~ 0,
                 arm_angle_savant < 10 ~ 0,
               ),
               should_y = case_when( # changes the height of the shoulder based on which arm angle / arm angle png 
                 arm_angle_savant >= 40 ~ 62.5,
                 between(arm_angle_savant, 10, 40)  ~ 56,
                 arm_angle_savant < 10 ~ 45,
                 
               ),
               rel_x = should_x + (arm_scale * (release_pos_x - should_x)), # calculates new release point along the original slope
               rel_z = shoulder_pos + (arm_scale * (release_pos_z - shoulder_pos)) + should_y - (shoulder_pos), # calculates new release point along the original slope
               arm_path = 'Arm Path'
        )
    )
    
    p <- p  %>% filter(!is.na(HorzBreak) & !is.na(InducedVertBreak))
    
    # this sets the pitch colors in the plotly versions of the code
    p_c <- pitch_colors %>%
      filter(PitchCode %in% p_mean$TaggedPitchType)
    
    p_c <- setNames(as.character(p_c$Color), p_c$PitchCode)
    
    
    if(nrow(p) > 0 & plot_type == 'savant') { # SAVANT PLOT CODE ----
      
      caption <- paste(
        'Speed:', round(p$RelSpeed,1),
        "\nSpin:", round(p$SpinRate),
        "\nAxis:", round(p$SpinAxis),
        "\nSpin Eff%:", round(p$yt_Efficiency),
        "\nDate:", p$Date,
        "\nBallpark:", p$HomeTeamCode
        
      )
      
      ggplotly(
        ggplot(data = p, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
          geom_polygon(data = circle, aes(x = x, y = y), fill = "#e5f3f3", color = "#e5f3f3", inherit.aes = F) +
          # DEGREE ANNOTATION
          annotate('text', x = 26, y = 1, label = '0°', size = 3)+
          annotate('text', x = -26, y = 1, label = '0°', size = 3)+
          annotate('text', x = 0, y = 26, label = '90°', size = 3)+
          # BREAK ANNOTATION
          annotate('text', x = 10, y = -1.5, label = '12"', size = 3)+
          annotate('text', x = 22, y = -1.5, label = '24"', size = 3)+
          annotate('text', x = -4.5, y = -1.5, label = '6"', size = 3)+
          annotate('text', x = -10, y = -1.5, label = '12"', size = 3)+
          annotate('text', x = -16, y = -1.5, label = '18"', size = 3)+
          annotate('text', x = -22, y = -1.5, label = '24"', size = 3)+
          annotate('text', y = 10, x = -2, label = '12"', size = 3)+
          annotate('text', y = 22, x = -2, label = '24"', size = 3)+
          annotate('text', y = -10, x = -2, label = '12"', size = 3)+
          annotate('text', y = -22, x = -2, label = '24"', size = 3)+
          geom_path(data = data.frame(
            x = 6 * cos(seq(0, 2*pi, length.out = 100)),  y = 6 * sin(seq(0, 2*pi, length.out = 100)) ), 
            aes(x = x, y = y), linetype = "dashed", color = "gray", inherit.aes = F) +
          geom_path(data = data.frame(
            x = 12 * cos(seq(0, 2*pi, length.out = 100)), y = 12 * sin(seq(0, 2*pi, length.out = 100)) ), 
            aes(x = x, y = y), linetype = "solid", color = "gray", inherit.aes = F) +
          geom_path(data = data.frame(
            x = 18 * cos(seq(0, 2*pi, length.out = 100)),  y = 18 * sin(seq(0, 2*pi, length.out = 100)) ), 
            aes(x = x, y = y), linetype = "dashed", color = "gray", inherit.aes = F) +
          geom_path(data = data.frame(
            x = 24 * sin(seq(0, 2*pi, length.out = 100)),  y = 24 * cos(seq(0, 2*pi, length.out = 100)) ), 
            aes(x = x, y = y), linetype = "solid", color = "gray", inherit.aes = F) +
          coord_fixed()+
          geom_segment(x = 0, y = -25, xend = 0, yend = 25, linewidth = .5, color = "grey55") +
          geom_segment(x = -25, y = 0, xend = 25, yend = 0, linewidth = .2, color = "grey55") +
          geom_segment(x = 25, y = 0, xend = 25, yend = 0, linewidth = .2, color = "grey55") +
          geom_segment(y=19.91,yend=21.65, x= 11.5, xend=12.5, color='grey55')+
          geom_segment(x=19.91,xend=21.65, y= 11.5, yend=12.5, color='grey55')+
          geom_segment(y=19.91,yend=21.65, x= -11.5, xend=-12.5, color='grey55')+
          geom_segment(x=-19.91,xend=-21.65, y= 11.5, yend=12.5, color='grey55')+
          annotate('text', x = 22.5, y = 13.5, label = '30°', size = 3)+
          annotate('text', x = -22.5, y = 13.5, label = '30°', size = 3)+
          annotate('text', y = 22.5, x = 13.5, label = '60°', size = 3)+
          annotate('text', y = 22.5, x = -13.5, label = '60°', size = 3)+
          theme(legend.position = "left",
                panel.background = element_blank(),
                legend.text = element_text(size = 8),
                axis.title = element_text(size = 10),
                panel.grid = element_blank(),
                plot.title = element_text(hjust = .5),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
          ) + 
          guides(color = "none")+
          scale_size_continuous(range = c(4, 9), guide = 'none') +
          labs(color = "", fill = 'Pitch',x = "", y = "", 
               title = paste0(round(p_arm$arm_angle_savant),"° ", p_arm$arm_angle_type) ),
        color = ~TaggedPitchType,
        
      ) %>%
        plotly::style(hoverinfo = "none", traces = 1:29)%>%
        plotly::layout(autosize = T,
                       title = list( text = paste0("Pitch Movement and Arm Angle" ),
                                     x = 0.5,xanchor = 'center') ,showlegend = TRUE)  %>%
        # INDIVIDUAL POINTS
        add_markers(data = p,
                    x = ~HorzBreak,  y = ~InducedVertBreak,  color = ~TaggedPitchType, colors = p_c,
                    marker = list(symbol = "circle",  opacity = 0.7, size = 8,    
                                  line = list(  color = 'black',   width = .5   )   ),
                    showlegend = TRUE,  legendgroup = "group1", visible = T ,
                    legendgrouptitle = list(text = "Pitches", font = list(size = 10)), hovertext = caption) %>%
        # GROUP POINTS
        plotly::add_markers(data = p_mean,
                            x = ~HorzBreak,  y = ~InducedVertBreak,  color = ~TaggedPitchType, colors = p_c,
                            marker = list(
                              symbol = "circle-dot",  opacity = 0.5, size = p_mean$scaled_usage,
                              sizeref = 0.0,   sizemode = "area",   
                              line = list(color = 'black', width = 2) ),  
                            showlegend = TRUE,  legendgroup = "group2", visible = "legendonly", 
                            legendgrouptitle = list(text = "Pitch Avg", font = list(size = 10))
        ) %>%
        # Arm Path
        add_segments(x = 0, xend = p_arm$relx, y = 0, yend = p_arm$relz,
                     line = list(color = 'grey', width = 7), opacity = .5, name = 'Arm',
                     legendgroup = "group4", visible = T ) %>%
        # Release Point
        add_markers(x = p_arm$relx, y = p_arm$relz,
                    marker = list(  symbol = "diamond",  size = 15,   color = 'orange', opacity = .7,    
                                    line = list(  color = 'black',   width = 1.5   )  ),
                    name = "Release",
                    legendgroup = "group4", visible = T,
                    legendgrouptitle = list(text = "Arm Angle", font = list(size = 10)))  %>%
        # LEAGUE AVERAGE
        add_markers(data = p_lg,
                    x = ~HorzBreak, y = ~InducedVertBreak,
                    color = ~TaggedPitchType, colors = p_c,
                    marker = list(
                      symbol = "circle-x-open", opacity = 0.5, size = 35,
                      line = list(  color = 'black',   width = 2   )
                    ),
                    showlegend = T,  legendgroup = "group3", visible = "legendonly",
                    legendgrouptitle = list(text = "League Avg", font = list(size = 10))
        ) %>%
        layout(legend = list(itemsizing = 'constant'))  %>%
        # Arm angle caption
        add_annotations(
          x = 0,  y = 28, xref = "x", yref = "y",
          text = paste0(round(p_arm$arm_angle_savant),"° Arm Angle - ", gsub("Three-Quarters", "3/4", p_arm$arm_angle_type)),
          showarrow = FALSE,
          font = list(size = 11, color = "#731209", face = 'bold')
        ) %>%
        # point size caption
        add_annotations(
          x = -23,  y = -20, xref = "x", yref = "y",
          text = "Pitch Avg point size\nrelative to usage",
          showarrow = FALSE,
          font = list(size = 11, color = "#731209", face = 'bold')
        ) 
      
      
    } else if(nrow(p) > 0 & plot_type == 'movement') {# MOVEMENT PLOT CODE ----
      
      caption <- paste(
        'Speed:', round(p$RelSpeed,1),
        "\nSpin:", round(p$SpinRate),
        "\nAxis:", round(p$SpinAxis),
        "\nSpin Eff%:", round(p$yt_Efficiency),
        "\nDate:", p$Date,
        "\nBallpark:", p$HomeTeamCode
        
      )
      
      # ellipse_df <- calculate_ellipse(p, "TaggedPitchType", "HorzBreak", "InducedVertBreak") 
      
      
      ggplotly(
        ggplot(data = p, aes(x = HorzBreak, y = InducedVertBreak )) +
          labs(color = 'Pitches', x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" ) + 
          coord_fixed()+
          xlim(-25, 25) + ylim(-25, 25) +
          geom_segment(x = 0, y = -25, xend = 0, yend = 25, linewidth = 1, color = "grey55", inherit.aes = F) +
          geom_segment(x = -25, y = 0, xend = 25, yend = 0, linewidth = 1, color = "grey55", inherit.aes = F) +
          theme(legend.position = "left",
                legend.text = element_text(size = 8),
                axis.title = element_text(size = 10)
          )+
          labs(color = "", fill = 'Pitch',x = "", y = "", 
               title = paste0(round(p_arm$arm_angle_savant),"° ", p_arm$arm_angle_type) ),
        color = ~TaggedPitchType,
      ) %>%
        plotly::layout(autosize = T,
                       title = list( text = paste0("Pitch Movement and Arm Angle" ),
                                     x = 0.5,xanchor = 'center') ,showlegend = TRUE)  %>%
        # # INDIVIDUAL POINTS
        add_markers(data = p,
                    x = ~HorzBreak,  y = ~InducedVertBreak,  color = ~TaggedPitchType, colors = p_c,
                    marker = list(symbol = "circle",  opacity = 0.7, size = 8,    
                                  line = list(  color = 'black',   width = .5   )   ),
                    showlegend = TRUE, visible = T ,
                    # legendgroup = "group1",
                    # legendgrouptitle = list(text = "Pitches", font = list(size = 10)),
                    hovertext = caption
        )%>%
        # GROUP POINTS
        plotly::add_markers(data = p_mean,
                            x = ~HorzBreak,  y = ~InducedVertBreak,  color = ~TaggedPitchType, colors = p_c,
                            marker = list(
                              symbol = "circle-dot",  opacity = 0.5, size = p_mean$scaled_usage,
                              sizeref = 0.0,   sizemode = "area",
                              line = list(color = 'black', width = 2) ),
                            showlegend = TRUE,  legendgroup = "group2", visible = "legendonly",
                            legendgrouptitle = list(text = "Pitch Avg", font = list(size = 10))
        ) %>%
        # Arm Path
        add_segments(x = 0, xend = p_arm$relx, y = 0, yend = p_arm$relz,
                     line = list(color = 'grey', width = 7), opacity = .5, name = 'Arm',
                     legendgroup = "group3", visible = T ) %>%
        # # Release Point
        add_markers(x = p_arm$relx, y = p_arm$relz,
                    marker = list(  symbol = "diamond",  size = 15,   color = 'orange', opacity = .7,
                                    line = list(  color = 'black',   width = 1.5   )  ),
                    name = "Release",
                    legendgroup = "group3", visible = T,
                    legendgrouptitle = list(text = "Arm Angle", font = list(size = 10)))
      
      
    } else if (nrow(p) > 0 & plot_type == 'arm_angle') { # ARM ANGLE PLOT CODE ----
      
      # PITCHING RUBBER COORDS
      rubber_xmin <- -9 
      rubber_xmax <- 9   
      rubber_ymin <- 4   
      rubber_ymax <- 4.5 
      
      # base of the arm_angle plot
      base_plot <- ggplot(df, aes(x, y)) + 
        geom_polygon(data = mound, aes(x = x, y = y), fill = "#8B4513") +
        xlim(-50,50) + ylim(0,100)+
        geom_rect(aes(xmin = rubber_xmin, xmax = rubber_xmax, ymin = rubber_ymin, ymax = rubber_ymax), 
                  fill = "white", color = "black") 
      
      if(p_arm$PitcherThrows == 'Right') {
        
        if(p_arm$arm_angle_savant >= 40) { 
          
          image_path <- "www/SavantPitchers_top_right_front-svg.png"
          
          base_plot +
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            labs(
              # title = paste(p_arm$Pitcher),
              title = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            )  +
            theme_void()+
            theme(plot.title = element_text(hjust = .5, size = 15)#,
                  # plot.subtitle = element_text(hjust = .5, size = 15)
            )
          
          
        } else if (between(p_arm$arm_angle_savant, 10,40)) {
          image_path <- "www/SavantPitchers_mid_right_front-svg.png"
          
          base_plot +
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            labs(
              # title = paste(p_arm$Pitcher),
              title = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            )  +
            theme_void()+
            theme(plot.title = element_text(hjust = .5, size = 15)#,
                  # plot.subtitle = element_text(hjust = .5, size = 15)
            )
          
        } else if(p_arm$arm_angle_savant < 10){
          image_path <- "www/SavantPitchers_low_right_front-svg.png"
          
          base_plot +
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            labs(
              # title = paste(p_arm$Pitcher),
              title = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            )  +
            theme_void()+
            theme(plot.title = element_text(hjust = .5, size = 15)#,
                  # plot.subtitle = element_text(hjust = .5, size = 15)
            )
          
        }
      } else if(p_arm$PitcherThrows == 'Left'){
        if(p_arm$arm_angle_savant >= 40) { 
          
          image_path <- "www/SavantPitchers_top_left_front-svg.png"
          
          base_plot +
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            labs(
              # title = paste(p_arm$Pitcher),
              title = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            )  +
            theme_void()+
            theme(plot.title = element_text(hjust = .5, size = 15)#,
                  # plot.subtitle = element_text(hjust = .5, size = 15)
            )
          
        } else if (between(p_arm$arm_angle_savant, 10,40)) {
          image_path <- "www/SavantPitchers_mid_left_front-svg.png"
          
          base_plot +
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            labs(
              # title = paste(p_arm$Pitcher),
              title = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            )  +
            theme_void()+
            theme(plot.title = element_text(hjust = .5, size = 15)#,
                  # plot.subtitle = element_text(hjust = .5, size = 15)
            )
          
        } else if(p_arm$arm_angle_savant < 10){
          image_path <- "www/SavantPitchers_low_left_front-svg.png"
          
          base_plot +
            geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(x=0, y =  p_arm$shoulder_pos + (p_arm$should_y - p_arm$shoulder_pos), 
                         xend = -p_arm$rel_x, 
                         yend = p_arm$rel_z , 
                         size = 8, color = "#6892a2", alpha = .5) +
            geom_point(x = -p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =7, stroke = 2)+
            labs(
              # title = paste(p_arm$Pitcher),
              title = paste0('Arm Angle: ', round(p_arm$arm_angle_savant), "° - ", gsub('Three-Quarters','3/4', p_arm$arm_angle_type))
            )  +
            theme_void()+
            theme(plot.title = element_text(hjust = .5, size = 15)#,
                  # plot.subtitle = element_text(hjust = .5, size = 15)
            )
          
          
        }
      } 
      
      
    } else {
      print('Plot Type or Pitcher not found. Try again!')
    }
    
  } else {
    print('Pitcher not found. Try again!')
  }
  
}


get_pitch_color <- function(value) {
  pitch_colors <- list(
    FB = list(background = "red", color = 'white'),
    `4S` = list(background = "#cd0000", color = 'white'),
    `2S` = list(background = "#FF5404", color = 'white'),
    SI = list(background = "#a34700", color = 'white'),
    CT = list(background = "#dcba01", color = 'black'),
    CB = list(background = "darkgreen", color = 'white'),
    SL = list(background = "#33A8FF", color = 'white'),
    SW = list(background = "#55ccab", color = 'white'),
    CH = list(background = "violet", color = 'white'),
    KN = list(background = "#4d4d4d", color = 'white'),
    SPL = list(background = "#4F1567", color = 'white')
  )
  pitch_colors[[value]]
}



# List of metrics to apply conditional styling
metrics <- c("Spin", "Spin Eff", "IVB", "HB", "VAA", 'Rel Height', "Rel Side",'Extension')

# Generate column definitions dynamically
metric_cols <- setNames(lapply(metrics, function(metric) {
  colDef(style = function(value, index) get_quantile_style(value, index, metric, higher_lower))
}), metrics)

# Construct the reactable

# Function to calculate luminance of a hex color
get_luminance <- function(hex_color) {
  # Convert hex to RGB
  rgb <- col2rgb(hex_color) / 255
  # Apply luminance formula
  return(0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3])
}

# Function to determine text color based on background luminance
get_text_color <- function(bg_color) {
  luminance <- get_luminance(bg_color)
  if (luminance > 0.5) {
    return("black")  # Light background → Black text
  } else {
    return("white")  # Dark background → White text
  }
}

# Updated quantile style function with better text contrast
get_quantile_style <- function(value, index, metric, color_palette) {
  if (is.na(value)) return(NULL)  # Handle missing values
  
  pitch_value <- table$Pitch[index]
  quantiles <- get_quantiles(pitch_value, quantiles_summary(), metric)
  
  if (is.null(quantiles) || length(quantiles) < 6 || any(is.na(quantiles))) {
    return(NULL)  # Return NULL if quantiles are missing or incomplete
  }
  
  # Determine background color
  for (i in seq_along(quantiles)) {
    if (!is.na(quantiles[i]) && value <= quantiles[i]) {
      bg_color <- color_palette[i]
      text_color <- get_text_color(bg_color)
      return(list(background = bg_color, color = text_color))
    }
  }
  
  # Default highest category
  bg_color <- color_palette[7]
  text_color <- get_text_color(bg_color)
  
  return(list(background = bg_color, color = text_color))
}



# STANDINGS ----
standings <- dbGetQuery(db, "SELECT * FROM standings")

midw_cent <- standings %>%
  filter(SEASON == 2025, 
         grepl("Mid",Conference),
         grepl("Cent", Division))

midw_west <- standings %>%
  filter(SEASON == 2025, 
         grepl("Mid",Conference),
         grepl("West", Division))

atl_north <- standings %>%
  filter(SEASON == 2025, 
         grepl("Atl",Conference),
         grepl("North", Division))

atl_east <- standings %>%
  filter(SEASON == 2025, 
         grepl("Atl",Conference),
         grepl("East", Division))
