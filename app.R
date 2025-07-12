suppressWarnings(suppressMessages({
  library(shiny)
  library(glue)
  library(RSQLite)
  library(shinydashboard)
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
  library(tonybsbl)
  library(zoo)
  library(gt)
  library(data.table)
}))

db = dbConnect(SQLite(), 
               "G:/.shortcut-targets-by-id/1gpEXlAsUgkGuM6l4rLGFyoTR2zHQs1SS/_SchaumburgBoomers/_Files/frontier_league.sqlite")

teams = dbGetQuery(db, 'select * from teams')

stevendata2 = dbGetQuery(db, 
  "SELECT Batter, Season, Date, PitchClass, PlateLocSide, PlateLocHeight, whiff, swing, woba_value, ExitSpeed, 
  Angle, is_pa, xwOBA, Balls, Strikes
  FROM pitch_data"
) %>%
  mutate(count = paste(Balls, Strikes, sep = "-"))

stevendata2$Date = as.Date(stevendata2$Date)

stevendata = stevendata2 %>%
  filter(is_pa == 1) %>%
  arrange(desc(Batter), -desc(Date), desc(is_pa)) %>%
  group_by(Batter, SEASON) %>%
  summarise(Date = Date, Season = SEASON, pa = cumsum(is_pa), woba_value = cumsum(woba_value),
            xwoba_value = cumsum(xwOBA), woba = woba_value/pa, xwoba = xwoba_value/pa,
            woba2 = rollmean(woba, k = 25, align = "right", fill = NA),
            xwoba2 = rollmean(xwoba, k = 25, align = "right", fill = NA))

stevenreactable = list(
  seasondata = aws_db_query(
    "SELECT Batter, BatterId, BatterTeam, Season, G, PA, BA, OBP, SLG, OPS, wOBA, xwOBA
    FROM stats_hitting_player_batted_ball"
  ) %>%
    mutate(BatterTeam = case_when(BatterTeam %in% c("BRO_ROX") ~ "Brockton Rox", BatterTeam %in% c("DOW_EAS") ~ "Down East Bird Dawgs",
                                  BatterTeam %in% c("EMP_STA") ~ "Empire State Greys", BatterTeam %in% c("EVA_OTT") ~ "Evansville Otters",
                                  BatterTeam %in% c("FLO_YAL") ~ "Florence Y'alls", BatterTeam %in% c("Florence Yalls") ~ "Florence Y'alls",
                                  BatterTeam %in% c("GAT_GRI") ~ "Gateway Grizzlies", BatterTeam %in% c("JOL_SLA") ~ "Joliet Slammers",
                                  BatterTeam %in% c("LAK_ERI") ~ "Lake Erie Crushers", BatterTeam %in% c("MIS_MUD") ~ "Mississippi Mud Monsters",
                                  BatterTeam %in% c("NEW_ENG") ~ "New England Knockouts", BatterTeam %in% c("NEW_JER") ~ "New Jersey Jackals",
                                  BatterTeam %in% c("NEW_YOR") ~ "New York Boulders", BatterTeam %in% c("OTT_TIT") ~ "Ottawa Titans",
                                  BatterTeam %in% c("QUE_CAP") ~ "Quebec Capitales", BatterTeam %in% c("SCH_BOO") ~ "Schaumburg Boomers",
                                  BatterTeam %in% c("SUS_COU") ~ "Sussex County Miners", 
                                  BatterTeam %in% c("Southern Illinois Miners") ~ "Sussex County Miners",
                                  BatterTeam %in% c("TRI_VAL") ~ "Tri-City ValleyCats", BatterTeam %in% c("TRO_AIG") ~ "Trois-Rivieres Aigles",
                                  BatterTeam %in% c("WAS_WIL") ~ "Washington Wild Things", BatterTeam %in% c("WIN_CIT") ~ "Windy City ThunderBolts",
                                  .default = BatterTeam),
           Status = 0),
  careerdata = aws_db_query(
    "SELECT Batter, BatterId, BatterTeam, Season, G, PA, BA, OBP, SLG, OPS, wOBA, xwOBA
    FROM stats_hitting_player_batted_ball_career"
  ) %>% mutate(BatterTeam = case_when(BatterTeam %in% c("BRO_ROX") ~ "Brockton Rox", BatterTeam %in% c("DOW_EAS") ~ "Down East Bird Dawgs",
                                      BatterTeam %in% c("EMP_STA") ~ "Empire State Greys", BatterTeam %in% c("EVA_OTT") ~ "Evansville Otters",
                                      BatterTeam %in% c("FLO_YAL") ~ "Florence Y'alls", BatterTeam %in% c("Florence Yalls") ~ "Florence Y'alls",
                                      BatterTeam %in% c("GAT_GRI") ~ "Gateway Grizzlies", BatterTeam %in% c("JOL_SLA") ~ "Joliet Slammers",
                                      BatterTeam %in% c("LAK_ERI") ~ "Lake Erie Crushers", BatterTeam %in% c("MIS_MUD") ~ "Mississippi Mud Monsters",
                                      BatterTeam %in% c("NEW_ENG") ~ "New England Knockouts", BatterTeam %in% c("NEW_JER") ~ "New Jersey Jackals",
                                      BatterTeam %in% c("NEW_YOR") ~ "New York Boulders", BatterTeam %in% c("OTT_TIT") ~ "Ottawa Titans",
                                      BatterTeam %in% c("QUE_CAP") ~ "Quebec Capitales", BatterTeam %in% c("SCH_BOO") ~ "Schaumburg Boomers",
                                      BatterTeam %in% c("SUS_COU") ~ "Sussex County Miners", 
                                      BatterTeam %in% c("Southern Illinois Miners") ~ "Sussex County Miners",
                                      BatterTeam %in% c("TRI_VAL") ~ "Tri-City ValleyCats", BatterTeam %in% c("TRO_AIG") ~ "Trois-Rivieres Aigles",
                                      BatterTeam %in% c("WAS_WIL") ~ "Washington Wild Things", BatterTeam %in% c("WIN_CIT") ~ "Windy City ThunderBolts",
                                      .default = BatterTeam),
               Status = 1)
) %>% rbindlist() %>%
  filter(Batter != "Team Totals") %>%
  arrange(desc(BatterId), -desc(Status)) %>%
  group_by(BatterId) %>%
  mutate(Season_n = n() - 1, Yr = case_when(is.na(Season) ~ Season_n, .default = Season)) %>%
  ungroup()

# db <- dbConnect(RMySQL::MySQL(), dbname = "frontier_league", host = "frontier-league.czcooiea00wp.us-east-2.rds.amazonaws.com",
#                 port = 3306, user = "admin", password = "boomers25")
# pitch_type_colors
pitch_type_colors <- c('FB' = 'red', 'Fastball' = 'red',  '4S' = '#cd0000', 'FourSeamFastBall' = '#cd0000',
                       '2S' = '#FF5404', 'TwoSeamFastBall' = '#FF5404', 'SI' = '#a34700', 'Sinker' = '#a34700',
                       'CT' = '#dcba01',  'Cutter' = '#dcba01',
                       'CB' = 'darkgreen', 'Curveball' = 'darkgreen', 'SL'='#325aa1', 'Slider'='#325aa1',
                       'Sweeper' = '#55ccab','SW' = '#55ccab',
                       'CH'='violet', 'Changeup'='violet', 'ChangeUp'='violet', 'ChangeUp' = 'violet',
                       'OT' = 'black', 'Other' = 'black',
                       'SPL' = '#4F1567', 'Splitter' = '#4F1567','KN' = '#4d4d4d', 'Knuckleball' = '#4d4d4d')


pitch_colors = data.frame(TaggedPitchType = c("Fastball", "FourSeamFastBall", "TwoSeamFastBall", "Sinker", "Cutter", 
                                              "Curveball", "Slider", "Sweeper",
                                              "Changeup", "Splitter", "Knuckleball", "Other"),
                          PitchCode = c('FB', '4S', '2S', 'SI', 'CT', 'CB', 'SL', 'SW', 'CH', 'SPL', 'KN', 'OT'),
                          Color = c('red', "#cd0000", "#FF5404", '#a34700', '#dcba01', 
                                    'darkgreen', '#325aa1', "#55ccab",
                                    'violet',  '#4F1567',  '#4d4d4d',  'black'))

# tonybaseball::savant_blue
# tonybaseball::savant_mid
# tonybaseball::savant_red
savant_blue <- "#325aa1"
savant_mid <- "#a8c1c3"
savant_red <- "#c91f26"

pitch_type_factor_and_recode <- function(data){
  data <- data %>%
    mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'FourSeamFastBall', 'TwoSeamFastBall', 'Sinker', 'Cutter',
                                                                'Curveball', 'Slider', 'Sweeper', 'Changeup', 'ChangeUp', 'Splitter', 'Knuckleball', 'Other') ),
           TaggedPitchType = dplyr::recode(TaggedPitchType, Fastball = "FB", FourSeamFastBall = '4S', TwoSeamFastBall = '2S',
                                           Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                           Cutter = 'CT', Changeup = 'CH', ChangeUp = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL', Sweeper = 'SW' ) )
  
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


pitch_data_lg_avg <- dbGetQuery(db, "
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

fl_avg_p <- dbGetQuery(db, "SELECT * FROM metrics_pitch_league_season_averages where SEASON = 2025") %>%
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
    SL = list(background = "#325aa1", color = 'white'),
    SW = list(background = "#55ccab", color = 'white'),
    CH = list(background = "violet", color = 'white'),
    KN = list(background = "#4d4d4d", color = 'white'),
    SPL = list(background = "#4F1567", color = 'white'),
    OT = list(background = 'black', color = 'white')
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

# hitting stats----
h <- dbGetQuery(db, 'select * from stats_hitting_player -- where SEASON = 2025') %>%
  mutate(across(c(AVG, OBP, SLG, wOBA), ~ round(.,3)))

x <- dbGetQuery(db, 'select * from stats_hitting_player_batted_ball -- where SEASON = 2025') %>%
  mutate(across(c(xBA, xwOBA, xSLG, wOBA), ~ round(.,3)),
         across(c(EV50, Barrel_pct, HardHit_pct, ExitVelo_max, ExitVelo), ~ round(.,1)),
  )%>%
  filter(Batter != 'Team Totals')

h_tm <- dbGetQuery(db, 'select * from stats_hitting_team -- where SEASON = 2025') %>%
  mutate(across(c(AVG, OBP, SLG, wOBA), ~ round(.,3)))

x_tm <- dbGetQuery(db, 'select * from stats_hitting_team_batted_ball -- where SEASON = 2025') %>%
  mutate(across(c(xBA, xwOBA, xSLG, wOBA), ~ round(.,3)),
         across(c(EV50, Barrel_pct, HardHit_pct, ExitVelo_max, ExitVelo), ~ round(.,1)),
  )

team <- dbGetQuery(db, 'select * from teams')

h_lg <- dbGetQuery(db, 'select * from stats_hitting_league where SEASON = 2025')
x_lg <- dbGetQuery(db, 'select * from stats_hitting_league_batted_ball where SEASON = 2025') 

lg_ev <- x_lg$ExitVelo
lg_avg <- h_lg$AVG
lg_slg <- h_lg$SLG
lg_xba <- x_lg$xBA
lg_xslg <- x_lg$xSLG
lg_xwoba <- x_lg$xwOBA

fg_dash <- h %>%
  left_join(x %>% select(SEASON, BatterId, xwOBA), by = c('player_id_fl' = 'BatterId', 'SEASON')) %>%
  select(Yr = SEASON, Player, Team, G, PA, HR, R, RBI, SB , 
         BB_pct, SO_pct, ISO, BABIP,
         AVG, OBP, SLG, wOBA, xwOBA, wRC_plus) %>%
  mutate(ISO = round(ISO,3),
         BABIP = round(BABIP,3) ) %>%
  rename(`BB%` = BB_pct,
         `SO%` = SO_pct,
         `wRC+` = wRC_plus)

fg_standard <- h %>%
  select(Yr = SEASON, Player, Team, G, PA, AB, AVG, H, X1B, X2B, X3B, HR, R, RBI, BB, SO, HBP, SF, SH, HDP, SB, CS)

fg_adv <- h %>%
  select(Yr = SEASON, Player, Team, BB_pct, SO_pct, BB_SO, AVG, OBP, SLG, OPS, OPS_plus,
         ISO, wOBA, wRC_plus )%>%
  rename(`BB%` = BB_pct,
         `SO%` = SO_pct,
         `BB/SO` = BB_SO,
         `OPS+` = OPS_plus,
         `wRC+` = wRC_plus)

fg_statcast <- x %>%
  left_join(h %>% select(SEASON, player_id_fl, PA, AVG, SLG, wOBA), by = c('SEASON','BatterId' = 'player_id_fl')) %>%
  select(Yr = SEASON, Player = Batter, Team = BatterTeam, PA= PA.y, AVG, xBA, SLG= SLG.y, xSLG, wOBA= wOBA.y, xwOBA,
         BBE, EV = ExitVelo, maxEV = ExitVelo_max, EV50,
         Barrels, Barrel_pct, HardHit_pct) %>%
  left_join(team %>% select(trackman_code, logo_small), by = c('Team' = 'trackman_code')) %>%
  mutate(Team = logo_small) %>%
  select(-logo_small) %>%
  arrange(desc(PA)) %>%
  rename(`Barrel%` = Barrel_pct,
         `HardHit%` = HardHit_pct)


fg_dash_tm <- h_tm %>%
  left_join(team %>% select(Team, trackman_code, logo_small), by = c('Team')) %>%
  left_join(x_tm %>% select(SEASON, BatterTeam, xwOBA), by = c('trackman_code' = 'BatterTeam', 'SEASON')) %>%
  select(Yr = SEASON, Team, G, PA, HR, R, RBI, SB , 
         BB_pct, SO_pct, ISO, BABIP,
         AVG, OBP, SLG, wOBA, xwOBA, wRC_plus) %>%
  mutate(ISO = round(ISO,3),
         BABIP = round(BABIP,3) ) %>%
  rename(`BB%` = BB_pct,
         `SO%` = SO_pct,
         `wRC+` = wRC_plus) %>%
  arrange(desc(wOBA))

fg_standard_tm <- h_tm %>%
  select(Yr = SEASON, Team, G, PA, AB, AVG, H, X1B, X2B, X3B, HR, R, RBI, BB, SO, HBP, SF, SH, HDP, SB, CS)

fg_adv_tm <- h_tm %>%
  select(Yr = SEASON, Team, BB_pct, SO_pct, BB_SO, AVG, OBP, SLG, OPS, OPS_plus,
         ISO, wOBA, wRC_plus )%>%
  rename(`BB%` = BB_pct,
         `SO%` = SO_pct,
         `BB/SO` = BB_SO,
         `OPS+` = OPS_plus,
         `wRC+` = wRC_plus)

fg_statcast_tm <- x_tm %>%
  left_join(team %>% select(Team, trackman_code, logo_small), by = c('BatterTeam' = 'trackman_code')) %>%
  left_join(h_tm %>% select(SEASON, Team, PA, AVG, SLG, wOBA), by = c('SEASON','Team')) %>%
  select(Yr = SEASON, Team = BatterTeam, PA= PA.y, AVG, xBA, SLG= SLG.y, xSLG, wOBA= wOBA.y, xwOBA,
         BBE, EV = ExitVelo, maxEV = ExitVelo_max, EV50,
         Barrels, Barrel_pct, HardHit_pct, logo_small) %>%
  mutate(Team = logo_small) %>%
  select(-logo_small) %>%
  arrange(desc(PA)) %>%
  rename(`Barrel%` = Barrel_pct,
         `HardHit%` = HardHit_pct)

fg_dash_lg <- dbGetQuery(db, "select h.SEASON Yr, h.team, t.logo_small Tm, h.wOBA, x.xwOBA, h.AVG, x.xBA, h.OBP, h.SLG, x.xSLG, h.BB_pct, h.SO_pct , x.Whiff_pct, x.Chase_pct
from stats_hitting_league h
left join teams t on t.Team = h.Team
left join stats_hitting_league_batted_ball x 
on
    x.SEASON = h.SEASON where h.SEASON = 2024 order by h.SEASON desc;")%>%
  mutate(across(c(AVG, xBA, OBP, SLG, xSLG, wOBA, xwOBA), ~ round(.,3)),
         across(c(BB_pct, SO_pct, Chase_pct, Whiff_pct), ~ round(.,1)),
         Team = Tm  )%>%
  select(-Tm) %>%  
  rename(`BB%` = BB_pct,
         `SO%` = SO_pct,
         `Whiff%` = Whiff_pct,
         `Chase%` = Chase_pct) 


# UI ----------------------------------------------------
ui <- dashboardPage(
  skin = "yellow",   title = "Boomers Analytics",
  
  dashboardHeader(title = tags$a(tags$img(src = "boomers_analytics.png", width = '60%', align = "center")),
                  tags$li(class = "dropdown",
                          tags$style(".main-header {max-height: 60px}"),
                          tags$style(".main-header .logo {height: 60px}")
                  )
  ),
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 50px}, .main-sidebar {padding-left: 25px}, .main-sidebar {padding-right: 25px}"),
    # selectInput(inputId = "season", label = "Select Season", choices =(c(2025)), selected = 2025) ,
    # verbatimTextOutput("selected_dates"),
    sidebarMenu(
      br(),br(),br(),br(),br(),
      menuItem("League", tabName = 'league', icon = icon("house", lib = "font-awesome")),
      menuItem("Stats", tabName = 'stats', icon = icon("chart-column", lib = "font-awesome"),
               menuSubItem("Hitting", "hitting_stats", icon = icon("baseball-bat-ball", lib = 'font-awesome')),
               menuSubItem("Pitching", "pitching_stats", icon = icon("baseball", lib = 'font-awesome')),
               menuSubItem("Catching", "catching_stats", icon = icon("mitten", lib = 'font-awesome'))
      ),
      menuItem("Pitcher Reports", tabName = 'pitch_tab', icon = icon("baseball", lib = "font-awesome")),
      menuItem("Hitter Reports", tabName = "hit_tab", icon = icon("baseball-bat-ball", lib = "font-awesome")   ),
      
      br(),
      
      
      div('For stats, use the season filter.', align = 'center'),
      div('For metrics use the date filter.', align = 'center'),
      
      br(),
      br(),
      br(),
      
      div( 'Made by Tony Medina',
           br(),
           tags$a(href="https://linktr.ee/tonybaseball",
                  tags$img(src="tonybaseball.png",height=50,width=50) ),
           # br(),
           tags$a(
             href="https://twitter.com/tony_baseball_", 
             tags$img(src="twitter.png",  width="50",  height="50" ) ), 
           tags$a(
             href="https://www.linkedin.com/in/tony-medina-30/", 
             tags$img(src="linkedin.png",  width="50",  height="50" )   ), align = 'center'
      )
    )
    
    
    
    
  ),
  dashboardBody( 
    tabItems(
      ### LEAGUE ----
      tabItem("league",
              wellPanel(align = 'center',
                        column(12,
                               h3(strong("Midwest Conference")),
                               column(6,align = 'center',
                                      h4(strong("West Division")),
                                      reactableOutput("midw_west_r") ),
                               column(6, align = 'center',
                                      h4(strong("Central Division")),
                                      reactableOutput("midw_cent_r"))),
                        br(),
                        column(12,
                               h3(strong("Atlantic Conference")),
                               column(6,align = 'center',
                                      h4(strong("North Division")),
                                      reactableOutput("atl_north_r") ),
                               column(6, align = 'center',
                                      h4(strong("East Division")),
                                      reactableOutput("atl_east_r")))
              )
      ),
      # HITTING STATS ----
      
      tabItem("hitting_stats",
              h2(strong("Hitting Stats")),
              column(3, 
                     sliderInput("szn_hit_stat", "Seasons", value = c(2025, 2025), min = 2021, max = 2025, sep = ""),
              ),
              br(), br(),br(), br(),br(), br(),
              tabsetPanel(
                tabPanel("Team Stats",
                         tabsetPanel(
                           tabPanel("Dashboard",
                                    wellPanel(
                                      reactableOutput("fg_dash_team_r")
                                    )),
                           tabPanel("Standard"),
                           tabPanel("Advanced"),
                           tabPanel("Batted Ball",
                                    wellPanel(
                                      reactableOutput("fg_statcast_r_team")
                                    )
                           ),
                           tabPanel("Value"),
                           tabPanel("Standard")
                         ),
                         
                         
                ),
                tabPanel("Player Stats",
                         tabsetPanel(
                           tabPanel("Dashboard",
                                    wellPanel(
                                      reactableOutput("fg_dash_r")
                                    )),
                           tabPanel("Standard"),
                           tabPanel("Advanced"),
                           tabPanel("Batted Ball",
                                    wellPanel(
                                      reactableOutput("fg_statcast_r")
                                    )
                           ),
                           tabPanel("Value"),
                           tabPanel("Career")
                         ),
                ),
                tabPanel("League Stats",
                         tabsetPanel(
                           tabPanel("Dashboard",
                                    wellPanel(
                                      reactableOutput("fg_dash_lg_r")
                                    )),
                           tabPanel("Standard"),
                           tabPanel("Advanced"),
                           tabPanel("Batted Ball",
                                    wellPanel(
                                      # reactableOutput("fg_statcast_r_team")
                                    )
                           ),
                         ),
                         
                         
                ),
              ),
              
              
              
              
              
              
      ),
      
      tabItem("pitching_stats",
              h2(strong("Pitching Stats")),
              column(3, 
                     sliderInput("szn_pitch_stat", "Seasons", value = c(2025, 2025), min = 2021, max = 2025, sep = ""),
              ),
              br(), br(),br(), br(),br(), br(),
              tabsetPanel(
                tabPanel("Team Stats",
                         tabsetPanel(
                           tabPanel("Standard"),
                           tabPanel("Advanced"),
                           tabPanel("Batted Ball",
                                    wellPanel(
                                      # reactableOutput("fg_statcast_r")
                                    )
                           ),
                           tabPanel("Value"),
                           tabPanel("Standard")
                         ),
                         
                         
                ),
                tabPanel("Player Stats",
                         tabsetPanel(
                           tabPanel("Standard"),
                           tabPanel("Advanced"),
                           tabPanel("Batted Ball",
                                    wellPanel(
                                      # reactableOutput("fg_statcast_r")
                                    )
                           ),
                           tabPanel("Value"),
                           tabPanel("Standard")
                         ),
                )
              ),
              
              
              
              
              
              
              
              
      ),
      
      tabItem("catching_stats",
              h2(strong("Catching Stats")),
              column(3, 
                     sliderInput("szn_catch_stat", "Seasons", value = c(2025, 2025), min = 2021, max = 2025, sep = ""),
              ),
              br(), br(),br(), br(),br(), br(),
              tabsetPanel(
                tabPanel("Team Stats",
                         tabsetPanel(
                           tabPanel("Framing"),
                           tabPanel("Throwing"),
                         ),
                ),
                tabPanel("Player Stats",
                         tabsetPanel(
                           tabPanel("Framing"),
                           tabPanel("Throwing"),
                         ),
                )
              ),
              
              
              
              
              
              
              
              
      ),
      # PITCHERS ----
      
      tabItem(tabName = "pitch_tab",
              
              fluidRow(column(3, 
                              selectInput(inputId = 'pitchers', 
                                          label = 'Select a Pitcher',
                                          choices = NULL, 
                                          selected = 'Cole Cook'
                              ),
                              align = 'center'),
                       column(2,actionButton("pitch_avg", "Pitch Averages"),actionButton("show_modal", "Color Scales")),
                       column(3, 
                              sliderInput("szn", "Seasons", value = c(2025, 2025), min = 2022, max = 2025, sep = ""),
                       ),
                       column(2,
                              selectInput(inputId = 'dates1', label = 'Start Date in Season range', selected = '2025-01-01', choices = '2025-01-01')                         ),
                       column(2,
                              selectInput(inputId = 'dates2', label = 'End Date in Season range', selected = '2025-12-31', choices =  '2025-12-31'),
                       )
              ),
              fluidRow(column(2,actionButton("update_pitcher", "Update Data"))),
              wellPanel(
                fluidRow(
                  
                  column(4, 
                         wellPanel(style = "border: 1px solid #73b3be;",
                                   h5(htmlOutput('team_pic'), align = 'center'),
                                   h1(strong(textOutput('selected_pitcher_szn')),align = 'center'),
                                   # h3(strong(textOutput('selected_team_p_reports')),align = 'center'), # h1(strong())
                                   h5(textOutput('player_info_p'),align = 'center'),
                                   h3(strong("Frontier League Stats"), align = 'center'),
                                   # dataTableOutput('yakcast_summary_p')
                                   reactableOutput('yakcast_summary_p')
                                   
                         )
                  ),
                  
                  column(4, 
                         wellPanel(style = "border: 1px solid #73b3be;",
                                   h4(strong("2025 FL Percentile Rankings")),
                                   # plotlyOutput('pitcher_percentile'),
                                   align = "center" )
                  ),
                  column(4, 
                         wellPanel(style = "border: 1px solid #73b3be;",
                                   h4(strong("2025 Pitch Movement")),
                                   plotlyOutput('pitch_movement_plot'),
                                   align = "center" )
                  ),
                  
                  
                  
                )
              ),br(),
              
              wellPanel(
                h4(strong("Frontier League Stats"), textOutput("selected_szn")),
                # tableOutput('test_tbl'),
                reactableOutput(outputId = 'pitcher_fl_stats'),
                # h5(strong("Color Scales: ")),
                # reactableOutput('color_def'),
                h4(strong("Pitch Metrics: "), textOutput("selected_dates"), textOutput('pitcher_games')),
                reactableOutput('pitch_metrics'),
                h4(strong("Batted Ball Stats"), textOutput("selected_szn2")),
                reactableOutput('savant_batting_p'),
                fluidRow(
                  column(4,
                         h4(strong("Pitch Movement"), align = 'center'),
                         # plotlyOutput("pitch_movement_plot")
                  ),
                  column(3, 
                         h4(strong("Pitch Speed Distribution")),
                         plotlyOutput('pitch_dist'), align = "center"
                  ),
                  column(5, 
                         h4(strong("Pitch Speed by Game")),
                         plotlyOutput('pitch_velo_game'), align = "center"
                  )
                ),
                fluidRow(
                  column(4,
                         h4(strong("Observed Spin Axis"), align = 'center'),
                         plotOutput("spinaxis_observed")
                  ) ,
                  column(4,
                         h4(strong("Spin-Based Axis"), align = 'center'),
                         plotOutput("spinaxis_inferred")
                  ) ,
                  column(4,
                         h4(strong("Pitcher Arm Angle")),
                         plotOutput('arm_angle'), align = "center"
                  )
                )
                
              )
      ),
      tabItem(tabName = "hit_tab",
              
              fluidRow(
                column(6,
                       sliderInput("season_input", "Season:",
                                   min = min(stevendata$SEASON),
                                   max = max(stevendata$SEASON),
                                   value = min(stevendata$SEASON),
                                   sep = "", step = 1)
                ),
                column(6,
                       selectInput("batter_input", "Select Batter:",
                                   choices = sort(unique(stevendata$Batter)),
                                   selected = unique(stevendata$Batter)[1])
                )
              ),
          
              
              fluidRow(
                column(12,
                       wellPanel(style = "border: 1px solid #73b3be;",
                                 h5(strong("Frontier League Hitting Stats"), align = 'center'),
                                 reactableOutput("summary_table2")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       wellPanel(style = "border: 1px solid #73b3be;",
                                 h5(strong("Rolling xwOBA Plot"), align = 'center'),
                                 plotOutput("xwoba_plot", height = "400px")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       wellPanel(style = "border: 1px solid #73b3be;",
                                 h5(strong("Swing Density (Ahead in Count)"), align = 'center'),
                                 plotOutput("aheadswing", height = "400px")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       wellPanel(style = "border: 1px solid #73b3be;",
                                 h5(strong("Swing Density (Even in Count)"), align = 'center'),
                                 plotOutput("evenswing", height = "400px")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       wellPanel(style = "border: 1px solid #73b3be;",
                                 h5(strong("Swing Density (Behind in Count)"), align = 'center'),
                                 plotOutput("behindswing", height = "400px")
                       )
                )
              )
      
      
      
    )
    )
  )
)

# Define server logic required to draw a histogram
# SERVER ------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # STANDINGS ----
  
  output$midw_west_r <- renderReactable({
    reactable(midw_west %>% 
                select (-c(SEASON, Division, Conference)),
              defaultColDef = colDef(width = 70, 
                                     style = list(display = "flex",
                                                  alignItems = "center",
                                                  height = "100%")  ,
                                     headerStyle = list(background = "#12294b", color = "white")),
              columns = list(
                Team = colDef(width = 200)
              ),
              rowStyle = function(index) {
                if (grepl("Schaumburg",midw_west$Team[index])) {
                  list(fontWeight = "bold", fontStyle = "italic",  background = "#f0f1ff99" )
                } else {
                  NULL
                }
              },
              theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif",  fontSize = "14px")))
    
  })
  
  output$midw_cent_r <- renderReactable({
    reactable(midw_cent %>% 
                select (-c(SEASON, Division, Conference)),
              defaultColDef = colDef(width = 70, 
                                     style = list(display = "flex",
                                                  alignItems = "center",
                                                  height = "100%")  ,
                                     headerStyle = list(background = "#12294b", color = "white")),
              columns = list(
                Team = colDef(width = 200)
              ),
              theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif",  fontSize = "14px")))
    
  })
  
  output$atl_north_r <- renderReactable({
    reactable(atl_north %>% 
                select (-c(SEASON, Division, Conference)),
              defaultColDef = colDef(width = 70, 
                                     style = list(display = "flex",
                                                  alignItems = "center",
                                                  height = "100%")  ,
                                     headerStyle = list(background = "#12294b", color = "white")),
              columns = list(
                Team = colDef(width = 200)
              ),
              theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif",  fontSize = "14px")))
    
  })
  
  output$atl_east_r <- renderReactable({
    reactable(atl_east %>% 
                select (-c(SEASON, Division, Conference)),
              defaultColDef = colDef(width = 70, 
                                     style = list(display = "flex",
                                                  alignItems = "center",
                                                  height = "100%")  ,
                                     headerStyle = list(background = "#12294b", color = "white")),
              columns = list(
                Team = colDef(width = 200)
              ),
              theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif",  fontSize = "14px")))
    
  })
  # HITTING STATS ----------------------------------------------------------------
  ### LG ---------------------------
  output$fg_dash_lg_r <- renderReactable({
    
    fg_dash_yr <- fg_dash_lg %>%
      filter(between(Yr, input$szn_hit_stat[1], input$szn_hit_stat[2]))
    
    reactable(fg_dash_yr,
              filterable = T, defaultPageSize = 50, sortable = T, highlight = T,
              defaultColDef = colDef(width = 70, 
                                     style = list(display = "flex",
                                                  alignItems = "center",
                                                  height = "100%")  ,
                                     headerStyle = list(background = "#12294b", color = "white")),
              columns = list(
                Yr = colDef(sticky = 'left', width = 60),
                # Player = colDef(sticky = 'left', width = 120),
                Team = colDef(sticky = 'left',
                              width = 70,
                              cell = function(url) {
                                img(src = url, height = "50px")  # Adjust height as needed
                              })
                ),
              theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif",  fontSize = "14px"))
    )
  })
  
  ### TEAM ----
  h_team_stats <- dbGetQuery(db, "select h.SEASON Yr, t.logo_small Tm, h.wOBA, x.xwOBA, h.AVG, x.xBA, h.OBP, h.SLG, x.xSLG, h.BB_pct, h.SO_pct , x.Whiff_pct, x.Chase_pct
from stats_hitting_team h
left join teams t on t.Team = h.Team
left join stats_hitting_team_batted_ball x on t.trackman_code = x.BatterTeam and x.SEASON = h.SEASON
where h.SEASON = 2025
order by h.wOBA desc") %>%
    mutate(across(c(3:9),~round(.,3)),
           across(10:13, ~round(.,1))) %>%
    rename(`SO%` = SO_pct,
           `BB%` = BB_pct,
           `Chase%` = Chase_pct,
           `Whiff%` = Whiff_pct)
  
  output$fg_dash_team_r <- renderReactable({
    
    fg_dash_yr <- fg_dash_tm %>%
      mutate(Team = teams$logo_small[match(Team, teams$Team)]) %>%
      filter(between(Yr, input$szn_hit_stat[1], input$szn_hit_stat[2])) %>%
      arrange(desc(wOBA))
    
    reactable(fg_dash_yr,
              filterable = T, defaultPageSize = 50, sortable = T, highlight = T,
              defaultColDef = colDef(width = 70, 
                                     style = list(display = "flex",
                                                  alignItems = "center",
                                                  height = "100%")  ,
                                     headerStyle = list(background = "#12294b", color = "white")),
              columns = list(
                Yr = colDef(sticky = 'left', width = 60),
                # Player = colDef(sticky = 'left', width = 120),
                Team = colDef(sticky = 'left',
                              width = 70,
                              cell = function(url) {
                                img(src = url, height = "50px")  # Adjust height as needed
                              }),
                AVG = colDef(width = 70,
                             style = function(value) {
                               if (is.na(value)) return(NULL)
                               
                               league_avg <- lg_avg
                               low_end <- 0.100
                               high_end <- 0.450
                               
                               if (value < league_avg) {
                                 # Blue to white for below-average values
                                 palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                 scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                               } else {
                                 # White to red for above-average values
                                 palette <- colorRampPalette(c("white", savant_red))(100)
                                 scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                               }
                               
                               idx <- round(scaled_value)
                               idx <- max(1, min(100, idx))  # Clamp to palette range
                               
                               bg_color <- palette[idx]
                               
                               # Convert hex to RGB
                               rgb <- grDevices::col2rgb(bg_color)
                               luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                               
                               text_color <- if (luminance < 0.5) "white" else "black"
                               
                               list(
                                 color = text_color,
                                 background = palette[idx],
                                 borderLeft = "1px solid black",
                                 display = "flex",
                                 alignItems = "center",
                                 height = "100%"
                               )
                             }
                ),
                SLG = colDef(width = 70,
                             style = function(value) {
                               if (is.na(value)) return(NULL)
                               
                               league_avg <- lg_slg
                               low_end <- .2
                               high_end <- 0.650
                               
                               if (value < league_avg) {
                                 # Blue to white for below-average values
                                 palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                 scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                               } else {
                                 # White to red for above-average values
                                 palette <- colorRampPalette(c("white", savant_red))(100)
                                 scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                               }
                               
                               idx <- round(scaled_value)
                               idx <- max(1, min(100, idx))  # Clamp to palette range
                               
                               bg_color <- palette[idx]
                               
                               # Convert hex to RGB
                               rgb <- grDevices::col2rgb(bg_color)
                               luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                               
                               text_color <- if (luminance < 0.5) "white" else "black"
                               
                               list(
                                 color = text_color,
                                 background = palette[idx],
                                 borderLeft = "1px solid black",
                                 display = "flex",
                                 alignItems = "center",
                                 height = "100%"
                               )
                             }
                ),
                
                wOBA = colDef(width = 70,
                              style = function(value) {
                                if (is.na(value)) return(NULL)
                                
                                league_avg <- lg_xwoba
                                low_end <- .2
                                high_end <- 0.650
                                
                                if (value < league_avg) {
                                  # Blue to white for below-average values
                                  palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                  scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                                } else {
                                  # White to red for above-average values
                                  palette <- colorRampPalette(c("white", savant_red))(100)
                                  scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                                }
                                
                                idx <- round(scaled_value)
                                idx <- max(1, min(100, idx))  # Clamp to palette range
                                
                                bg_color <- palette[idx]
                                
                                # Convert hex to RGB
                                rgb <- grDevices::col2rgb(bg_color)
                                luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                                
                                text_color <- if (luminance < 0.5) "white" else "black"
                                
                                list(
                                  color = text_color,
                                  background = palette[idx],
                                  borderLeft = "1px solid black",
                                  display = "flex",
                                  alignItems = "center",
                                  height = "100%"
                                )
                              }
                ),
                xwOBA = colDef(width = 70,
                               style = function(value) {
                                 if (is.na(value)) return(NULL)
                                 
                                 league_avg <- lg_xwoba
                                 low_end <- 0.200
                                 high_end <- 0.650
                                 
                                 if (value < league_avg) {
                                   # Blue to white for below-average values
                                   palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                   scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                                 } else {
                                   # White to red for above-average values
                                   palette <- colorRampPalette(c("white", savant_red))(100)
                                   scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                                 }
                                 
                                 idx <- round(scaled_value)
                                 idx <- max(1, min(100, idx))  # Clamp to palette range
                                 
                                 bg_color <- palette[idx]
                                 
                                 # Convert hex to RGB
                                 rgb <- grDevices::col2rgb(bg_color)
                                 luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                                 
                                 text_color <- if (luminance < 0.5) "white" else "black"
                                 
                                 list(
                                   color = text_color,
                                   background = palette[idx],
                                   borderRight = "1px solid black",
                                   display = "flex",
                                   alignItems = "center",
                                   height = "100%"
                                 )
                               }
                )#,
                # EV = colDef(width = 70,
                #             style = function(value) {
                #               if (is.na(value)) return(NULL)
                #               
                #               league_avg <- lg_ev
                #               low_end <- 75
                #               high_end <- 95
                #               
                #               if (value < league_avg) {
                #                 # Blue to white for below-average values
                #                 palette <- colorRampPalette(c(savant_blue, "white"))(100)
                #                 scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                #               } else {
                #                 # White to red for above-average values
                #                 palette <- colorRampPalette(c("white", savant_red))(100)
                #                 scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                #               }
                #               
                #               idx <- round(scaled_value)
                #               idx <- max(1, min(100, idx))  # Clamp to palette range
                #               
                #               bg_color <- palette[idx]
                #               
                #               # Convert hex to RGB
                #               rgb <- grDevices::col2rgb(bg_color)
                #               luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                #               
                #               text_color <- if (luminance < 0.5) "white" else "black"
                #               
                #               list(
                #                 color = text_color,
                #                 background = palette[idx],
                #                 borderRight = "1px solid black",
                #                 display = "flex",
                #                 alignItems = "center",
                #                 height = "100%"
                #               )
                #             }
                # )
              ),
              theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif",  fontSize = "14px"))
    )
  })
  
  output$fg_statcast_r_team <- renderReactable({
    # h_team_stats_yr <- 
    reactable(
      h_team_stats %>%
        filter(between(Yr, input$szn_hit_stat[1], input$szn_hit_stat[2])),
      # filter(between(Yr, 2025, 2025)),
      pagination = FALSE,
      defaultColDef = colDef(minWidth = 10, headerStyle = list(background = "#12294b", color = "white")),
      columns = list(
        Yr = colDef(width = 70),
        Tm = colDef(
          width = 70,
          cell = function(url) {
            img(src = url, height = "50px")  # Adjust height as needed
          },
          sortable = FALSE
        ),
        wOBA = colDef(
          width = 70,
          style = function(value) {
            color <- colorRampPalette(c(savant_blue, "white", savant_red))(100)
            idx <- round(scales::rescale(value, to = c(1, 100), from = c(0,  0.35870006770481*2))) # 4.7847526315802 lg Adjusted range for WHIP
            list(background = color[idx]) } ),
        xwOBA = colDef(
          width = 70,
          style = function(value) {
            color <- colorRampPalette(c(savant_blue, "white", savant_red))(100)
            idx <- round(scales::rescale(value, to = c(1, 100), from = c(0,  0.35633401886121*2))) # 4.7847526315802 lg Adjusted range for WHIP
            list(background = color[idx]) } ),
        AVG = colDef(
          width = 70,
          style = function(value) {
            color <- colorRampPalette(c(savant_blue, "white", savant_red))(100)
            idx <- round(scales::rescale(value, to = c(1, 100), from = c(0, .261*2))) # 1.51 lg Adjusted range for WHIP
            list(background = color[idx]) } ),
        xBA = colDef(
          width = 70,
          style = function(value) {
            color <- colorRampPalette(c(savant_blue, "white", savant_red))(100)
            idx <- round(scales::rescale(value, to = c(1, 100), from = c(0,  0.25710600553141*2))) # 4.7847526315802 lg Adjusted range for WHIP
            list(background = color[idx]) } ),
        OBP = colDef(
          width = 70,
          style = function(value) {
            color <- colorRampPalette(c(savant_blue, "white", savant_red))(100)
            idx <- round(scales::rescale(value, to = c(1, 100), from = c(0,  0.35870006770481*2))) # 4.7847526315802 lg Adjusted range for WHIP
            list(background = color[idx]) } ), 
        SLG = colDef(
          width = 70,
          style = function(value) {
            color <- colorRampPalette(c(savant_blue, "white", savant_red))(100)
            idx <- round(scales::rescale(value, to = c(1, 100), from = c(0,  0.38859607091519*2))) # 4.7847526315802 lg Adjusted range for WHIP
            list(background = color[idx]) } ),
        xSLG = colDef(
          width = 70,
          style = function(value) {
            color <- colorRampPalette(c(savant_blue, "white", savant_red))(100)
            idx <- round(scales::rescale(value, to = c(1, 100), from = c(0,  0.39258127024891*2))) # 4.7847526315802 lg Adjusted range for WHIP
            list(background = color[idx]) } ),
        `SO%` = colDef(
          width = 70,
          style = function(value) {
            color <- colorRampPalette(c(savant_red, "white", savant_blue))(100)
            idx <- round(scales::rescale(value, to = c(1, 100), from = c(0, 21.2*2))) # 1.51 lg Adjusted range for WHIP
            list(background = color[idx]) } ),
        `BB%` = colDef(
          width = 70,
          style = function(value) {
            color <- colorRampPalette(c(savant_blue, "white", savant_red))(100)
            idx <- round(scales::rescale(value, to = c(1, 100), from = c(0, 11.2*2))) # 1.51 lg Adjusted range for WHIP
            list(background = color[idx]) } ),
        `Whiff%` = colDef(
          width = 80,
          style = function(value) {
            color <- colorRampPalette(c(savant_red, "white", savant_blue))(100)
            idx <- round(scales::rescale(value, to = c(1, 100), from = c(0,  24.74*2))) # 4.7847526315802 lg Adjusted range for WHIP
            list(background = color[idx]) } ),
        `Chase%` = colDef(
          width = 80,
          style = function(value) {
            color <- colorRampPalette(c(savant_red, "white", savant_blue))(100)
            idx <- round(scales::rescale(value, to = c(1, 100), from = c(0,  25.1*2))) # 4.7847526315802 lg Adjusted range for WHIP
            list(background = color[idx]) } )
      ),
      rowStyle = function(index) {
        if (grepl("Schaumburg",h_team_stats$Tm[index])) {
          list(fontWeight = "bold", fontStyle = "italic",  background = "#f0f1ff99" )
        } else {
          NULL
        }
      },
      theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif",  fontSize = "14px"))
    )
  })
  
  ### PLAYER -----
  output$fg_dash_r <- renderReactable({
    
    fg_dash_yr <- fg_dash %>%
      mutate(Team = teams$logo_small[match(Team, teams$Team)]) %>%
      filter(between(Yr, input$szn_hit_stat[1], input$szn_hit_stat[2])) %>%
      arrange(desc(PA))
    
    reactable(fg_dash_yr,
              filterable = T, defaultPageSize = 50, sortable = T, highlight = T,
              defaultColDef = colDef(width = 70, 
                                     style = list(display = "flex",
                                                  alignItems = "center",
                                                  height = "100%")  ,
                                     headerStyle = list(background = "#12294b", color = "white")),
              columns = list(
                Yr = colDef(sticky = 'left', width = 60),
                Player = colDef(sticky = 'left', width = 120),
                Team = colDef(
                  width = 70,
                  cell = function(url) {
                    img(src = url, height = "50px")  # Adjust height as needed
                  }),
                AVG = colDef(width = 70,
                             style = function(value) {
                               if (is.na(value)) return(NULL)
                               
                               league_avg <- lg_avg
                               low_end <- 0.100
                               high_end <- 0.450
                               
                               if (value < league_avg) {
                                 # Blue to white for below-average values
                                 palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                 scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                               } else {
                                 # White to red for above-average values
                                 palette <- colorRampPalette(c("white", savant_red))(100)
                                 scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                               }
                               
                               idx <- round(scaled_value)
                               idx <- max(1, min(100, idx))  # Clamp to palette range
                               
                               bg_color <- palette[idx]
                               
                               # Convert hex to RGB
                               rgb <- grDevices::col2rgb(bg_color)
                               luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                               
                               text_color <- if (luminance < 0.5) "white" else "black"
                               
                               list(
                                 color = text_color,
                                 background = palette[idx],
                                 borderLeft = "1px solid black",
                                 display = "flex",
                                 alignItems = "center",
                                 height = "100%"
                               )
                             }
                ),
                SLG = colDef(width = 70,
                             style = function(value) {
                               if (is.na(value)) return(NULL)
                               
                               league_avg <- lg_slg
                               low_end <- .2
                               high_end <- 0.650
                               
                               if (value < league_avg) {
                                 # Blue to white for below-average values
                                 palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                 scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                               } else {
                                 # White to red for above-average values
                                 palette <- colorRampPalette(c("white", savant_red))(100)
                                 scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                               }
                               
                               idx <- round(scaled_value)
                               idx <- max(1, min(100, idx))  # Clamp to palette range
                               
                               bg_color <- palette[idx]
                               
                               # Convert hex to RGB
                               rgb <- grDevices::col2rgb(bg_color)
                               luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                               
                               text_color <- if (luminance < 0.5) "white" else "black"
                               
                               list(
                                 color = text_color,
                                 background = palette[idx],
                                 borderLeft = "1px solid black",
                                 display = "flex",
                                 alignItems = "center",
                                 height = "100%"
                               )
                             }
                ),
                
                wOBA = colDef(width = 70,
                              style = function(value) {
                                if (is.na(value)) return(NULL)
                                
                                league_avg <- lg_xwoba
                                low_end <- .2
                                high_end <- 0.650
                                
                                if (value < league_avg) {
                                  # Blue to white for below-average values
                                  palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                  scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                                } else {
                                  # White to red for above-average values
                                  palette <- colorRampPalette(c("white", savant_red))(100)
                                  scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                                }
                                
                                idx <- round(scaled_value)
                                idx <- max(1, min(100, idx))  # Clamp to palette range
                                
                                bg_color <- palette[idx]
                                
                                # Convert hex to RGB
                                rgb <- grDevices::col2rgb(bg_color)
                                luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                                
                                text_color <- if (luminance < 0.5) "white" else "black"
                                
                                list(
                                  color = text_color,
                                  background = palette[idx],
                                  borderLeft = "1px solid black",
                                  display = "flex",
                                  alignItems = "center",
                                  height = "100%"
                                )
                              }
                ),
                xwOBA = colDef(width = 70,
                               style = function(value) {
                                 if (is.na(value)) return(NULL)
                                 
                                 league_avg <- lg_xwoba
                                 low_end <- 0.200
                                 high_end <- 0.650
                                 
                                 if (value < league_avg) {
                                   # Blue to white for below-average values
                                   palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                   scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                                 } else {
                                   # White to red for above-average values
                                   palette <- colorRampPalette(c("white", savant_red))(100)
                                   scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                                 }
                                 
                                 idx <- round(scaled_value)
                                 idx <- max(1, min(100, idx))  # Clamp to palette range
                                 
                                 bg_color <- palette[idx]
                                 
                                 # Convert hex to RGB
                                 rgb <- grDevices::col2rgb(bg_color)
                                 luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                                 
                                 text_color <- if (luminance < 0.5) "white" else "black"
                                 
                                 list(
                                   color = text_color,
                                   background = palette[idx],
                                   borderRight = "1px solid black",
                                   display = "flex",
                                   alignItems = "center",
                                   height = "100%"
                                 )
                               }
                )#,
                # EV = colDef(width = 70,
                #             style = function(value) {
                #               if (is.na(value)) return(NULL)
                #               
                #               league_avg <- lg_ev
                #               low_end <- 75
                #               high_end <- 95
                #               
                #               if (value < league_avg) {
                #                 # Blue to white for below-average values
                #                 palette <- colorRampPalette(c(savant_blue, "white"))(100)
                #                 scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                #               } else {
                #                 # White to red for above-average values
                #                 palette <- colorRampPalette(c("white", savant_red))(100)
                #                 scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                #               }
                #               
                #               idx <- round(scaled_value)
                #               idx <- max(1, min(100, idx))  # Clamp to palette range
                #               
                #               bg_color <- palette[idx]
                #               
                #               # Convert hex to RGB
                #               rgb <- grDevices::col2rgb(bg_color)
                #               luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                #               
                #               text_color <- if (luminance < 0.5) "white" else "black"
                #               
                #               list(
                #                 color = text_color,
                #                 background = palette[idx],
                #                 borderRight = "1px solid black",
                #                 display = "flex",
                #                 alignItems = "center",
                #                 height = "100%"
                #               )
                #             }
                # )
              ),
              theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif",  fontSize = "14px"))
    )
  })
  
  output$fg_statcast_r <- renderReactable({
    
    fg_statcast_yr <- fg_statcast %>%
      filter(between(Yr, input$szn_hit_stat[1], input$szn_hit_stat[2]))
    
    reactable(fg_statcast_yr,
              filterable = T, defaultPageSize = 50, sortable = T, highlight = T,
              defaultColDef = colDef(width = 70, 
                                     style = list(display = "flex",
                                                  alignItems = "center",
                                                  height = "100%")  ,
                                     headerStyle = list(background = "#12294b", color = "white")),
              columns = list(
                Yr = colDef(sticky = 'left', width = 60),
                Player = colDef(sticky = 'left', width = 120),
                Team = colDef(
                  width = 70,
                  cell = function(url) {
                    img(src = url, height = "50px")  # Adjust height as needed
                  }),
                AVG = colDef(width = 70,
                             style = function(value) {
                               if (is.na(value)) return(NULL)
                               
                               league_avg <- lg_avg
                               low_end <- 0.100
                               high_end <- 0.450
                               
                               if (value < league_avg) {
                                 # Blue to white for below-average values
                                 palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                 scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                               } else {
                                 # White to red for above-average values
                                 palette <- colorRampPalette(c("white", savant_red))(100)
                                 scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                               }
                               
                               idx <- round(scaled_value)
                               idx <- max(1, min(100, idx))  # Clamp to palette range
                               
                               bg_color <- palette[idx]
                               
                               # Convert hex to RGB
                               rgb <- grDevices::col2rgb(bg_color)
                               luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                               
                               text_color <- if (luminance < 0.5) "white" else "black"
                               
                               list(
                                 color = text_color,
                                 background = palette[idx],
                                 borderLeft = "1px solid black",
                                 display = "flex",
                                 alignItems = "center",
                                 height = "100%"
                               )
                             }
                ),
                xBA = colDef(width = 70,
                             style = function(value) {
                               if (is.na(value)) return(NULL)
                               
                               league_avg <- lg_xba
                               low_end <- 0.100
                               high_end <- 0.450
                               
                               if (value < league_avg) {
                                 # Blue to white for below-average values
                                 palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                 scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                               } else {
                                 # White to red for above-average values
                                 palette <- colorRampPalette(c("white", savant_red))(100)
                                 scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                               }
                               
                               idx <- round(scaled_value)
                               idx <- max(1, min(100, idx))  # Clamp to palette range
                               
                               bg_color <- palette[idx]
                               
                               # Convert hex to RGB
                               rgb <- grDevices::col2rgb(bg_color)
                               luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                               
                               text_color <- if (luminance < 0.5) "white" else "black"
                               
                               list(
                                 color = text_color,
                                 background = palette[idx],
                                 borderRight = "1px solid black",
                                 display = "flex",
                                 alignItems = "center",
                                 height = "100%"
                               )
                             }
                ),
                SLG = colDef(width = 70,
                             style = function(value) {
                               if (is.na(value)) return(NULL)
                               
                               league_avg <- lg_slg
                               low_end <- .2
                               high_end <- 0.650
                               
                               if (value < league_avg) {
                                 # Blue to white for below-average values
                                 palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                 scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                               } else {
                                 # White to red for above-average values
                                 palette <- colorRampPalette(c("white", savant_red))(100)
                                 scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                               }
                               
                               idx <- round(scaled_value)
                               idx <- max(1, min(100, idx))  # Clamp to palette range
                               
                               bg_color <- palette[idx]
                               
                               # Convert hex to RGB
                               rgb <- grDevices::col2rgb(bg_color)
                               luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                               
                               text_color <- if (luminance < 0.5) "white" else "black"
                               
                               list(
                                 color = text_color,
                                 background = palette[idx],
                                 borderLeft = "1px solid black",
                                 display = "flex",
                                 alignItems = "center",
                                 height = "100%"
                               )
                             }
                ),
                xSLG = colDef(width = 70,
                              style = function(value) {
                                if (is.na(value)) return(NULL)
                                
                                league_avg <- lg_xslg
                                low_end <- 0.200
                                high_end <- 0.650
                                
                                if (value < league_avg) {
                                  # Blue to white for below-average values
                                  palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                  scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                                } else {
                                  # White to red for above-average values
                                  palette <- colorRampPalette(c("white", savant_red))(100)
                                  scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                                }
                                
                                idx <- round(scaled_value)
                                idx <- max(1, min(100, idx))  # Clamp to palette range
                                
                                bg_color <- palette[idx]
                                
                                # Convert hex to RGB
                                rgb <- grDevices::col2rgb(bg_color)
                                luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                                
                                text_color <- if (luminance < 0.5) "white" else "black"
                                
                                list(
                                  color = text_color,
                                  background = palette[idx],
                                  borderRight = "1px solid black",
                                  display = "flex",
                                  alignItems = "center",
                                  height = "100%"
                                )
                              }
                ),
                wOBA = colDef(width = 70,
                              style = function(value) {
                                if (is.na(value)) return(NULL)
                                
                                league_avg <- lg_xwoba
                                low_end <- .2
                                high_end <- 0.650
                                
                                if (value < league_avg) {
                                  # Blue to white for below-average values
                                  palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                  scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                                } else {
                                  # White to red for above-average values
                                  palette <- colorRampPalette(c("white", savant_red))(100)
                                  scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                                }
                                
                                idx <- round(scaled_value)
                                idx <- max(1, min(100, idx))  # Clamp to palette range
                                
                                bg_color <- palette[idx]
                                
                                # Convert hex to RGB
                                rgb <- grDevices::col2rgb(bg_color)
                                luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                                
                                text_color <- if (luminance < 0.5) "white" else "black"
                                
                                list(
                                  color = text_color,
                                  background = palette[idx],
                                  borderLeft = "1px solid black",
                                  display = "flex",
                                  alignItems = "center",
                                  height = "100%"
                                )
                              }
                ),
                xwOBA = colDef(width = 70,
                               style = function(value) {
                                 if (is.na(value)) return(NULL)
                                 
                                 league_avg <- lg_xwoba
                                 low_end <- 0.200
                                 high_end <- 0.650
                                 
                                 if (value < league_avg) {
                                   # Blue to white for below-average values
                                   palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                   scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                                 } else {
                                   # White to red for above-average values
                                   palette <- colorRampPalette(c("white", savant_red))(100)
                                   scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                                 }
                                 
                                 idx <- round(scaled_value)
                                 idx <- max(1, min(100, idx))  # Clamp to palette range
                                 
                                 bg_color <- palette[idx]
                                 
                                 # Convert hex to RGB
                                 rgb <- grDevices::col2rgb(bg_color)
                                 luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                                 
                                 text_color <- if (luminance < 0.5) "white" else "black"
                                 
                                 list(
                                   color = text_color,
                                   background = palette[idx],
                                   borderRight = "1px solid black",
                                   display = "flex",
                                   alignItems = "center",
                                   height = "100%"
                                 )
                               }
                ),
                EV = colDef(width = 70,
                            style = function(value) {
                              if (is.na(value)) return(NULL)
                              
                              league_avg <- lg_ev
                              low_end <- 75
                              high_end <- 95
                              
                              if (value < league_avg) {
                                # Blue to white for below-average values
                                palette <- colorRampPalette(c(savant_blue, "white"))(100)
                                scaled_value <- scales::rescale(value, from = c(low_end, league_avg), to = c(1, 100))
                              } else {
                                # White to red for above-average values
                                palette <- colorRampPalette(c("white", savant_red))(100)
                                scaled_value <- scales::rescale(value, from = c(league_avg, high_end), to = c(1, 100))
                              }
                              
                              idx <- round(scaled_value)
                              idx <- max(1, min(100, idx))  # Clamp to palette range
                              
                              bg_color <- palette[idx]
                              
                              # Convert hex to RGB
                              rgb <- grDevices::col2rgb(bg_color)
                              luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
                              
                              text_color <- if (luminance < 0.5) "white" else "black"
                              
                              list(
                                color = text_color,
                                background = palette[idx],
                                borderRight = "1px solid black",
                                display = "flex",
                                alignItems = "center",
                                height = "100%"
                              )
                            }
                )
              ),
              theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif",  fontSize = "14px"))
    )
  })
  
  
  
  # PITCHERS ----
  output$selected_pitcher_szn <- renderText({paste(reactive_data$pitchers)})
  
  all_pitcher_names <- dbGetQuery(db, 'SELECT distinct Pitcher from pitch_data order by Pitcher ')
  observe({
    updateSelectInput(session, "pitchers",
                      choices = all_pitcher_names$Pitcher,
                      selected = "Cole Cook") # Default selection
  })
  
  observeEvent(input$pitchers, {
    req(input$pitchers) # Ensure input is available
    max_date_range <- dbGetQuery(db, glue("SELECT MIN(SEASON) as min, MAX(SEASON) as max FROM pitch_data"))
    
    query <- glue("SELECT MIN(SEASON) as min, MAX(SEASON) as max FROM pitch_data WHERE Pitcher = '{input$pitchers}'")
    
    season_range <- dbGetQuery(db, query)
    showSpinner()
    updateSliderInput(session, "szn",
                      min = 2022,
                      max = max_date_range$max,
                      value = c(season_range$max, season_range$max))
    hideSpinner()
  })
  
  output$player_info_p <- renderText({
    
    player <- dbGetQuery(db, glue::glue('select * 
                                        from rosters 
                                        where Player = "{reactive_data$pitchers}" 
                                        and SEASON in 
                                             (select max(SEASON) from rosters where Player = "{reactive_data$pitchers}")')) 
    
    b_day <- player$Birthday
    
    paste("Pitcher |", "Bats/Throws:", player$BATS[1],"/", player$THROWS[1], "|", player$Ht[1], player$Wt[1],
          "| Age:", floor(as.numeric(today()-as.Date('07/07/1997', "%m/%d/%Y")) /365  ))
  })
  
  
  output$yakcast_summary_p <-  renderReactable({
    # reactive_data <- ''
    # reactive_data$pitchers <- 'Cole Cook'
    # Check if a pitcher is selected
    req(reactive_data$pitchers)
    
    player_stats <<- dbGetQuery(db, glue::glue('
      select s.SEASON Yr, s.W, s.L, s.ERA, round(x.xERA,2) xERA, s.IP, s.SO, s.WHIP
      from stats_pitching_player s
      left join stats_pitching_player_batted_ball x
      on s.SEASON = x.SEASON and s.player_id_fl = x.PitcherId
      WHERE s.Player = "{reactive_data$pitchers}"
      order by s.SEASON')) 
    
    pitcher_career <<- dbGetQuery(db, glue::glue('
  WITH pitcher AS (
    SELECT s.Player, s.player_id_fl, 
           COUNT(DISTINCT s.SEASON) Yr, 
           SUM(s.W) W, SUM(s.L) L, SUM(s.ER) ER, 
           SUM(s.BB) BB, SUM(s.H) H, 
           SUM(CAST(s.IP AS INTEGER) * 3 + ROUND((s.IP - CAST(s.IP AS INTEGER)) * 3)) AS outs, 
           SUM(s.SO) SO
    FROM stats_pitching_player s
    WHERE s.Player = {shQuote(reactive_data$pitchers)}
  )
  SELECT Yr, W, L, 
         ROUND(ER / (outs / 3.0) * 9, 2) AS ERA, 
         ROUND(x.xERA, 2) AS xERA,
         ROUND(outs / 3.0, 2) AS IP, 
         s.SO, 
         ROUND((s.BB + s.H) / (outs / 3.0), 2) AS WHIP
  FROM pitcher s
  LEFT JOIN stats_pitching_player_batted_ball_career x 
    ON s.player_id_fl = x.PitcherId;
'))
    
    player_summary_p <<- rbind(player_stats, pitcher_career ) 
    
    tableFilter <- reactive({player_summary_p})
    
    reactable(tableFilter(),
              rowStyle = function(index) {
                if (index==nrow(tableFilter())) {
                  list(background = "lightgrey")
                }
              },
              theme = reactableTheme(
                style = list(fontFamily = "Calibri, sans-serif"),
                headerStyle = list(background = "hsl(219, 89%, 14%);",  color = "hsl(0, 0%, 100%);")
              ),
              defaultColDef = colDef(align = 'center',
                                     minWidth = 30  # Adjust the minimum width as needed
              ),
              columns = list(
                WHIP = colDef(minWidth = 35)
              )
    )
  })
  
  
  output$team_pic <- renderUI({
    req(reactive_data$pitchers)
    rosters <- dbGetQuery(db, glue::glue('SELECT Team from rosters 
                                         where SEASON in
                                                  (select max(season) from rosters where Player = "{reactive_data$pitchers}") 
                                         and Player = "{reactive_data$pitchers}"'))
    # Filter the dataframe to get the selected team's image URL
    image_url <- teams$banner[teams$Team == rosters$Team]
    
    # Return the image if the URL exists, otherwise return a default image
    if (length(image_url) > 0) {
      img(src = image_url, width = "100%")
    } else {
      img(src = "https://i.imgur.com/kFCZZkv.png", width = "100%")
    }
  })
  
  master_season_1 = 2025
  master_season_2 = 2025
  master_date_1 = '2025-01-01'
  master_date_2 = '2025-12-31'
  
  
  # Initialize reactiveValues with default values
  reactive_data <- reactiveValues(
    szn1 = 2025,  # Default initial values
    szn2 = 2025,
    pitchers = "Cole Cook",
    p_clean = gsub(' ', "%20", "Cole Cook"), # Encode spaces in the default value
    dates1 = '2025-01-01',  # Default date range
    dates2 = '2025-12-31'
  )
  
  
  observeEvent(input$pitchers, {
    req(input$pitchers)  # Ensure both inputs are available
    query <- glue("SELECT DISTINCT Date FROM pitch_data WHERE 
                Pitcher = '{input$pitchers}' 
                AND SEASON BETWEEN {input$szn[1]} AND {input$szn[2]}")
    
    season_range <- dbGetQuery(db, query)
    showSpinner()
    if (nrow(season_range) > 0) {  # Ensure there are available dates
      updateSelectInput(session, "dates1",
                        choices = season_range$Date,
                        selected = season_range$Date[1])  # Default selection
    } else {
      updateSelectInput(session, "dates1",
                        choices = NULL,
                        selected = NULL)  # Reset if no dates are found
    }
    
    if (nrow(season_range) > 0) {  # Ensure there are available dates
      updateSelectInput(session, "dates2",
                        choices = season_range$Date,
                        selected = tail(season_range$Date,1))  # Default selection
    } else {
      updateSelectInput(session, "dates2",
                        choices = NULL,
                        selected = NULL)  # Reset if no dates are found
    }
    hideSpinner()
  })
  
  
  
  observeEvent(input$szn, {
    req(input$pitchers)  # Ensure both inputs are available
    query <- glue("SELECT DISTINCT Date FROM pitch_data WHERE 
                Pitcher = '{input$pitchers}' 
                AND SEASON BETWEEN {input$szn[1]} AND {input$szn[2]}")
    
    season_range <- dbGetQuery(db, query)
    showSpinner()
    if (nrow(season_range) > 0) {  # Ensure there are available dates
      updateSelectInput(session, "dates1",
                        choices = season_range$Date,
                        selected = season_range$Date[1])  # Default selection
    } else {
      updateSelectInput(session, "dates1",
                        choices = NULL,
                        selected = NULL)  # Reset if no dates are found
    }
    
    if (nrow(season_range) > 0) {  # Ensure there are available dates
      updateSelectInput(session, "dates2",
                        choices = season_range$Date,
                        selected = tail(season_range$Date,1))  # Default selection
    } else {
      updateSelectInput(session, "dates2",
                        choices = NULL,
                        selected = NULL)  # Reset if no dates are found
    }
    hideSpinner()
  })
  
  pitcher_data <- reactive({
    #   # Construct the API URL based on the updated reactive_data
    query_ <- glue::glue(
      "SELECT * FROM pitch_data WHERE Pitcher='{reactive_data$pitchers}'  AND SEASON BETWEEN {reactive_data$szn1} AND {reactive_data$szn2} AND Date BETWEEN '{reactive_data$dates1}' AND '{reactive_data$dates2}'"
    )
    print(query_)
    #   
    #   # Fetch and parse the data
    dbGetQuery(db, query_)  
  })
  
  
  observe({
    reactive_data$pitcher_data <- dbGetQuery(db, 
                                             glue('SELECT * FROM pitch_data 
          WHERE Pitcher = "{reactive_data$pitchers}"
          AND SEASON BETWEEN {reactive_data$szn1} AND {reactive_data$szn2}
          AND Date BETWEEN "{reactive_data$dates1}" and "{reactive_data$dates2}" ')
    )
    
    reactive_data$pitcher_stats <- dbGetQuery(db, 
                                              glue('SELECT * FROM stats_pitching_player 
          WHERE Player = "{reactive_data$pitchers}"
          AND SEASON BETWEEN {reactive_data$szn1} AND {reactive_data$szn2}')
    )
    
    reactive_data$pitcher_stats <- dbGetQuery(db, 
                                              glue('SELECT * FROM year_stats_pitching_player_batted_ball 
          WHERE Pitcher = "{reactive_data$pitchers}" 
          AND SEASON BETWEEN {reactive_data$szn1} AND {reactive_data$szn2}')
    )
  })
  
  observeEvent(input$pitchers, {
    req(input$pitchers)  # Ensure both inputs are available
    query <- glue("SELECT DISTINCT Date FROM pitch_data WHERE 
                Pitcher = '{input$pitchers}' 
                AND SEASON BETWEEN {input$szn[1]} AND {input$szn[2]}")
    
    season_range <- dbGetQuery(db, query)
    showSpinner()
    if (nrow(season_range) > 0) {  # Ensure there are available dates
      updateSelectInput(session, "dates1",
                        choices = season_range$Date,
                        selected = season_range$Date[1])  # Default selection
    } else {
      updateSelectInput(session, "dates1",
                        choices = NULL,
                        selected = NULL)  # Reset if no dates are found
    }
    
    if (nrow(season_range) > 0) {  # Ensure there are available dates
      updateSelectInput(session, "dates2",
                        choices = season_range$Date,
                        selected = tail(season_range$Date,1))  # Default selection
    } else {
      updateSelectInput(session, "dates2",
                        choices = NULL,
                        selected = NULL)  # Reset if no dates are found
    }
    hideSpinner()
  })
  
  
  observeEvent(input$szn, {
    req(input$pitchers)  # Ensure both inputs are available
    query <- glue("SELECT DISTINCT Date FROM pitch_data WHERE 
                Pitcher = '{input$pitchers}' 
                AND SEASON BETWEEN {input$szn[1]} AND {input$szn[2]}")
    
    season_range <- dbGetQuery(db, query)
    showSpinner()
    if (nrow(season_range) > 0) {  # Ensure there are available dates
      updateSelectInput(session, "dates1",
                        choices = season_range$Date,
                        selected = season_range$Date[1])  # Default selection
    } else {
      updateSelectInput(session, "dates1",
                        choices = NULL,
                        selected = NULL)  # Reset if no dates are found
    }
    
    if (nrow(season_range) > 0) {  # Ensure there are available dates
      updateSelectInput(session, "dates2",
                        choices = season_range$Date,
                        selected = tail(season_range$Date,1))  # Default selection
    } else {
      updateSelectInput(session, "dates2",
                        choices = NULL,
                        selected = NULL)  # Reset if no dates are found
    }
    hideSpinner()
  })
  
  pitcher_data <- reactive({
    #   # Construct the API URL based on the updated reactive_data
    query_ <- glue::glue(
      "SELECT * FROM pitch_data WHERE Pitcher='{reactive_data$pitchers}' 
      AND SEASON BETWEEN {reactive_data$szn1} AND {reactive_data$szn2} 
      AND Date BETWEEN '{reactive_data$dates1}' AND '{reactive_data$dates2}'"
    )
    print(query_)
    #   
    #   # Fetch and parse the data
    dbGetQuery(db, query_)  
    
  })
  
  
  # observeEvent(input$update_pitcher,{
  #   
  # })
  
  all_batter_names <- dbGetQuery(db, 'SELECT distinct Batter from pitch_data order by Batter ')
  observe({
    updateSelectInput(session, "hitters",
                      choices = all_batter_names$Batter,
                      selected = "Chase Dawson") # Default selection
  })
  
  
  observeEvent(input$show_modal, {
    
    # Modal with the reactable table
    showModal(modalDialog(
      title = "Color Scales Explained",
      reactable(colors_df_ex, rownames = FALSE,
                columns = list(
                  `V1` = colDef(
                    style = function(value, index) {
                      if (colors_df_ex$`Color_code`[index] == 'better or worse') {
                        list(background = "#242766", color = "white")
                      } else {
                        list(background = "#C3B700", color = "white")
                      }
                    }
                  ),
                  `V2` = colDef(
                    style = function(value, index) {
                      if (colors_df_ex$`Color_code`[index] == 'better or worse') {
                        list(background = "#4e55d9", color = "white")
                      } else {
                        list(background = "#E7DE55", color = "white")
                      }
                    }
                  ),
                  `V3` = colDef(
                    style = function(value, index) {
                      if (colors_df_ex$`Color_code`[index] == 'better or worse') {
                        list(background = "#babeff", color = "black")
                      } else {
                        list(background = "#F8FFC1", color = "black")
                      }
                    }
                  ),
                  `V4` = colDef(
                    style = function(value, index) {
                      if (colors_df_ex$`Color_code`[index] == 'better or worse') {
                        list(background = "#ebebeb", color = "black")
                      } else {
                        list(background = "#ebebeb", color = "black")
                      }
                    }
                  ),
                  `V5` = colDef(
                    style = function(value, index) {
                      if (colors_df_ex$`Color_code`[index] == 'better or worse') {
                        list(background = "#ffbabf", color = "black")
                      } else {
                        list(background = "#E6FFE8", color = "black")
                      }
                    }
                  ),
                  `V6` = colDef(
                    style = function(value, index) {
                      if (colors_df_ex$`Color_code`[index] == 'better or worse') {
                        list(background = "#bd3e47", color = "white")
                      } else {
                        list(background = "#A2CEA6", color = "white")
                      }
                    }
                  ),
                  `V7` = colDef(
                    style = function(value, index) {
                      if (colors_df_ex$`Color_code`[index] == 'better or worse') {
                        list(background = "#780008", color = "white")
                      } else {
                        list(background = "#00840D", color = "white")
                      }
                    }
                  )
                ),
                theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif"))
      ),
      easyClose = TRUE,   # Allows clicking outside to close the modal
      footer = modalButton("Close") # Adds a close button
    ))
  })
  
  observeEvent(input$pitch_avg, {
    
    fl_avg_p <- dbGetQuery(db, "SELECT * FROM metrics_pitch_league_season_averages where SEASON = 2025") %>%
      pitch_type_factor_and_recode()%>%
      arrange(TaggedPitchType)%>%
      rename('Pitch' = TaggedPitchType)
    
    
    # Modal with the reactable table
    showModal(modalDialog(
      title = "Pitch Averages", 
      
      div(
        p("This table shows average pitch metrics from the 2025 Frontier League season, normalized to a RHP"),
        style = "margin-bottom: 20px;"
      ),
      
      reactable(fl_avg_p,
                defaultColDef = colDef(width = 85),
                columns = list(Pitch = colDef(style = function(value) get_pitch_color(value))),
                pagination = F,
                theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif"))
      ),
      easyClose = TRUE,   # Allows clicking outside to close the modal
      footer = modalButton("Close") # Adds a close button
    ))
  })
  
  
  # pitch_data_lg_avg <- dbGetQuery(db, "SELECT * FROM pitch_data where TaggedPitchType <> '' and TaggedPitchType is not null ")%>%
  #   mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
  #          TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
  #                                   Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN' , Splitter = 'SPL'  ) ) %>%
  #   group_by(PitcherThrows, TaggedPitchType) %>%
  #   dplyr::summarise(usage = n(),
  #                    InducedVertBreak = mean(InducedVertBreak,na.rm = T),
  #                    HorzBreak = mean(HorzBreak,na.rm = T),
  #                    SpinAxis = mean(SpinAxis,na.rm = T),
  #   )
  
  
  output$selected_dates <- renderText({
    if(reactive_data$dates1 == reactive_data$dates2) {
      reactive_data$dates2
    } else{
      paste(reactive_data$dates1, "to", reactive_data$dates2)
      
    }
  })
  
  output$selected_szn <- renderText({
    if(reactive_data$szn1 == reactive_data$szn2) {
      input$szn1
    } else {
      paste(reactive_data$szn1, "to", reactive_data$szn2)
    }
  })
  
  output$selected_szn2 <- renderText({
    if(reactive_data$szn1 == reactive_data$szn2) {
      reactive_data$szn1
    } else {
      paste(reactive_data$szn1, "to", reactive_data$szn2)
    }
  })
  
  pitcher_dates <- reactive({
    req(input$pitchers, input$szn)  # Ensure input values are available before querying
    # query_result <- dbGetQuery(db, 
    #                            'SELECT DISTINCT Date 
    #                FROM pitch_data 
    #                WHERE Pitcher = :player_name AND SEASON BETWEEN :szn1 AND :szn2',
    #                params = list(player_name = input$pitchers, 
    #                              szn1 = input$szn[1], 
    #                              szn2 = input$szn[2])
    # )
    # 
    
    query_result <- dbGetQuery(db, glue( 
      'SELECT DISTINCT Date 
                   FROM pitch_data 
                   WHERE Pitcher = "{input$pitchers}" AND SEASON BETWEEN {input$szn[1]} AND {input$szn[2]}' ),
      # params = list(player_name = input$pitchers, 
      #               szn1 = input$szn[1], 
      #               szn2 = input$szn[2])
    )
    
    unique(sort(query_result$Date))  # Ensure dates are unique and sorted
  })
  
  
  
  
  observeEvent(input$update_pitcher, {
    showPageSpinner()
    
    reactive_data$szn1 <- input$szn[1]
    reactive_data$szn2 <- input$szn[2]
    reactive_data$pitchers <- input$pitchers
    reactive_data$p_clean <- gsub(' ', "%20", input$pitchers) # Encode spaces
    reactive_data$dates1 <- input$dates1
    reactive_data$dates2 <- input$dates2
    
    quantiles_summary <- reactive({
      
      dbGetQuery(db, glue('SELECT Pitcher, TaggedPitchType, RelSpeed, SpinRate, InducedVertBreak,HorzBreak,
                  yt_Efficiency, VertApprAngle,RelHeight,RelSide,Extension
                  FROM pitch_data 
                  WHERE TaggedPitchType is not null and TaggedPitchType <>"" 
                  AND SEASON between {reactive_data$szn1} AND {reactive_data$szn2}')) %>%
        # tonybsbl::pitch_types_recode() %>%
        group_by(TaggedPitchType) %>%
        dplyr::summarise(
          n = n(),
          velo = list(calculate_quantiles(cur_data(), "RelSpeed")),
          spin = list(calculate_quantiles(cur_data(), "SpinRate")),
          ivb = list(calculate_quantiles(cur_data(), "InducedVertBreak")),
          hbb = list(calculate_quantiles(cur_data(), "HorzBreak")),
          spineff = list(calculate_quantiles(cur_data(), "yt_Efficiency")),
          vaa = list(calculate_quantiles(cur_data(), "VertApprAngle")),
          relh = list(calculate_quantiles(cur_data(), "RelHeight")),
          rels = list(calculate_quantiles(cur_data(), "RelSide")),
          ext = list(calculate_quantiles(cur_data(), "Extension")),
          
        ) 
    })
    
    hidePageSpinner()
  })
  
  
  
  quantiles_summary <- reactive({
    
    dbGetQuery(db, glue('SELECT Pitcher, TaggedPitchType, RelSpeed, SpinRate, InducedVertBreak,HorzBreak,
                  yt_Efficiency, VertApprAngle,RelHeight,RelSide,Extension
                  FROM pitch_data 
                  WHERE TaggedPitchType is not null and TaggedPitchType <>"" 
                  AND SEASON between {reactive_data$master_season_1} AND {reactive_data$master_season_2}')) %>%
      # tonybsbl::pitch_types_recode() %>%
      group_by(TaggedPitchType) %>%
      dplyr::summarise(
        n = n(),
        velo = list(calculate_quantiles(cur_data(), "RelSpeed")),
        spin = list(calculate_quantiles(cur_data(), "SpinRate")),
        ivb = list(calculate_quantiles(cur_data(), "InducedVertBreak")),
        hbb = list(calculate_quantiles(cur_data(), "HorzBreak")),
        spineff = list(calculate_quantiles(cur_data(), "yt_Efficiency")),
        vaa = list(calculate_quantiles(cur_data(), "VertApprAngle")),
        relh = list(calculate_quantiles(cur_data(), "RelHeight")),
        rels = list(calculate_quantiles(cur_data(), "RelSide")),
        ext = list(calculate_quantiles(cur_data(), "Extension")),
        
      ) 
  })
  
  
  output$pitcher_games <- renderText({
    
    req(input$pitchers, input$szn)  # Ensure input values are available before querying
    # 
    # query_result <- dbGetQuery(db, 
    #                            'SELECT COUNT( DISTINCT (GameID )) gm
    #                FROM pitch_data
    #                WHERE Pitcher = :player_name AND Date BETWEEN :szn1 AND :szn2',
    #                params = list(player_name = reactive_data$pitchers, 
    #                              szn1 = reactive_data$dates1, 
    #                              szn2 = reactive_data$dates2)
    # )
    
    query_result <- dbGetQuery(db, glue('SELECT COUNT( DISTINCT (GameID )) gm
                   FROM pitch_data
                   WHERE Pitcher = "{reactive_data$pitchers}" AND Date BETWEEN "{reactive_data$dates1}" AND "{reactive_data$dates2}"'))
    
    
    
    paste(query_result$gm, 'games worth of pitch tracking data')
    
  })
  
  
  
  output$pitcher_fl_stats <- renderReactable({
    
    
    fl_stats <- dbGetQuery(db, glue('select * from stats_pitching_player where Player = "{paste0(reactive_data$pitchers)}" and 
                              SEASON between {paste(reactive_data$szn1)} and {paste(reactive_data$szn2)}')  ) %>%
      arrange(desc(SEASON))
    
    team_name_for_player <- fl_stats %>%
      filter(SEASON == max(SEASON)) %>%
      pull(Team)
    
    p_team_info <- unique(dbGetQuery(db, glue('select * from teams where Team = "{team_name_for_player}"'))$main_logo)
    
    fl_stats$TEAM <- p_team_info
    
    reactable(fl_stats %>% select(-Player, -`Num`),
              defaultColDef = colDef(width = 70, align = 'center',),
              columns = list(
                TEAM = colDef(
                  align = 'center',
                  width = 90,
                  name = "TEAM",
                  cell = function(value) {
                    tagList(
                      tags$img(src = value, style = "width:100%; height:auto;")
                    )
                  }
                )
              ),
              theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif"))
    )
    
  })
  
  output$test_tbl <- renderTable({
    pitcher_data() %>%
      slice(1:5)
  })
  
  
  
  output$pitch_metrics <- renderReactable({
    
    table <-
      pitcher_data() %>%
      filter(TaggedPitchType != '' | is.na(TaggedPitchType)) %>%
      pitch_type_factor_and_recode() %>%
      mutate(SpinAxis_inferred =  atan2(HorzBreak,InducedVertBreak) * (180 / pi) + 180) %>%
      group_by('Pitch' = TaggedPitchType) %>%
      filter(TaggedPitchType != '') %>%
      dplyr::summarize('No.' = n(),
                       'Usage' = n(),
                       'Usage %' = n(),
                       'Velo' = round(mean(RelSpeed, na.rm = TRUE),1),
                       'Max Velo' = round(max(RelSpeed, na.rm = TRUE),1),
                       'Spin' = round(mean(SpinRate, na.rm = TRUE),0),
                       'Spin Eff' = round(mean(yt_Efficiency, na.rm= TRUE),0),
                       'IVB' = round(mean(InducedVertBreak, na.rm = TRUE),1),
                       'HB' = round(mean(HorzBreak, na.rm = TRUE),1),
                       'Axis Observed' = round(mean(SpinAxis, na.rm = TRUE),0),
                       'Time' = sapply(`Axis Observed`, function(x) if (is.na(x)){return(NA)}
                                       else if(x > 180 & x <= 360){(x/30)-6}
                                       else if(x == 180){12}
                                       else{(x/30)+6}),
                       'HH' = as.integer(Time),
                       'HH' = sapply(HH, function(x) if (is.na(x)){return(NA)}
                                     else if(x == 0){x+12}
                                     else if(x > 12){x-12}
                                     else{x+0}),
                       "MM" = formatC(round((Time%%1)*60, digits = 0), width = 2, flag = "0"),
                       'Tilt Observed' = paste0(HH,":", MM),
                       'Axis Spin-Based' = round(mean(SpinAxis_inferred, na.rm = T)),
                       'Time' = sapply(`Axis Spin-Based`, function(x) if (is.na(x)){return(NA)}
                                       else if(x > 180 & x <= 360){(x/30)-6}
                                       else if(x == 180){12}
                                       else{(x/30)+6}),
                       'HH' = as.integer(Time),
                       'HH' = sapply(HH, function(x) if (is.na(x)){return(NA)}
                                     else if(x == 0){x+12}
                                     else if(x > 12){x-12}
                                     else{x+0}),
                       "MM" = formatC(round((Time%%1)*60, digits = 0), width = 2, flag = "0"),
                       'Tilt Spin-Based' = paste0(HH,":", MM),
                       'VAA' = round(mean(VertApprAngle, na.rm = TRUE),1),
                       'Arm Angle' = round(mean(arm_angle_savant, na.rm = T), 1),
                       'Rel Height' = round(mean(RelHeight, na.rm = TRUE),1),
                       'Rel Side' = round(mean(RelSide, na.rm = TRUE),1),
                       'Extension' = round(mean(Extension, na.rm = TRUE),1)
      ) %>%
      filter(`No.` > 1) %>%
      mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100) %>%
      dplyr::select(-Usage,-Time,-HH,-MM)
    
    
    reactable(table,
              defaultColDef = colDef(width = 85),
              columns = c(
                list(Pitch = colDef(style = function(value) get_pitch_color(value))),
                theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif"))
                # list(Velo = colDef(style = function(value, index) get_quantile_style(value, index, "Velo", hot_cold)))#,
                # metric_cols
              )
    )
    
  })
  
  
  output$savant_batting_p <- renderReactable({
    
    # fl_stats <- dbGetQuery(db, 'select * from year_stats_pitching_player_batted_ball where Pitcher = :player_name and Season between :szn1 and :szn2', 
    #                        params = list(player_name = paste0(reactive_data$pitchers), 
    #                                      szn1 = paste(reactive_data$szn1), 
    #                                      szn2 = paste(reactive_data$szn2))
    # ) %>%
    #   arrange(desc(SEASON)) 
    
    fl_stats <- dbGetQuery(db, glue('select * from year_stats_pitching_player_batted_ball where Pitcher = "{paste0(reactive_data$pitchers)}" and 
                              SEASON between {paste(reactive_data$szn1)} and {paste(reactive_data$szn2)}')  ) %>%
      arrange(desc(SEASON))
    
    reactable(fl_stats,
              theme = reactableTheme(style = list(fontFamily = "Calibri, sans-serif")))
    
  })
  
  
  
  
  pitch_movement <- reactive({
    # dbGetQuery(db,  "SELECT * FROM pitch_data where Pitcher = :pitcher AND Date between :dates1 and :dates2 AND (TaggedPitchType is not NULL OR TaggedPitchType <>'')", 
    #            params = list(pitcher = reactive_data$pitchers, 
    #                          dates1 = paste(reactive_data$dates1), 
    #                          dates2 = paste(reactive_data$dates2))
    # ) 
    pitcher_data() %>%
      filter(TaggedPitchType != '' | is.na(TaggedPitchType)) %>% 
      pitch_type_factor_and_recode() 
  })
  
  output$pitch_movement_plot <- renderPlotly({
    
    # req(input$pitchers)  # Ensure 'pitchers' input is available
    req(nrow(pitch_movement()) > 0)
    
    roster_info <- reactive({
      
      dbGetQuery(db, 'select * from rosters_ where Player = :pitcher',
                 params = list(pitcher = paste(reactive_data$pitchers))) %>%
        filter(SEASON == max(SEASON)) %>%
        mutate(height_inches = convert_to_inches(HEIGHT)
        )
    })
    
    arm_angle <- reactive({
      # dbGetQuery(db, 'select Pitcher, PitcherTeam, PitcherThrows, arm_angle, arm_angle_savant, RelHeight, RelSide, height_inches, shoulder_pos from pitch_data
      #              where Pitcher = :pitcher and Date between :dates1 and :dates2',
      #            params = list(pitcher = paste(reactive_data$pitchers),
      #                          dates1 = paste(reactive_data$dates1),
      #                          dates2 = paste(reactive_data$dates2))
      # ) %>%
      pitcher_data() %>%
        group_by(Pitcher, PitcherThrows) %>%
        summarise(#PitcherTeam = unique(PitcherTeam),
          height_inches = mean(height_inches, na.rm = T),
          shoulder_pos = mean(shoulder_pos, na.rm = T),
          release_pos_x = median(RelSide * 12, na.rm = T),
          release_pos_z = median(RelHeight * 12, na.rm = T),
          arm_angle = mean(arm_angle, na.rm = T),
          arm_angle_savant = mean(arm_angle_savant, na.rm = T)
        ) %>%
        arm_angle_categories()
    })
    
    caption <- paste(
      'Speed:', round(pitch_movement()$RelSpeed,1),
      "\nSpin:", round(pitch_movement()$SpinRate),
      "\nAxis:", round(pitch_movement()$SpinAxis),
      "\nSpin Eff%:", round(pitch_movement()$yt_Efficiency),
      "\nDate:", pitch_movement()$Date,
      "\nBallpark:", pitch_movement()$HomeTeamCode
      
    )
    
    
    ggplotly(
      ggplot(data = pitch_movement(), aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
        labs(color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" ) + 
        xlim(-25, 25) + ylim(-25, 25) +
        geom_segment(x = 0, y = -25, xend = 0, yend = 25, linewidth = 1, color = "grey55") +
        geom_segment(x = -25, y = 0, xend = 25, yend = 0, linewidth = 1, color = "grey55") +
        stat_ellipse(aes(fill = TaggedPitchType),level = .99, alpha = 0.3, geom = "polygon") +  # Adds a filled ellipse with transparency
        geom_segment(x = 0, xend = arm_angle()$release_pos_x, y = 0, yend = arm_angle()$release_pos_z - arm_angle()$shoulder_pos, 
                     color = "grey", size = 0.75, alpha = .3) +
        geom_point(fill = 'orange', color = 'black', x=0, y=0, pch = 21, size = 4, alpha = .01, show.legend = T) + # shoulder
        geom_point(fill = 'cornflowerblue', color = 'black', x=arm_angle()$release_pos_x  , y= arm_angle()$release_pos_z - arm_angle()$shoulder_pos , 
                   pch = 21, size = 4, alpha = .01, show.legend = T) + # shoulder
        geom_point(aes(color = TaggedPitchType,
                       text = caption), # paste('Spin: ', SpinRate)), 
                   size = 1, alpha = .5) +
        scale_color_manual(values =pitch_type_colors) +
        scale_fill_manual(values = pitch_type_colors) +
        theme(legend.position = "left", 
              legend.text = element_text(size = 8), 
              axis.title = element_text(size = 10),
              aspect.ratio = 1
        ) +
        theme_minimal()+
        guides(fill = "none")     
    ) %>% 
      
      plotly::layout(autosize = T #,
                     # showlegend = TRUE,
                     # legend=list(x=0, 
                     #             xanchor='left',
                     #             yanchor='left',
                     #             orientation='v')
      ) 
    
  } )
  
  
  output$pitch_dist <- renderPlotly( {
    
    # req(input$pitchers)  
    
    table <- 
      #   dbGetQuery(db,  "SELECT * FROM pitch_data where Pitcher = :pitcher AND Date between :dates1 and :dates2 AND (TaggedPitchType is not NULL OR TaggedPitchType <>'')", 
      #                     params = list(pitcher = reactive_data$pitchers, 
      #                                   dates1 = paste(reactive_data$dates1), 
      #                                   dates2 = paste(reactive_data$dates2))
      # ) %>%
      pitcher_data() %>%
      filter(TaggedPitchType != '' | is.na(TaggedPitchType)) %>% 
      pitch_type_factor()    
    
    table_filter <- reactive({table})
    
    req(nrow(table_filter()) > 0)
    
    ggplotly(
      ggplot(table_filter()) +
        geom_density(aes(x = RelSpeed, fill = TaggedPitchType), alpha = .7) +
        geom_point(data = table_filter() %>% group_by(TaggedPitchType) %>% filter(n() < 5), 
                   aes(x = RelSpeed, y = 0.03 ,fill = TaggedPitchType), color = 'black', size = 4, pch = 21, alpha = .7) +
        facet_wrap2(~TaggedPitchType, ncol = 1, strip.position = 'left', scales = 'free_y')+
        scale_x_continuous(
          breaks = seq(
            floor(min(table_filter()$RelSpeed, na.rm = TRUE)), 
            ceiling(max(table_filter()$RelSpeed, na.rm = TRUE)), 
            by = 2
          )
        )+
        # theme_minimal()  +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.length.y = element_blank(),
              legend.position = "none",
              strip.text = element_text(color = 'black', face = 'bold'),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_line(size = 0.25, linetype = 'solid',
                                                colour = "grey"),
              panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
                                                colour = "grey") ,
              panel.background = element_rect(fill = 'white')
              # panel.background = element_rect(fill = '#f5f5f5'),
              # plot.background = element_rect(fill = '#f5f5f5')
        )   + 
        scale_fill_manual(values =pitch_type_colors) 
    )  %>% 
      # plotly::config(displayModeBar = F) %>%
      # plotly::style(hoverinfo = 'none') %>% 
      plotly::layout(autosize = TRUE) %>% 
      plotly::layout(xaxis=list(autorange=TRUE)) %>% 
      plotly::layout(yaxis=list(autorange=TRUE)) 
    
  })
  
  
  output$pitch_velo_game <- renderPlotly({
    
    # req(input$pitchers)
    # req(input$dates1)
    # req(input$dates2)
    
    if (reactive_data$dates1 == reactive_data$dates2) {
      pvp_game <- reactive({
        #   dbGetQuery(db,  "SELECT * FROM pitch_data where Pitcher = :pitcher AND Date between :dates1 and :dates2 AND (TaggedPitchType is not NULL OR TaggedPitchType <>'')", 
        #                                   params = list(pitcher = reactive_data$pitchers, dates1 = paste(reactive_data$dates1), dates2 = paste(reactive_data$dates2))
        # ) %>%
        pitcher_data() %>%
          filter(!is.na(RelSpeed))  %>%
          pitch_type_factor_and_recode() %>%
          dplyr::group_by(Date, Pitcher, TaggedPitchType, Inning, HomeTeamCode) %>%
          dplyr::summarise(Avg = mean(RelSpeed, na.rm = TRUE), 
                           Max = max(RelSpeed, na.rm = T), 
                           min = min(RelSpeed, na.rm = T),
                           Spin = mean(SpinRate, na.rm = T),
                           IVB = mean(InducedVertBreak, na.rm = T),
                           HB = mean(HorzBreak, na.rm = T)
          ) %>%
          dplyr::arrange(Date, desc(Max)) %>%
          dplyr:: mutate(Date = as.Date(Date)) %>%
          pitch_type_recode()%>%
          filter(!is.na(Date))
      })
      
      caption <- paste(
        'Speed:', round(pvp_game()$Avg,1),
        "\nSpin:", round(pvp_game()$Spin),
        "\nIVB:", round(pvp_game()$IVB,1),
        "\nHB:", round(pvp_game()$HB,1),
        "\nBallpark:", pvp_game()$HomeTeamCode
      )
      
      if(length(unique(pvp_game()$Inning)) == 1) {
        ggplotly(
          ggplot(data = pvp_game(), aes(x = factor(Inning), y = Avg, fill = TaggedPitchType) ) +
            geom_point( size = 3, alpha = .75, color = 'black', aes(text = caption) )+
            scale_fill_manual(values = c(pitch_type_colors)) + # , na.rm = TRUE)+
            labs(x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by Date") +
            theme_bw() + 
            theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
                  axis.text = element_text(size = 8),
                  axis.text.x = element_text(angle = 90),
                  legend.position = "bottom", 
                  legend.text = element_text(size = 8),
                  axis.title = element_text(size = 8))
        )
      } else{
        
        ggplotly(
          ggplot(data = pvp_game(), aes(x = factor(Inning), y = Avg, color = TaggedPitchType, group = TaggedPitchType) ) +
            geom_point( size = 2, alpha = .75, aes(text = caption)) +
            geom_line() +
            scale_color_manual(values = c(pitch_type_colors)) + # , na.rm = TRUE)+
            labs(x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by Date") +
            theme_bw() + 
            theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
                  axis.text = element_text(size = 8),
                  axis.text.x = element_text(angle = 90),
                  legend.position = "bottom", 
                  legend.text = element_text(size = 8),
                  axis.title = element_text(size = 8))
        ) 
        
      }
      
    } else  {
      
      pvp_game <- reactive({ 
        
        #   dbGetQuery(db,  "SELECT * FROM pitch_data where Pitcher = :pitcher AND Date between :dates1 and :dates2 AND (TaggedPitchType is not NULL OR TaggedPitchType <>'')", 
        #                                   params = list(pitcher = reactive_data$pitchers, dates1 = paste(reactive_data$dates1), dates2 = paste(reactive_data$dates2))
        # ) %>%
        pitcher_data() %>%
          filter(!is.na(RelSpeed))  %>%
          pitch_type_factor_and_recode() %>%
          dplyr::group_by(Date, Pitcher, TaggedPitchType, HomeTeamCode) %>%
          dplyr::summarise(Avg = mean(RelSpeed, na.rm = TRUE), 
                           Max = max(RelSpeed, na.rm = T), 
                           min = min(RelSpeed, na.rm = T),
                           Spin = mean(SpinRate, na.rm = T),
                           IVB = mean(InducedVertBreak, na.rm = T),
                           HB = mean(HorzBreak, na.rm = T)
          ) %>%
          dplyr::arrange(Date, desc(Max)) %>%
          dplyr:: mutate(Date = as.Date(Date)) %>%
          pitch_type_recode() %>%
          filter(!is.na(Date))
      })
      
      
      caption <- paste(
        'Speed:', round(pvp_game()$Avg,1),
        "\nSpin:", round(pvp_game()$Spin),
        "\nIVB:", round(pvp_game()$IVB,1),
        "\nHB:", round(pvp_game()$HB,1),
        "\nBallpark:", pvp_game()$HomeTeamCode
      )
      
      ggplotly(
        ggplot(data = pvp_game(), aes(x = factor(Date), y = Avg, color = TaggedPitchType, group = TaggedPitchType) ) +
          geom_point( size = 2, alpha = .75, aes(text = caption)) +
          geom_line() +
          scale_color_manual(values = c(pitch_type_colors)) + # , na.rm = TRUE)+
          labs(x = "Date", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by Date") +
          theme_bw() + 
          theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
                axis.text = element_text(size = 8),
                axis.text.x = element_text(angle = 90),
                legend.position = "bottom", 
                legend.text = element_text(size = 8),
                axis.title = element_text(size = 8))
      ) 
    } 
    
    
  })
  
  
  output$spinaxis_observed <- renderPlot({
    test_data <- reactive({
      # dbGetQuery(db,  "SELECT Date, Pitcher, TaggedPitchType, SpinAxis, InducedVertBreak,HorzBreak 
      #                       FROM pitch_data where Pitcher = :pitcher AND Date between :dates1 and :dates2 AND (TaggedPitchType is not NULL OR TaggedPitchType <>'')", 
      #            params = list(pitcher = reactive_data$pitchers, dates1 = paste(reactive_data$dates1), dates2 = paste(reactive_data$dates2))
      # ) %>%
      pitcher_data() %>%
        filter(TaggedPitchType != '') %>% 
        filter(!is.na(SpinAxis)) %>% 
        pitch_type_factor_and_recode() %>%
        mutate(SpinAxis_rounded = round(SpinAxis / 5) * 5,
               SpinAxis_inferred =  atan2(HorzBreak,InducedVertBreak) * (180 / pi) + 180,
               # SpinAxis_inferred2 = ifelse(SpinAxis_inferred2 < 0, SpinAxis_inferred2 + 360, SpinAxis_inferred2)
        ) %>%
        group_by(Pitcher, TaggedPitchType, SpinAxis_rounded) %>%
        summarise(count = n())
    })
    
    ggplot(test_data(), aes(x = SpinAxis_rounded, y = 1)) +
      geom_hline(yintercept = 1)+      
      geom_point(alpha = .7, aes(size = count, fill = TaggedPitchType), color = 'black',  shape = 21) +
      scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 30)) + 
      scale_y_continuous(limits = c(0, 1)) +
      coord_polar(theta = "x", start = pi) +  
      theme_minimal() + 
      scale_fill_manual(values = c(pitch_type_colors))+
      labs(x = "Spin Axis (degrees)", y = "", fill = '') +
      theme(
        panel.grid = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(), 
        # plot.title.position = 'none',
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = 'right',
        legend.text = element_text(size = 12)
      )  +
      scale_size_continuous(range = c(2, 12), guide = 'none') +
      guides(fill = guide_legend(override.aes = list(size = 6), 
                                 ncol = 1) ) 
  })
  
  output$spinaxis_inferred <- renderPlot({
    test_data <- reactive({
      # dbGetQuery(db,  "SELECT Date, Pitcher, TaggedPitchType, SpinAxis, InducedVertBreak,HorzBreak 
      #                       FROM pitch_data where Pitcher = :pitcher AND Date between :dates1 and :dates2 AND (TaggedPitchType is not NULL OR TaggedPitchType <>'')", 
      #            params = list(pitcher = reactive_data$pitchers, dates1 = paste(reactive_data$dates1), dates2 = paste(reactive_data$dates2))
      # ) %>%
      pitcher_data() %>%
        filter(TaggedPitchType != '') %>% 
        filter(!is.na(SpinAxis)) %>% 
        pitch_type_factor_and_recode() %>%
        mutate(SpinAxis_inferred =  atan2(HorzBreak,InducedVertBreak) * (180 / pi) + 180,
               SpinAxis_rounded = round(SpinAxis_inferred / 5) * 5,
        ) %>%
        group_by(Pitcher, TaggedPitchType, SpinAxis_rounded) %>%
        summarise(count = n())
    })
    
    ggplot(test_data(), aes(x = SpinAxis_rounded, y = 1)) +
      geom_hline(yintercept = 1)+      
      geom_point(alpha = .7, aes(size = count, fill = TaggedPitchType), color = 'black',  shape = 21) +
      scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 30)) + 
      scale_y_continuous(limits = c(0, 1)) +
      coord_polar(theta = "x", start = pi) +  
      theme_minimal() + 
      scale_fill_manual(values = c(pitch_type_colors))+
      labs(x = "Spin Axis (degrees)", y = "", fill = '') +
      theme(
        panel.grid = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(), 
        # plot.title.position = 'none',
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = 'right',
        legend.text = element_text(size = 12)
      )  +
      scale_size_continuous(range = c(2, 12), guide = 'none') +
      guides(fill = guide_legend(override.aes = list(size = 6), 
                                 ncol = 1) ) 
  })
  
  
  
  output$arm_angle <- renderPlot({ # change this based on which plot i want to show
    
    # req(input$pitchers)
    # req(input$dates1)
    # req(input$dates2)
    
    
    # pitch_data <- reactive({
    #   dbGetQuery(db, glue('select Date, Pitcher, PitcherTeam, PitcherThrows, TaggedPitchType,RelSpeed, InducedVertBreak, HorzBreak, SpinRate, SpinAxis, arm_angle_savant, arm_angle, RelHeight, RelSide, height_inches, shoulder_pos, HomeTeamCode, yt_Efficiency from pitch_data
    #                where Pitcher = "{reactive_data$pitchers}" and 
    #              Date between "{reactive_data$dates1}" and "{reactive_data$dates2}"')
    #   )
    # })
    # 
    
    pitcher_plot(pitcher_name = paste(reactive_data$pitchers), 
                 plot_type = 'arm_angle', pitch_data_df = pitcher_data())
    
  })
  
  filtered_data <- reactive({
    stevendata %>%
      filter(
        Season == input$season_input,
        Batter == input$batter_input
      )
  })
  
  filtered_data3 <- reactive({
    stevendata2 %>%
      filter(
        SEASON == input$season_input,
        Batter == input$batter_input
      )
  })
  
  output$xwoba_plot <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) == 0) return(NULL)
    
    latest_woba = data %>%
      filter(Season == 2025, pa == max(pa, na.rm = T)) %>%
      pull(woba)
    
    Batter = data %>%
      pull(Batter)
    
    Season = data %>%
      pull(Season)
    
    wobatitle = paste0(Season, " Rolling wOBA & xwOBA - 25 PA")
    
    ggplot(data %>% filter(Season == 2025), aes(x = pa)) +
      geom_path(aes(y = woba, group = 1, color = "wOBA"), size = 2) +
      geom_path(aes(y = xwoba, group = 1, color = "xwOBA"), size = 2) +
      scale_y_continuous(limits = c(0.25, 0.55)) +
      geom_hline(yintercept = latest_woba, linetype = "dashed", size = 1.5) +
      labs(title = wobatitle,
           x = "Last 25 PA",
           y = "wOBA & xwOBA") +
      theme_bw() +
      theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
  })
  
  steventableFilter <- reactive({
    stevenreactable %>%
      filter(Batter == input$batter_input) %>%
      select(Yr, G, PA, BA, OBP, SLG, OPS, wOBA, xwOBA) %>%
      mutate(BA = round(BA, 3), OBP = round(OBP, 3), SLG = round(SLG, 3), OPS = round(OPS, 3),
             wOBA = round(wOBA, 3), xwOBA = round(xwOBA, 3))
  })
  
  output$summary_table2 <- renderReactable({
    
    reactable(steventableFilter(),
              rowStyle = function(index) {
                if (index==nrow(steventableFilter())) {
                  list(background = "lightgrey")
                }
              },
              theme = reactableTheme(
                style = list(fontFamily = "Calibri, sans-serif"),
                headerStyle = list(background = "hsl(219, 89%, 14%);",  color = "hsl(0, 0%, 100%);")
              ),
              defaultColDef = colDef(align = 'center',
                                     minWidth = 30  # Adjust the minimum width as needed
              ),
              columns = list(
                wOBA = colDef(minWidth = 35),
                xwOBA = colDef(minWidth = 35)
              )
    )
  })
  
  output$aheadswing <- renderPlot({
    aheaddata <- filtered_data3() %>%
      filter(count %in% c("1-0", "2-0", "3-0", "2-1", "3-1", "3-2"))
    
    Batter = aheaddata %>%
      pull(Batter)
    
    Season = aheaddata %>%
      pull(SEASON)
    
    swingmaptitle = paste0(Season, " Ahead in Count Swing Profile")
    
    plate_width <- 17 + 2 * (9/pi)
    
    home_plate = data.frame(x = c(-0.708, -0.708, -0.708, 0, 0.708),
                            y = c(0.5, 0.3, 0.3, 0.15, 0.5),
                            xend = c(0.708, -0.708, 0, 0.708, 0.708),
                            yend = c(0.5, 0.5, 0.15, 0.3, 0.3))
    
    strike_zone = data.frame(x = c(-0.3156886, 0.3156886, -plate_width/24, -plate_width/24),
                             xend = c(-0.3156886, 0.3156886, plate_width/24, plate_width/24),
                             y = c(1.4, 1.4, 2.2, 2.9),
                             yend = c(3.6, 3.6, 2.2, 2.9))
    
    fastballswing = aheaddata %>%
      filter(swing == 1, PitchClass == "Fastballs") %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                      linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
      geom_rect(xmin = -(plate_width/2)/12,
                xmax = (plate_width/2)/12,
                ymin = 1.5,
                ymax = 3.5, color = "black", alpha = 0, linewidth = 1.2) +
      geom_segment(data = home_plate,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1) +
      geom_segment(data = strike_zone,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1, linetype = "dashed", alpha = 0.75) +
      scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_x_continuous(limits = c(-1.5, 1.5)) +
      scale_y_continuous(limits = c(0, 4.25)) +
      annotate(geom = "text", label = "Fastballs", x = 0, y = 4, fontface = "bold", size = 5, color = "#D22D49") +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -0.5), axis.ticks = element_blank(),
            strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
            axis.text = element_blank())
    
    breakingballswing = aheaddata %>%
      filter(swing == 1, PitchClass == "BreakingBalls") %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                      linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
      geom_rect(xmin = -(plate_width/2)/12,
                xmax = (plate_width/2)/12,
                ymin = 1.5,
                ymax = 3.5, color = "black", alpha = 0, linewidth = 1.2) +
      geom_segment(data = home_plate,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1) +
      geom_segment(data = strike_zone,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1, linetype = "dashed", alpha = 0.8) +
      scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_x_continuous(limits = c(-1.5, 1.5)) +
      scale_y_continuous(limits = c(0, 4.25)) +
      annotate(geom = "text", label = "Breaking Pitches", x = 0, y = 4, fontface = "bold", size = 5, color = "#0068FF") +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -1), axis.ticks = element_blank(),
            strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
            axis.text = element_blank())
    
    offspeedswing = aheaddata %>%
      filter(swing == 1, PitchClass == "Offspeed") %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                      linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
      geom_rect(xmin = -(plate_width/2)/12,
                xmax = (plate_width/2)/12,
                ymin = 1.5,
                ymax = 3.5, color = "black", alpha = 0, linewidth = 1.2) +
      geom_segment(data = home_plate,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1) +
      geom_segment(data = strike_zone,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1, linetype = "dashed", alpha = 0.9) +
      scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_x_continuous(limits = c(-1.5, 1.5)) +
      scale_y_continuous(limits = c(0, 4.25)) +
      annotate(geom = "text", label = "Offspeed Pitches", x = 0, y = 4, fontface = "bold", size = 5, color = "#60DB33") +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -0.75), axis.ticks = element_blank(),
            strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
            axis.text = element_blank())
    
    swingplot = ggarrange(fastballswing, breakingballswing, offspeedswing, ncol = 3)
    
    annotate_figure(swingplot,
                    left = text_grob("Horizontal Pitch Location (ft.)", rot = 90),
                    bottom = text_grob("Vertical Pitch Location (ft.)"),
                    top = text_grob(swingmaptitle, size = 20, face = "bold"))
  })
  
  output$evenswing <- renderPlot({
    evendata <- filtered_data3() %>%
      filter(count %in% c("0-0", "1-1", "2-2"))
    
    Batter = evendata %>%
      pull(Batter)
    
    Season = evendata %>%
      pull(SEASON)
    
    swingmaptitle = paste0(Season, " Even in Count Swing Profile")
    
    plate_width <- 17 + 2 * (9/pi)
    
    home_plate = data.frame(x = c(-0.708, -0.708, -0.708, 0, 0.708),
                            y = c(0.5, 0.3, 0.3, 0.15, 0.5),
                            xend = c(0.708, -0.708, 0, 0.708, 0.708),
                            yend = c(0.5, 0.5, 0.15, 0.3, 0.3))
    
    strike_zone = data.frame(x = c(-0.3156886, 0.3156886, -plate_width/24, -plate_width/24),
                             xend = c(-0.3156886, 0.3156886, plate_width/24, plate_width/24),
                             y = c(1.4, 1.4, 2.2, 2.9),
                             yend = c(3.6, 3.6, 2.2, 2.9))
    
    fastballswing = evendata %>%
      filter(swing == 1, PitchClass == "Fastballs") %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                      linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
      geom_rect(xmin = -(plate_width/2)/12,
                xmax = (plate_width/2)/12,
                ymin = 1.5,
                ymax = 3.5, color = "black", alpha = 0, linewidth = 1.2) +
      geom_segment(data = home_plate,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1) +
      geom_segment(data = strike_zone,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1, linetype = "dashed", alpha = 0.75) +
      scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_x_continuous(limits = c(-1.5, 1.5)) +
      scale_y_continuous(limits = c(0, 4.25)) +
      annotate(geom = "text", label = "Fastballs", x = 0, y = 4, fontface = "bold", size = 5, color = "#D22D49") +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -0.5), axis.ticks = element_blank(),
            strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
            axis.text = element_blank())
    
    breakingballswing = evendata %>%
      filter(swing == 1, PitchClass == "BreakingBalls") %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                      linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
      geom_rect(xmin = -(plate_width/2)/12,
                xmax = (plate_width/2)/12,
                ymin = 1.5,
                ymax = 3.5, color = "black", alpha = 0, linewidth = 1.2) +
      geom_segment(data = home_plate,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1) +
      geom_segment(data = strike_zone,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1, linetype = "dashed", alpha = 0.8) +
      scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_x_continuous(limits = c(-1.5, 1.5)) +
      scale_y_continuous(limits = c(0, 4.25)) +
      annotate(geom = "text", label = "Breaking Pitches", x = 0, y = 4, fontface = "bold", size = 5, color = "#0068FF") +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -1), axis.ticks = element_blank(),
            strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
            axis.text = element_blank())
    
    offspeedswing = evendata %>%
      filter(swing == 1, PitchClass == "Offspeed") %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                      linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
      geom_rect(xmin = -(plate_width/2)/12,
                xmax = (plate_width/2)/12,
                ymin = 1.5,
                ymax = 3.5, color = "black", alpha = 0, linewidth = 1.2) +
      geom_segment(data = home_plate,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1) +
      geom_segment(data = strike_zone,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1, linetype = "dashed", alpha = 0.9) +
      scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_x_continuous(limits = c(-1.5, 1.5)) +
      scale_y_continuous(limits = c(0, 4.25)) +
      annotate(geom = "text", label = "Offspeed Pitches", x = 0, y = 4, fontface = "bold", size = 5, color = "#60DB33") +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -0.75), axis.ticks = element_blank(),
            strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
            axis.text = element_blank())
    
    swingplot = ggarrange(fastballswing, breakingballswing, offspeedswing, ncol = 3)
    
    annotate_figure(swingplot,
                    left = text_grob("Horizontal Pitch Location (ft.)", rot = 90),
                    bottom = text_grob("Vertical Pitch Location (ft.)"),
                    top = text_grob(swingmaptitle, size = 20, face = "bold"))
  })
  
  output$behindswing <- renderPlot({
    behinddata <- filtered_data3() %>%
      filter(count %in% c("0-1", "0-2", "1-2"))
    
    Batter = behinddata %>%
      pull(Batter)
    
    Season = behinddata %>%
      pull(SEASON)
    
    swingmaptitle = paste0(Season, " Behind in Count Swing Profile")
    
    plate_width <- 17 + 2 * (9/pi)
    
    home_plate = data.frame(x = c(-0.708, -0.708, -0.708, 0, 0.708),
                            y = c(0.5, 0.3, 0.3, 0.15, 0.5),
                            xend = c(0.708, -0.708, 0, 0.708, 0.708),
                            yend = c(0.5, 0.5, 0.15, 0.3, 0.3))
    
    strike_zone = data.frame(x = c(-0.3156886, 0.3156886, -plate_width/24, -plate_width/24),
                             xend = c(-0.3156886, 0.3156886, plate_width/24, plate_width/24),
                             y = c(1.4, 1.4, 2.2, 2.9),
                             yend = c(3.6, 3.6, 2.2, 2.9))
    
    fastballswing = behinddata %>%
      filter(swing == 1, PitchClass == "Fastballs") %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                      linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
      geom_rect(xmin = -(plate_width/2)/12,
                xmax = (plate_width/2)/12,
                ymin = 1.5,
                ymax = 3.5, color = "black", alpha = 0, linewidth = 1.2) +
      geom_segment(data = home_plate,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1) +
      geom_segment(data = strike_zone,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1, linetype = "dashed", alpha = 0.75) +
      scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_x_continuous(limits = c(-1.5, 1.5)) +
      scale_y_continuous(limits = c(0, 4.25)) +
      annotate(geom = "text", label = "Fastballs", x = 0, y = 4, fontface = "bold", size = 5, color = "#D22D49") +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -0.5), axis.ticks = element_blank(),
            strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
            axis.text = element_blank())
    
    breakingballswing = behinddata %>%
      filter(swing == 1, PitchClass == "BreakingBalls") %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                      linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
      geom_rect(xmin = -(plate_width/2)/12,
                xmax = (plate_width/2)/12,
                ymin = 1.5,
                ymax = 3.5, color = "black", alpha = 0, linewidth = 1.2) +
      geom_segment(data = home_plate,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1) +
      geom_segment(data = strike_zone,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1, linetype = "dashed", alpha = 0.8) +
      scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_x_continuous(limits = c(-1.5, 1.5)) +
      scale_y_continuous(limits = c(0, 4.25)) +
      annotate(geom = "text", label = "Breaking Pitches", x = 0, y = 4, fontface = "bold", size = 5, color = "#0068FF") +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -1), axis.ticks = element_blank(),
            strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
            axis.text = element_blank())
    
    offspeedswing = behinddata %>%
      filter(swing == 1, PitchClass == "Offspeed") %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                      linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
      geom_rect(xmin = -(plate_width/2)/12,
                xmax = (plate_width/2)/12,
                ymin = 1.5,
                ymax = 3.5, color = "black", alpha = 0, linewidth = 1.2) +
      geom_segment(data = home_plate,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1) +
      geom_segment(data = strike_zone,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", size = 1, linetype = "dashed", alpha = 0.9) +
      scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
      scale_x_continuous(limits = c(-1.5, 1.5)) +
      scale_y_continuous(limits = c(0, 4.25)) +
      annotate(geom = "text", label = "Offspeed Pitches", x = 0, y = 4, fontface = "bold", size = 5, color = "#60DB33") +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -0.75), axis.ticks = element_blank(),
            strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
            axis.text = element_blank())
    
    swingplot = ggarrange(fastballswing, breakingballswing, offspeedswing, ncol = 3)
    
    annotate_figure(swingplot,
                    left = text_grob("Horizontal Pitch Location (ft.)", rot = 90),
                    bottom = text_grob("Vertical Pitch Location (ft.)"),
                    top = text_grob(swingmaptitle, size = 20, face = "bold"))
  })
  
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
