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
  
  # PITCHERS ----
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
                )
      ),
      easyClose = TRUE,   # Allows clicking outside to close the modal
      footer = modalButton("Close") # Adds a close button
    ))
  })
  
  observeEvent(input$pitch_avg, {
    
    # Modal with the reactable table
    showModal(modalDialog(
      title = "Pitch Averages", 
      
      div(
        p("This table shows average pitch metrics from the 2022-2025 Frontier League seasons, normalized to a."),
        style = "margin-bottom: 20px;"
      ),
      
      reactable(fl_avg_p,
                defaultColDef = colDef(width = 85),
                columns = list(
                  Pitch = colDef(
                    style = function(value) {
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
                  )
                )
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
              )
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
    
    # reactable(table,
    #           defaultColDef = colDef(width = 85),
    #           columns = list(
    #             Pitch = colDef(
    #               style = function(value) {
    #                 pitch_colors <- list(
    #                   FB = list(background = "red", color = 'white'),
    #                   SI = list(background = "#a34700", color = 'white'),
    #                   CT = list(background = "gold", color = 'black'),
    #                   CB = list(background = "darkgreen", color = 'white'),
    #                   SL = list(background = "cornflowerblue", color = 'white'),
    #                   CH = list(background = "violet", color = 'white'),
    #                   KN = list(background = "black", color = 'white'),
    #                   SPL = list(background = "black", color = 'white')
    #                 )
    #                 pitch_colors[[value]]
    #               }
    #             )))
    
    reactable(table,
              defaultColDef = colDef(width = 85),
              columns = c(
                list(Pitch = colDef(style = function(value) get_pitch_color(value)))#,
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
    
    reactable(fl_stats)
    
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
  
  
  
  
  
  
  
  
}