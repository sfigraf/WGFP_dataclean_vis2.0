###############To do:
##### ADD periods as group to animation
#try different transitions?
#make sure to get period dates in title
#mess around with different eases


mod_animationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             titlePanel("Data to Animate"),
               withSpinner(DTOutput(ns("movements1")))
      ),
      column(width = 4, 
             sidebarPanel(
               
               width = 12,
               titlePanel("Animation Specifications"),
               textInput(ns("anim_Title"), "Animation Title"),
               # radioButtons("renderOption", "Render as GIF or Video", 
               #              choices = c("GIF","Video")),
               column(width = 6,
                      radioButtons(ns("TimeframeButtons"), "Timeframe", 
                            #actual values are based off column names; so if they change, this needs to change as well
                            choices = c("Days" = "days_since", "Weeks" = "weeks_since", "MARK Time Period" = "TimePeriodDates"), 
                            selected = "weeks_since")
               ),
               column(width = 6,
                      radioButtons(ns("aggregator"),
                                  label = "Aggregate data in Selected Time Frame",
                                  choices = c("First Movement", "Last Movement", "None"),
                                  selected = "First Movement"
                        )
                      ),
               # sliderInput("pointSize_Slider", "Select Size of Point", 
               #             min = 1, 
               #             max = 12, 
               #             value = 4),
               sliderInput(ns("fps_Slider"), "Select frames per Second",
                           min = 0,
                           max = 15,
                           value = 10,
                           step = .2), 
               actionButton(ns("renderAnimationButton"), "Render Animation"), 
               h6("Notes: Need to click 'Render Map and Data' button to obtain data. Render progress shown in RStudio console."),
               h6("GIF will appear below and is automatically saved in project directory."), 
               h6("Aggregating the data by timeframe avoids having multiple points for the same fish plotted at a time.")
             )
             
      )
    ),#end of fluidrow
    br(), 
    imageOutput(ns("movementsAnimation"))
  
  )
}
mod_animationServer <- function(id, filtered_movements_data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      animationDatalist <- eventReactive(input$renderAnimationButton,{
        print(input$TimeframeButtons)
        
        movementsWithTimeForFrames <- filtered_movements_data() %>%
          mutate(
            days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
            #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
            # if you want to start at week 1 instead of week 0, add +1 to the end of expression
            # when you change this too, it changes the number of entries in the states dataframe
            weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks"))), 
            hours_since = as.numeric(floor(difftime(Datetime, min(Datetime), units = "hours"))), 
          ) %>%
          # need to ungroup to get this code to work
          ungroup()
        filtered_movements_data()
        
        if(input$aggregator == "First Movement"){
          
          movementsGrouped <- movementsWithTimeForFrames %>%
            group_by(TAG, !!sym(input$TimeframeButtons)) %>%
            filter(Datetime == first(Datetime)) %>%
            ungroup()
          
        } else if(input$aggregator == "Last Movement"){
          movementsGrouped <- movementsWithTimeForFrames %>%
            group_by(TAG, !!sym(input$TimeframeButtons)) %>%
            filter(Datetime == last(Datetime)) %>%
            ungroup()
        } else{
          movementsGrouped <- movementsWithTimeForFrames
        }
        
        Animation_function(movementsGrouped)
      })
      
      #data output
      output$movements1 <- renderDT({
        
        req(filtered_movements_data())
        datatable(
          filtered_movements_data(),
          rownames = FALSE,
          selection = "single",
          filter = 'top',
          options = list(
            scrollX = TRUE,
            stateSave = TRUE,
            pageLength = 5,
            info = TRUE,
            lengthMenu = list(c(1, 5, 10, 25, 50, 100, 200), c("1", "5", "10", "25", "50", "100", "200")),
            dom = 'lfrtip',
            #had to add 'lowercase L' letter to display the page length again
            language = list(emptyTable = "No data to display for the selected filters.")
          )
        ) %>%
          formatRound(columns = c("UTM_X", "UTM_Y"), digits = 0, mark = "")
      })
      
      observe({
        #set basemaps default imagery to satellite and set up basemap with extent specified 
        basemaps::set_defaults(map_service = "esri", map_type = "world_imagery")
        
        basemapGGplot <- ggplot() +
          basemap_gglayer(animationDatalist()$boundingBox) +
          scale_fill_identity() +
          coord_sf() +
          theme_classic() +
          guides(size = 'none', color = guide_legend(title = "Movement"))
        
        output$movementsAnimation <- renderImage({
          
            #makes it so code executes on button push
            input$renderAnimationButton
          #number of frames to pause at the end f the gif
            endPauseValue = 5
            #isolate makes it so it wont execute when all the inputs inside the isolate() are changed (title, fps, days/weeks)
            isolate(
              if (input$TimeframeButtons == "weeks_since"){
                
                  mapWithData <- basemapGGplot + 
                    #to get the data to show up, it needs to be a layer over the basemap
                    #to associate the right type of movements wth the same tag, need to group by Tag for aesthetics
                    geom_sf(data = animationDatalist()$mercatorSFMovements, aes(size = 10,
                                                                 color = animationDatalist()$mercatorSFMovements$movement_only, 
                                                                 group = animationDatalist()$mercatorSFMovements$TAG)) +
                  transition_time(weeks_since) +
                  scale_color_manual(values = allColors) + 
                  ease_aes('cubic-in-out') +
                  labs(title = "Weeks")
                  ggtitle(
                    paste(input$anim_Title, '{frame_time}'),
                    subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(animationDatalist()$mercatorSFMovements$Date)))
                
                mapWithData
                
                anim_save("WindyGapFishMovements.gif", gganimate::animate(mapWithData, 
                                                                          nframes = animationDatalist()$num_weeks + endPauseValue, 
                                                                          end_pause = endPauseValue,
                                                                          fps = input$fps_Slider, height = 1200, width =1200)) 
                  
                
              } else if (input$TimeframeButtons == "days_since"){
                mapWithData <- basemapGGplot + 
                  geom_sf(data = animationDatalist()$mercatorSFMovements, aes(size = 10,
                                                               color = animationDatalist()$mercatorSFMovements$movement_only, 
                                                               group = animationDatalist()$mercatorSFMovements$TAG))+
                  transition_time(days_since) +
                  scale_color_manual(values = allColors) + 
                  ease_aes('cubic-in-out') +
                  labs(title = "Days") +
                  ggtitle(
                    paste(input$anim_Title, '{frame_time}'),
                    subtitle = paste("Day {frame} of {nframes} past Initial Date of", min(animationDatalist()$mercatorSFMovements$Date)))
                
                mapWithData
                
                anim_save("WindyGapFishMovements.gif", gganimate::animate(mapWithData, 
                                                                          nframes = animationDatalist()$num_days + endPauseValue, 
                                                                          end_pause = endPauseValue, 
                                                                          fps = input$fps_Slider, height = 1200, width = 1200)) # New
                
              } else if (input$TimeframeButtons == "TimePeriodDates"){
                mapWithData <- basemapGGplot + 
                  geom_sf(data = animationDatalist()$mercatorSFMovements, aes(size = 10,
                                                                              color = animationDatalist()$mercatorSFMovements$movement_only, 
                                                                              group = animationDatalist()$mercatorSFMovements$TAG)) +
                  transition_states(TimePeriodDates, 
                                    transition_length = 4,
                                    state_length = 2) +
                  scale_color_manual(values = allColors) + 
                  labs(title = "MARK Periods") +
                  ggtitle(

                    paste(input$anim_Title, '{closest_state}')#, #{frame_time}
                    #subtitle = "Ghost/predated tags included, TGM excluded"
                    )
                  #facet_wrap(~Species) +
                  #transition_time(TimeperiodsSince) +
                  # ease_aes('sine-in-out') +
                  # enter_fade() + 
                  # exit_shrink() +
                  
                  # transition_time(days_since) + 
                  # ease_aes('cubic-in-out') +
                  
                  # ggtitle(
                  #   paste(input$anim_Title, '{frame_time}'),
                  #   subtitle = paste("Day {frame} of {nframes} past Initial Date of", min(animationDatalist()$mercatorSFMovements$Date)))
                
                mapWithData
                
                anim_save("WindyGapFishMovements.gif", gganimate::animate(mapWithData, 
                                                                          nframes = animationDatalist()$num_periods + endPauseValue, 
                                                                          end_pause = endPauseValue, 
                                                                          fps = input$fps_Slider, height = 1200, width = 1200))
              }
            )
            
            list(src = "WindyGapFishMovements.gif", contentType = "image/gif")
          },
          deleteFile = FALSE
        )
        })
    }
  )
}