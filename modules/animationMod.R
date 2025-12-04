
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
               textInput(ns("anim_Caption"), "Animation Caption"),
               # radioButtons("renderOption", "Render as GIF or Video", 
               #              choices = c("GIF","Video")),
               column(width = 6,
                      radioButtons(ns("TimeframeButtons"), "Time Frame", 
                            #actual values are based off column names; so if they change, this needs to change as well
                            choices = c("Days" = "daySequence", "Weeks" = "weeks_since", "MARK Time Period" = "TimePeriodDates"), 
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
               #if these column names change then this will have to change too
               selectInput(ns("facetWrapOption"), "Facet Wrap", 
                           choices = c("None" = "None", 
                                       "Species" = "Species", 
                                       "Release Site" = "ReleaseSite", 
                                       "Tag (only use less than 10 tags)" = "TAG")),
               selectInput(ns("fps_Select"), 
                           "Frames Per Second",
                           choices = c(1,2,4,5,10,20), 
                           selected = 10), 
               actionButton(ns("renderAnimationButton"), "Render Animation"), 
               h6("Notes: Need to click 'Render' button in left sidebar to obtain data. Render progress shown in RStudio console."),
               h6("GIF will appear below and is automatically saved in project directory."), 
               h6("Aggregating the data by timeframe avoids having multiple points for the same fish plotted at a time.")
             )
             
      )
    ),#end of fluidrow
    br(), 
    imageOutput(ns("movementsAnimation"))
  
  )
}
mod_animationServer <- function(id, filtered_movements_data, allColors = allColors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      animationDatalist <- eventReactive(input$renderAnimationButton,{

        movementsWithTimeForFrames <- filtered_movements_data() %>%
          mutate(
            #currently days_since isn't being used since daySequence will show date when used in transition time than days_since
            #however it might be a lto slower to render so still keeping it in just in case
            days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days"))),
            #makes sense to use floor not cieling with weeks because then there are are more fish in week 0
            # if you want to start at week 1 instead of week 0, add +1 to the end of expression
            weeks_since = as.numeric(floor(difftime(Date, min(Date), units = "weeks"))), 
            #hours not being used at all currently but could potentially be used in future. 
            #Doesn't make sense to use with movement data since it's subsetted by day but hours are used in bigAnimation on all data
            hours_since = as.numeric(floor(difftime(Datetime, min(Datetime), units = "hours"))),
            daySequence = as.Date(as.character(round(Datetime, units = "days"))),
          ) %>%
          # need to ungroup to get this code to work
          ungroup()

        
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
        #time frame options
        #aggregating and completing helps to create continuous animation to avoid erros, especially when gacet wrapping by tag
        if (input$TimeframeButtons == "weeks_since"){
          movementsGrouped <- movementsGrouped %>%
            complete(TAG, weeks_since = full_seq(min(weeks_since):max(weeks_since),1)) %>%
            group_by(TAG) %>%
            arrange(weeks_since) %>%
            fill(Species, ReleaseSite, X, Y, .direction = "downup") %>%
            ungroup()

        } else if (input$TimeframeButtons == "daySequence"){
          
          dateRange <- range(movementsGrouped$daySequence, na.rm = TRUE)
         
          movementsGrouped <- movementsGrouped %>%
            complete(TAG, daySequence = seq.Date(
              from = dateRange[1],
              to   = dateRange[2],
              by   = "day"
            )) %>%
            group_by(TAG) %>%
            arrange(daySequence) %>%
            fill(Species, ReleaseSite, X, Y, .direction = "downup") %>%
            ungroup()

        } # don't think we need to do this for time periods, but if there are errors for negative length vector, it could be related to this
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
          guides(size = 'none', color = guide_legend(title = "Movement")) +
          theme(axis.title.x = element_blank(), axis.title.y = element_blank())
        
        mapWithData <- basemapGGplot + 
          #to get the data to show up, it needs to be a layer over the basemap
          #to associate the right type of movements wth the same tag, need to group by Tag for aesthetics
          geom_sf(data = animationDatalist()$mercatorSFMovements, aes(size = 10,
                                                                      color = animationDatalist()$mercatorSFMovements$movement_only, 
                                                                      group = animationDatalist()$mercatorSFMovements$TAG)) +
          #assigns based on manually set names in app.R
          scale_color_manual(values = allColors) +
          #https://www.r-bloggers.com/2021/01/ease_aes-demo/ for dif options; not a big deal
          ease_aes('cubic-in-out') +
          labs(caption = input$anim_Caption)
        
        output$movementsAnimation <- renderImage({
          
            #makes it so code executes on button push
            input$renderAnimationButton
          #number of frames to pause at the end of the gif
            endPauseValue = 5
            #isolate makes it so it wont execute when all the inputs inside the isolate() are changed (title, fps, days/weeks)
            isolate({
              
              #time frame options
              if (input$TimeframeButtons == "weeks_since"){
                
                nframesUnit <- "num_weeks"
                
                mapWithData <- mapWithData +  
                  transition_time(weeks_since) +
                  ggtitle(
                    paste(input$anim_Title, '{frame_time}'),
                    subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(animationDatalist()$mercatorSFMovements$Date, na.rm = TRUE)))
                
                
              } else if (input$TimeframeButtons == "daySequence"){
                
                nframesUnit <- "num_days"

                mapWithData <- mapWithData +
                  transition_time(daySequence) +
                  ggtitle(
                    paste(input$anim_Title, '{frame_time}'),
                    subtitle = paste("Day {frame} of {nframes} past Initial Date of", min(animationDatalist()$mercatorSFMovements$Date, na.rm = TRUE)))
                
                
              } else if (input$TimeframeButtons == "TimePeriodDates"){
                
                nframesUnit <- "num_periods"
                
                mapWithData <- mapWithData + 
                  
                  transition_states(TimePeriodDates, 
                                    transition_length = 4,
                                    state_length = 2) +
                  labs(title = "MARK Periods") +
                  ggtitle(

                    paste(input$anim_Title, '{closest_state}')
                    )
              }
              ###Facet wrap options
              
              if(input$facetWrapOption == "Species"){
                
                mapWithData <- mapWithData +
                  facet_wrap(~Species)
                
              } else if(input$facetWrapOption == "ReleaseSite") {
                
                mapWithData <- mapWithData +
                  facet_wrap(~ReleaseSite)
              } else if(input$facetWrapOption == "TAG") {
                
                mapWithData <- mapWithData +
                  facet_wrap(~TAG)
              } 
              #display data in app
              mapWithData
              #save it automatically
              anim_save("WindyGapFishMovements.gif", gganimate::animate(mapWithData, 
                                                                        nframes = animationDatalist()[[nframesUnit]] + endPauseValue, 
                                                                        end_pause = endPauseValue,
                                                                        fps = as.numeric(input$fps_Select), height = 1200, width = 1200)) 
            })
            
            list(src = "WindyGapFishMovements.gif", contentType = "image/gif")
          },
          deleteFile = FALSE
        )
        })
    }
  )
}