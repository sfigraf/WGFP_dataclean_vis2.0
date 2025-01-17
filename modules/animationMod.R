mod_animationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             withSpinner(DTOutput(ns("movements1")))
      ),
      column(width = 4, 
             textInput(ns("anim_Title"), "Animation Title"),
             # radioButtons("renderOption", "Render as GIF or Video", 
             #              choices = c("GIF","Video")), 
             radioButtons(ns("radio2"), "Timeframe", 
                          choices = c("days", "weeks"), 
                          selected = "weeks"),
             # sliderInput("pointSize_Slider", "Select Size of Point", 
             #             min = 1, 
             #             max = 12, 
             #             value = 4),
             sliderInput(ns("fps_Slider"), "Select frames per Second",
                         min = 0,
                         max = 15,
                         value = 10,
                         step = .2), 
             pickerInput(ns("aggregator"),
                         label = "Aggregate data in Selected Time Frame",
                         choices = c("First Movement", "Last Movement"),
                         selected = "",
                         multiple = FALSE
             )
      )
    ),#end of fluidrow
    br(), 
    actionButton(ns("renderAnimationButton"), "Render Animation: Need to click 'Render Map and Data' button in Sidebar first. Takes a couple minutes to render usually. Will appear below"), 
    imageOutput(ns("movementsAnimation"))
  
  )
}
mod_animationServer <- function(id, filtered_movements_data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      animationDatalist <- eventReactive(input$renderAnimationButton,{
        Animation_function(filtered_movements_data())
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
            #isolate makes it so it wont execute when all the inputs inside the isolate() are changed (title, fps, days/weeks)
            isolate(
              if (input$radio2 == "weeks"){
                
                  mapWithData <- basemapGGplot + 
                    #to get the data to show up, it needs to be a layer over the basemap
                    #to associate the right type of movements wth the same tag, need to group by Tag for aesthetics
                    geom_sf(data = animationDatalist()$mercatorSFMovements, aes(size = 10,
                                                                 color = animationDatalist()$mercatorSFMovements$movement_only, 
                                                                 group = animationDatalist()$mercatorSFMovements$TAG)) +
                  transition_time(weeks_since) +
                  ggtitle(
                    paste(input$anim_Title, '{frame_time}'),
                    subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(animationDatalist()$mercatorSFMovements$Date)))
                
                mapWithData
                
                anim_save("WindyGapFishMovements.gif", gganimate::animate(mapWithData, 
                                                                          nframes = animationDatalist()$num_weeks, 
                                                                          fps = input$fps_Slider, height = 1200, width =1200)) 
                  
                
              } else if (input$radio2 == "days"){
                mapWithData <- basemapGGplot + 
                  geom_sf(data = animationDatalist()$mercatorSFMovements, aes(size = 10,
                                                               color = animationDatalist()$mercatorSFMovements$movement_only, 
                                                               group = animationDatalist()$mercatorSFMovements$TAG))+
                  transition_time(days_since) + 
                  labs(title = "Days") +
                  ggtitle(
                    paste(input$anim_Title, '{frame_time}'),
                    subtitle = paste("Day {frame} of {nframes} past Initial Date of", min(animationDatalist()$mercatorSFMovements$Date)))
                
                mapWithData
                
                anim_save("WindyGapFishMovements.gif", gganimate::animate(mapWithData, 
                                                                          nframes = animationDatalist()$num_days, 
                                                                          fps = input$fps_Slider, height = 1200, width = 1200)) # New
                
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