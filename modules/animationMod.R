mod_animationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             withSpinner(DTOutput(ns("movements1")))
      ),#end of column
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
    actionButton(ns("button9"), "Render Animation: Need to click 'Render Map and Data' button in Sidebar first. Takes a couple minutes to render usually"), 
    imageOutput(ns("plot12"))
  
  )
}
mod_animationServer <- function(id, filtered_movements_data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      animationDatalist <- eventReactive(input$button9,{
        Animation_function(filtered_movements_data)
      })
      #data output
      output$movements1 <- renderDT({
        
        req(filtered_movements_data)
        datatable(
          filtered_movements_data,
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
        basemaps::set_defaults(map_service = "esri", map_type = "world_imagery")
        
        map_with_data <- ggplot() +
          basemap_gglayer(animationDatalist()$coords1) +
          scale_fill_identity() +
          coord_sf() +
          theme_classic() +
          guides(size = 'none', color = guide_legend(title = "Movement"))
        
        output$plot12 <- renderImage(
          {
            #makes it so code executes on button push
            input$button9
            #isolate makes it so it wont execute when all the inputs inside the isolate() are changed (title, fps, days/weeks)
            isolate(
              if (input$radio2 == "weeks"){
                map_with_data <- map_with_data + 
                  geom_sf(data = animationDatalist()$data, aes(#x = animationDatalist()$data$X.1, y = animationDatalist()$data$Y.1,
                                                                  size = 10,
                                                                  color = animationDatalist()$data$movement_only, group = animationDatalist()$data$TAG))+
                  transition_time(weeks_since) +
                  ggtitle(
                    
                    paste(input$anim_Title, '{frame_time}'),
                    subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(animationDatalist()$data$Date)))
                
                map_with_data
                
                anim_save("WindyGapFishMovements.gif", animate(map_with_data, nframes = animationDatalist()$num_weeks, fps = input$fps_Slider, height = 1200, width =1200)) # New
                
                
              } else if (input$radio2 == "days"){
                map_with_data <- map_with_data + 
                  geom_sf(data = animationDatalist()$data, aes(#x = animationDatalist()$data$X.1, y = animationDatalist()$data$Y.1,
                                                                  size = 10,
                                                                  color = animationDatalist()$data$movement_only, group = animationDatalist()$data$TAG))+
                  transition_time(days_since) + 
                  labs(title = "Days") +
                  ggtitle(
                    
                    paste(input$anim_Title, '{frame_time}'),
                    subtitle = paste("Day {frame} of {nframes} past Initial Date of", min(animationDatalist()$data$Date)))
                
                map_with_data
                
                anim_save("WindyGapFishMovements.gif", animate(map_with_data, nframes = animationDatalist()$num_days, fps = input$fps_Slider, height = 1200, width =1200)) # New
                
              }
            )#end of isolate
            
            list(src = "WindyGapFishMovements.gif", contentType = "image/gif")
          },
          deleteFile = FALSE
        )
      })
      
    }
  )
}