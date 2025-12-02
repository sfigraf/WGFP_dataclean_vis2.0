#list of inputs
aggregator <- "First Movement"
TimeframeButtons <- "daySequence"
anim_Caption <- "one tag"
endPauseValue = 5
anim_Title <- "230000294213"
facetWrapOption <- ""
fps_Select <- 10

movementsOneTag <- movements_list$Movements_df %>%
  filter(TAG == "230000294213")


movementsWithTimeForFrames <- movementsOneTag %>%
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


if(aggregator == "First Movement"){
  
  movementsGrouped <- movementsWithTimeForFrames %>%
    group_by(TAG, !!sym(TimeframeButtons)) %>%
    filter(Datetime == first(Datetime)) %>%
    ungroup()
  
} else if(aggregator == "Last Movement"){
  movementsGrouped <- movementsWithTimeForFrames %>%
    group_by(TAG, !!sym(TimeframeButtons)) %>%
    filter(Datetime == last(Datetime)) %>%
    ungroup()
} else{
  movementsGrouped <- movementsWithTimeForFrames
}
#time frame options
#aggregating and completing helps to create continuous animation to avoid erros, especially when gacet wrapping by tag
if (TimeframeButtons == "weeks_since"){
  movementsGrouped <- movementsGrouped %>%
    complete(TAG, weeks_since = full_seq(min(weeks_since):max(weeks_since),1)) %>%
    group_by(TAG) %>%
    arrange(weeks_since) %>%
    fill(Species, ReleaseSite, X, Y, .direction = "downup") %>%
    ungroup()
  
} else if (TimeframeButtons == "daySequence"){
  
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
animationDatalist <- Animation_function(movementsGrouped)


basemaps::set_defaults(map_service = "esri", map_type = "world_imagery")

basemapGGplot <- ggplot() +
  basemap_gglayer(animationDatalist$boundingBox) +
  scale_fill_identity() +
  coord_sf() +
  theme_classic() +
  guides(size = 'none', color = guide_legend(title = "Movement")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

mapWithData <- basemapGGplot + 
  #to get the data to show up, it needs to be a layer over the basemap
  #to associate the right type of movements wth the same tag, need to group by Tag for aesthetics
  geom_sf(data = animationDatalist$mercatorSFMovements, aes(size = 10,
                                                              color = animationDatalist$mercatorSFMovements$movement_only, 
                                                              group = animationDatalist$mercatorSFMovements$TAG)) +
  #assigns based on manually set names in app.R
  scale_color_manual(values = allColors) +
  #https://www.r-bloggers.com/2021/01/ease_aes-demo/ for dif options; not a big deal
  ease_aes('cubic-in-out') +
  labs(caption = anim_Caption)

#time frame options
if (TimeframeButtons == "weeks_since"){
  
  nframesUnit <- "num_weeks"
  
  mapWithData <- mapWithData +  
    transition_time(weeks_since) +
    ggtitle(
      paste(anim_Title, '{frame_time}'),
      subtitle = paste("Week {frame} of {nframes} past Initial Date of", min(animationDatalist$mercatorSFMovements$Date, na.rm = TRUE)))
  
  
} else if (TimeframeButtons == "daySequence"){
  
  nframesUnit <- "num_days"
  
  mapWithData <- mapWithData +
    transition_time(daySequence) +
    ggtitle(
      paste(anim_Title, '{frame_time}'),
      subtitle = paste("Day {frame} of {nframes} past Initial Date of", min(animationDatalist$mercatorSFMovements$Date, na.rm = TRUE)))
  
  
} else if (TimeframeButtons == "TimePeriodDates"){
  
  nframesUnit <- "num_periods"
  
  mapWithData <- mapWithData + 
    
    transition_states(TimePeriodDates, 
                      transition_length = 4,
                      state_length = 2) +
    labs(title = "MARK Periods") +
    ggtitle(
      
      paste(anim_Title, '{closest_state}')
    )
}
###Facet wrap options

if(facetWrapOption == "Species"){
  
  mapWithData <- mapWithData +
    facet_wrap(~Species)
  
} else if(facetWrapOption == "ReleaseSite") {
  
  mapWithData <- mapWithData +
    facet_wrap(~ReleaseSite)
} else if(facetWrapOption == "TAG") {
  
  mapWithData <- mapWithData +
    facet_wrap(~TAG)
} 
#display data in app
#mapWithData
#save it automatically
#anim_save("WindyGapFishMovements.gif", 
gganimate::animate(mapWithData, 
                   nframes = animationDatalist[[nframesUnit]] + endPauseValue, 
                   end_pause = endPauseValue,
                   fps = as.numeric(fps_Select), height = 1200, width = 1200)
