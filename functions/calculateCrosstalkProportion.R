calculateCrosstalkProportion <- function(SelectedAllEvents, antennaCodes){
  
  antennaSpecificDetections <- SelectedAllEvents %>%
    filter(Event %in% antennaCodes) 
  
  antennaCrosstalk <- antennaSpecificDetections %>%
    group_by(Datetime, TAG) %>%
    summarise(antennas = paste(unique(Event), collapse = ", ")) %>%
    filter(grepl(",", antennas))
  
  proportionOccurance <- nrow(antennaCrosstalk)/nrow(antennaSpecificDetections)
  
  return(proportionOccurance)
}
