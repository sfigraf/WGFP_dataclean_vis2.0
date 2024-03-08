qaqcGhostTagMovements <- function(GhostTags, Movements_df){
  
  ghosttagsCondensed <- GhostTags %>%
    select(TagID, GhostDate) %>%
    left_join(Movements_df, by = c("TagID" = "TAG"))
  
  ghostTagsWithMovementAfterGhostDate <- ghosttagsCondensed %>%
    group_by(TagID) %>%
    filter(Date > GhostDate) %>%
    summarise(
      antennasDetectedAfterGhostDate = paste(unique(det_type), collapse = ", "),
      total_distmovedAfterGhostDate = (sum(abs(dist_moved), na.rm = TRUE)),
      maxUpstreamDistMovedAfterGhost = max(dist_moved)) %>%
    filter(total_distmovedAfterGhostDate > 0) %>%
    arrange(desc(total_distmovedAfterGhostDate))
  
  return(ghostTagsWithMovementAfterGhostDate)
}

