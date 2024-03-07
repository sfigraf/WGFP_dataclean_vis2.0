qaqcGhostTagMovements <- function(GhostTags, Movements_df){
  
  ghosttagsCondensed <- GhostTags %>%
    select(TagID, GhostDate) %>%
    left_join(Movements_df, by = c("TagID" = "TAG"))
  
  ghostTagsWithMovementAfterGhostDate <- ghosttagsCondensed %>%
    group_by(TagID) %>%
    filter(Date > GhostDate) %>%
    summarise(total_distmovedAfterGhostDate = (sum(abs(dist_moved), na.rm = TRUE))) %>%
    filter(total_distmovedAfterGhostDate >= 50) %>%
    arrange(desc(total_distmovedAfterGhostDate))
  
  return(ghostTagsWithMovementAfterGhostDate)
}