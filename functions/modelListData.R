modelListData <- function(siteQAQCList, flowModel = FALSE){
  final_table <- purrr::map_df(siteQAQCList, function(site_data) {
    
    # 1. Check if site_data is just a character string (the "empty" case)
    if (is.character(site_data)) {
      if(flowModel){
        
        return(
          data.frame(
            model_type = c("baseModelList", "noOutliersModelList", "dailyFlowModelList", "allDataFlowModelList"),
            intercept_val = c(NA_real_, NA_real_, NA_real_, NA_real_),
            slope_val = c(NA_real_, NA_real_, NA_real_, NA_real_), 
            DataCalStart = c(NA, NA, NA, NA), 
            DataCalEnd = c(NA, NA, NA, NA)
          )
        )
        
      } else{
        return(
          data.frame(
            model_type = c("baseModelList", "noOutliersModelList"),
            intercept_val = c(NA_real_, NA_real_),
            slope_val = c(NA_real_, NA_real_), 
            DataCalStart = c(NA, NA), 
            DataCalEnd = c(NA, NA)
          )
        )
      }
      
    }
    
    # 2. If it's not a character, proceed with data extraction
    # We use tryCatch or basic NULL checks to ensure the coef() call doesn't break
    cal_start <- min(site_data$USGSGageHeightList$subsetDataWithOutliersPredicted$dateTime, na.rm = TRUE)
    cal_end   <- max(site_data$USGSGageHeightList$subsetDataWithOutliersPredicted$dateTime, na.rm = TRUE)
    
    # Base Model Row
    base_row <- data.frame(
      model_type = "baseModelList",
      intercept_val = site_data$USGSGageHeightList$baseModelList$intercept_val %||% NA,
      slope_val = site_data$USGSGageHeightList$baseModelList$slope_val %||% NA, 
      DataCalStart = cal_start,
      DataCalEnd = cal_end
    )
    
    # No Outliers Model Row (using your specific coef logic)
    # We check if the model object exists before trying to index [2]
    no_outlier_slope <- NA
    if (!is.null(site_data$USGSGageHeightList$noOutliersModelList$noOutliersModel)) {
      no_outlier_slope <- coef(site_data$USGSGageHeightList$noOutliersModelList$noOutliersModel)[2]
    }
    
    no_outliers_row <- data.frame(
      model_type = "noOutliersModelList",
      intercept_val = site_data$USGSGageHeightList$noOutliersModelList$intercept_val %||% NA,
      slope_val = no_outlier_slope, 
      DataCalStart = cal_start,
      DataCalEnd = cal_end
    )
    
    # Combine the two rows for this site
    rows <- bind_rows(base_row, no_outliers_row)
    
    if(flowModel){
      # dailyFlowModel Row
      dailyFlowModelList_row <- data.frame(
        model_type = "dailyFlowModelList",
        intercept_val = site_data$flowModelList$dailyFlowModelList$intercept_val %||% NA,
        slope_val = site_data$flowModelList$dailyFlowModelList$slope_val %||% NA, 
        DataCalStart = cal_start,
        DataCalEnd = cal_end
      )
      
      allDataFlowModelList_row <- data.frame(
        model_type = "allDataFlowModelList",
        intercept_val = site_data$flowModelList$allDataFlowModelList$intercept_val %||% NA,
        slope_val = site_data$flowModelList$allDataFlowModelList$slope_val %||% NA, 
        DataCalStart = cal_start,
        DataCalEnd = cal_end
      )
      
      rows <- bind_rows(rows, dailyFlowModelList_row, allDataFlowModelList_row)
      
      
    }
    rows
    
    
  }, .id = "site_name")
  
  # Clean up the names (optional: removes the 'slope_val' name if coef() kept it)
  final_table$slope_val <- as.numeric(final_table$slope_val)
  return(final_table)
}