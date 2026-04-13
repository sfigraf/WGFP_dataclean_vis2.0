#meant for a df with the coliumns Water_Level_NoIce_ft and USGSGageHeightFt to compare against each other
pressureTransducerQAQCFunction <- function(subsetData){
  # subsetData <- rbOnly %>%
  #   filter(year(dateTime) == 2021, 
  #          Water_Level_NoIce_ft > 0,
  #          !is.na(gageDif))
  #plotting against each other to decide to do a linear baseModel (y = mx+b)
  # plot <- subsetData %>%
  #   ggplot(aes(x = USGSGageHeightFt, y = Water_Level_NoIce_ft)) +
  #   geom_point() +
  #   ggtitle("Red Barn 2021 USGS vs gage height") +
  #   theme_classic()
  # ggplotly(plot)
  
  baseModel <- lm(Water_Level_NoIce_ft ~ USGSGageHeightFt, data = subsetData)
  
  baseModelList <- list(
    "baseModel" = baseModel,
    "summary" = summary(baseModel),
    "plot" = plot(baseModel),
    "resids" = residuals(baseModel), 
    "intercept_val" = coef(baseModel)[1],
    "slope_val" = coef(baseModel)[2]
  )
  
  # See the results
  # summary(baseModel)
  # #r squared = .8912
  # #residuals
  # plot(baseModel)
  # resids <- residuals(baseModel)
  #outliers; need to recalculate for these vals?
  #clean data; no outliers
  #
  
  ##flag outliers
  #add residuelas as a column
  subsetData$resids <- residuals(baseModel)
  subsetData$is_outlier <- abs(subsetData$resids) > 3 * sd(subsetData$resids)
  
  numberOfOutliers <- subsetData %>%
    filter(is_outlier) %>%
    nrow()
  
  print(paste("Number of outliers present:", numberOfOutliers))
  # 3. Check how many you have
  #table(subsetData$is_outlier)
  #recalulte baseModel with outliers removed
  subsetData_noOutliers <- subsetData %>%
    filter(!is_outlier)
  modelNoOutliersFromModel1 <- lm(Water_Level_NoIce_ft ~ USGSGageHeightFt, data = subsetData_noOutliers)
  
  noOutliersModelList <- list(
    "noOutliersModel" = modelNoOutliersFromModel1,
    "summary" = summary(modelNoOutliersFromModel1),
    "plot" = plot(modelNoOutliersFromModel1),
    "resids" = residuals(modelNoOutliersFromModel1), 
    "intercept_val" = coef(modelNoOutliersFromModel1)[1],
    "slope_val" <- coef(modelNoOutliersFromModel1)[2]
  )
  # See the results
  #summary(modelNoOutliersFromModel1)
  
  ##add line to the plot from before
  intercept_val <- coef(baseModel)[1]
  slope_val <- coef(baseModel)[2]
  #slope val .8346057
  
  #'No outlier' baseModel values
  intercept_valNoOutliers <- coef(modelNoOutliersFromModel1)[1]
  slope_valNoOutliers <- coef(modelNoOutliersFromModel1)[2]
  
  #predict vals
  subsetDataPredicted <- subsetData %>%
    mutate(predictedValue = predict(modelNoOutliersFromModel1, newdata = .), 
           predictedValueDif = predictedValue - Water_Level_NoIce_ft) 
  
  plot <- subsetDataPredicted %>%
    ggplot(aes(x = USGSGageHeightFt, y = Water_Level_NoIce_ft, color = is_outlier, alpha = .6, 
               text = paste0("USGS gage height: ", USGSGageHeightFt,
                             "<br>PT Gage Height: ", Water_Level_NoIce_ft,
                             "<br>Gage Difference: ", gageDif,
                             "<br>Predicted Value: ", predictedValue,
                             "<br>Predicted Value - Actual value: ", predictedValueDif,
                             "<br>Datetime: ", dateTime, 
                             "<br>Is Outlier: ", is_outlier
               )
    )
    ) +
    geom_point() +
    ggtitle("Red Barn 2021 USGS vs gage height") +
    theme_classic() +
    scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
    geom_abline(intercept = intercept_val, slope = slope_val, color = "blue", linewidth = 1) +
    geom_abline(intercept = intercept_valNoOutliers, slope = slope_valNoOutliers, linetype = "dashed", color = "cyan")
  
  plot1 <- ggplotly(plot, tooltip = "text")
  
  return(list(
    "baseModelList" = baseModelList,
    "noOutliersModelList" = noOutliersModelList,
    "plotIwthModels" = plot1,
    "subsetDataWithOutliersPredicted" = subsetDataPredicted
  ))
}