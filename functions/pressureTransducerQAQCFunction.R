#meant for a df with the coliumns Water_Level_NoIce_ft and USGSGageHeightFt 
# optional to model gage height against flow as awell on a daily and not daily level
subsetData <- cfFiltered#redBarn_list$`Red Barn_10`
pressureTransducerQAQCFunction <- function(subsetData, controlVariable = "USGSGageHeightFt", SiteName, flowModel = FALSE, flowVariable = "CFFlow", 
                                           showDiagnosticPlots = FALSE){
  print(paste("Start: ", min(subsetData$dateTime), "and end: ", max(subsetData$dateTime)))

  #can only use data for the model where both columns have data, so gageDif is a good column to filter on to remove NAs
  #don't model data with ice either
  subsetData <- subsetData %>%
    filter(!is.na(gageDif), 
           Water_Level_NoIce_ft > 0)
  
  if(nrow(subsetData) > 0){
    #cant use !! opterator as bang bang symbol in base r funcitons becuase it's a tidyvese thing. need to create formula first
    form <- as.formula(paste("Water_Level_NoIce_ft ~", controlVariable))
    
    baseModel <- lm(form, data = subsetData)
    
    baseModelList <- list(
      "baseModel" = baseModel,
      "summary" = summary(baseModel),
      "plot" = if(showDiagnosticPlots) plot(baseModel) else "Plot skipped", #plot(baseModel),
      "resids" = residuals(baseModel), 
      "intercept_val" = coef(baseModel)[1],
      "slope_val" = coef(baseModel)[2]
    )
    
    
    ##flag outliers as more htan 3 standard deviations away from the data
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
    
    modelNoOutliersFromModel1 <- lm(form, data = subsetData_noOutliers)
    
    noOutliersModelList <- list(
      "noOutliersModel" = modelNoOutliersFromModel1,
      "summary" = summary(modelNoOutliersFromModel1),
      "plot" = if(showDiagnosticPlots) plot(modelNoOutliersFromModel1) else "Plot skipped", #plot(modelNoOutliersFromModel1),
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
    subsetDataUSGSGagePredicted <- subsetData %>%
      mutate(predictedValue = predict(modelNoOutliersFromModel1, newdata = .), 
             predictedValueDif = predictedValue - Water_Level_NoIce_ft) 
    
    plot <- subsetDataUSGSGagePredicted %>%
      ggplot(aes(x = !!sym(controlVariable), y = Water_Level_NoIce_ft, color = is_outlier, alpha = .6, 
                 text = paste0("USGS gage height: ", !!sym(controlVariable),
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
      ggtitle(paste(SiteName, min(subsetData$dateTime), "to", max(subsetData$dateTime), "USGS vs gage height")) +
      theme_classic() +
      scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
      geom_abline(intercept = intercept_val, slope = slope_val, color = "blue", linewidth = 1) +
      geom_abline(intercept = intercept_valNoOutliers, slope = slope_valNoOutliers, linetype = "dashed", color = "cyan")
    
    
    plot1 <- ggplotly(plot, tooltip = "text")
    plot1
    
    USGSGageHeightList <- list(
      "baseModelList" = baseModelList,
      "noOutliersModelList" = noOutliersModelList,
      "plotIwthModels" = plot1,
      "subsetDataWithOutliersPredicted" = subsetDataUSGSGagePredicted
    )
    
    
    if(flowModel){
      print("Flow model Data")
      ## first model against the daily average flow for selected variable
      #get averageDaily
      dailyData <- subsetData %>%
        group_by(date1 = date(dateTime), !!sym(flowVariable)) %>%
        summarise(dailyGageHeight = mean(Water_Level_NoIce_ft, na.rm = T)) %>%
        ungroup()
      form <- as.formula(paste("dailyGageHeight ~", flowVariable))
      
      dailyDataflowModel <- lm(form, data = dailyData, na.action = na.exclude)
      
      ##plot Daily
      
      intercept_val <- coef(dailyDataflowModel)[1]
      slope_val <- coef(dailyDataflowModel)[2]
      
      dailyData$resids <- residuals(dailyDataflowModel)
      dailyData$is_outlier <- abs(dailyData$resids) > 3 * sd(dailyData$resids, na.rm = T)
      
      numberOfOutliers <- dailyData %>%
        filter(is_outlier) %>%
        nrow()
      
      print(paste("Number of outliers present:", numberOfOutliers))
      
      #predict vals
      dailyDataPredicted <- dailyData %>%
        mutate(predictedValue = predict(dailyDataflowModel, newdata = .),
               predictedValueDif = predictedValue - dailyGageHeight
               ) 
      
      plot <- dailyDataPredicted %>%
        ggplot(aes(x = !!sym(flowVariable), y = dailyGageHeight, color = is_outlier, alpha = .6, 
                   text = paste0("Flow Variable: ", !!sym(flowVariable),
                                 "<br>Average Daily PT Gage Height: ", dailyGageHeight,
                                 #"<br>Gage Difference: ", gageDif,
                                 "<br>Predicted Value: ", predictedValue,
                                 "<br>Predicted Value - Actual value: ", predictedValueDif,
                                 "<br>Date: ", date1, 
                                 "<br>Is Outlier: ", is_outlier
                   )
        )
        ) +
        geom_point() +
        ggtitle(paste(SiteName, min(dailyDataPredicted$date1), "to", max(dailyDataPredicted$date1), "Daily Assumed Flow vs gage height")) +
        theme_classic() +
        scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
        geom_abline(intercept = intercept_val, slope = slope_val, color = "blue", linewidth = 1) #+
        #geom_abline(intercept = intercept_valNoOutliers, slope = slope_valNoOutliers, linetype = "dashed", color = "cyan")
      plot1 <- ggplotly(plot, tooltip = "text")
      
      dailyFlowModelList <- list(
        "dailyFlowModel" = dailyDataflowModel,
        "summary" = summary(dailyDataflowModel),
        "plot" = if(showDiagnosticPlots) plot(dailyDataflowModel) else "Plot skipped", #plot(dailyDataflowModel),
        "resids" = residuals(dailyDataflowModel), 
        "intercept_val" = coef(dailyDataflowModel)[1],
        "slope_val" = coef(dailyDataflowModel)[2], 
        "ggplotly" = plot1, 
        "dataWithDailyPredictionsBasedonFlow" = dailyDataPredicted
      )
      
      ## then model against all flow for selected flwo variable
      form <- as.formula(paste("Water_Level_NoIce_ft ~", flowVariable))
      allDataFlowModel <- lm(form, data = subsetData, na.action = na.exclude)
      
      ##plot Daily
      
      intercept_val <- coef(allDataFlowModel)[1]
      slope_val <- coef(allDataFlowModel)[2]
      
      subsetData$resids <- residuals(allDataFlowModel)
      subsetData$is_outlier <- abs(subsetData$resids) > 3 * sd(subsetData$resids, na.rm = T)
      
      numberOfOutliers <- subsetData %>%
        filter(is_outlier) %>%
        nrow()
      
      print(paste("Number of outliers present:", numberOfOutliers))
      
      #predict vals
      subsetDataPredicted <- subsetData %>%
        mutate(predictedValue = predict(allDataFlowModel, newdata = .),
               predictedValueDif = predictedValue - Water_Level_NoIce_ft
        ) 
      
      plot <- subsetDataPredicted %>%
        ggplot(aes(x = !!sym(flowVariable), y = Water_Level_NoIce_ft, color = is_outlier, alpha = .6, 
                   text = paste0("Flow Variable: ", !!sym(flowVariable),
                                 "<br>PT Gage Height: ", Water_Level_NoIce_ft,
                                 #"<br>Gage Difference: ", gageDif,
                                 "<br>Predicted Value: ", predictedValue,
                                 "<br>Predicted Value - Actual value: ", predictedValueDif,
                                 "<br>Datetime: ", dateTime, 
                                 "<br>Is Outlier: ", is_outlier
                   )
        )
        ) +
        geom_point() +
        ggtitle(paste(SiteName, min(subsetDataPredicted$dateTime), "to", max(subsetDataPredicted$dateTime), "Flow Data vs gage height")) +
        theme_classic() +
        scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
        geom_abline(intercept = intercept_val, slope = slope_val, color = "blue", linewidth = 1)
      
      plot1 <- ggplotly(plot, tooltip = "text")
      
      allDataFlowModelList <- list(
        "allDataFlowModel" = allDataFlowModel,
        "summary" = summary(allDataFlowModel),
        "plot" = if(showDiagnosticPlots) plot(allDataFlowModel) else "Plot skipped", #plot(allDataFlowModel),
        "resids" = residuals(allDataFlowModel), 
        "intercept_val" = coef(allDataFlowModel)[1],
        "slope_val" = coef(allDataFlowModel)[2], 
        "ggplotly" = plot1, 
        "dataWithPredictionsBasedonFlow" = subsetDataPredicted
      )
      
     
      ###return it all
      flowModelList <- list(
        "dailyFlowModelList" = dailyFlowModelList,
        "allDataFlowModelList" = allDataFlowModelList
      )
      
    
      } else{
      flowModelList <- "Not modeled against discharge"
      }
    
    
    return(list(
      "USGSGageHeightList" = USGSGageHeightList,
      "flowModelList" = flowModelList
    ))
  } else{
    print("No non-NA data to model")
  }
  
}