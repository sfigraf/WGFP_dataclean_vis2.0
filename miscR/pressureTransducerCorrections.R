###Pressure Transducer Corrections
usgs15Min <- USGSData$USGS15Min
ptdDataWide <- PTData$PTDataWide

x <- ptdDataWide %>%
  mutate(gageDif = USGSGageHeightFt - Water_Level_NoIce_ft)
rbOnly <- x %>%
  filter(Site == "Red Barn") %>%
  select(dateTime, USGSDischarge, USGSGageHeightFt, Water_Level_NoIce_ft, gageDif)

plot <- rbOnly %>%
  ggplot(aes(x = gageDif, text = as.character(gageDif))) +
  geom_histogram()  +
  ggtitle("RB Only Difs") +
  theme_classic()
ggplotly(plot)

#rite_xlsx(rbOnly, "RedBarnUSGSandPTData.xlsx")
#make reverse rating curves for each year

# RED BARN 2020 -----------------------------------------------------------


rbOnly2020 <- rbOnly %>%
  filter(year(dateTime) == 2020, 
         Water_Level_NoIce_ft > 0,
         !is.na(gageDif))
#plotting against each other to decide to do a linear model (y = mx+b)
plot <- rbOnly2020 %>%
  ggplot(aes(x = USGSGageHeightFt, y = Water_Level_NoIce_ft)) +
  geom_point() +
  ggtitle("Red Barn 2020 USGS vs gage height") +
  theme_classic()
ggplotly(plot)

model <- lm(Water_Level_NoIce_ft ~ USGSGageHeightFt, data = rbOnly2020)

# See the results
summary(model)
#r squared = .8912
#residuals
plot(model)

##add line to the plot from before
intercept_val <- coef(model)[1]
slope_val <- coef(model)[2]

plot <- rbOnly2020 %>%
  ggplot(aes(x = USGSGageHeightFt, y = Water_Level_NoIce_ft)) +
  geom_point() +
  ggtitle("Red Barn 2020 USGS vs gage height") +
  theme_classic() +
  geom_abline(intercept = intercept_val, slope = slope_val, color = "blue", linewidth = 1) 
#geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray")

ggplotly(plot)

##predicting values
#get usgs values
new_usgs_values <- data.frame(USGSGageHeightFt = c(1.2, 1.5, 2.1)) 

# Use the model to predict your site's height
predicted_gage_heights <- predict(model, newdata = new_usgs_values)
new_usgs_values$PredictedGageheight <- predicted_gage_heights
predictedVals <- new_usgs_values


# Red bArn 2021 -----------------------------------------------------------
rbOnly2021 <- rbOnly %>%
  filter(year(dateTime) == 2021, 
         Water_Level_NoIce_ft > 0,
         !is.na(gageDif))
#plotting against each other to decide to do a linear model (y = mx+b)
plot <- rbOnly2021 %>%
  ggplot(aes(x = USGSGageHeightFt, y = Water_Level_NoIce_ft)) +
  geom_point() +
  ggtitle("Red Barn 2021 USGS vs gage height") +
  theme_classic()
ggplotly(plot)

model <- lm(Water_Level_NoIce_ft ~ USGSGageHeightFt, data = rbOnly2021)

# See the results
summary(model)
#r squared = .8912
#residuals
plot(model)
resids <- residuals(model)
#outliers; need to recalculate for these vals?
#clean data; no outliers
#

##flag outliers
#add residuelas as a column
rbOnly2021$resids <- residuals(model)
rbOnly2021$is_outlier <- abs(rbOnly2021$resids) > 3 * sd(rbOnly2021$resids)
numberOfOutliers <- rbOnly2021 %>%
  filter(is_outlier) %>%
  nrow()
# 3. Check how many you have
table(rbOnly2021$is_outlier)
#recalulte model with outliers removed
rbOnly2021_noOutliers <- rbOnly2021 %>%
  filter(!is_outlier)
modelNoOutliersFromModel1 <- lm(Water_Level_NoIce_ft ~ USGSGageHeightFt, data = rbOnly2021_noOutliers)

# See the results
summary(modelNoOutliersFromModel1)

##add line to the plot from before
intercept_val <- coef(model)[1]
slope_val <- coef(model)[2]
#slope val .8346057

#'No outlier' model values
intercept_valNoOutliers <- coef(modelNoOutliersFromModel1)[1]
slope_valNoOutliers <- coef(modelNoOutliersFromModel1)[2]

plot <- rbOnly2021 %>%
  ggplot(aes(x = USGSGageHeightFt, y = Water_Level_NoIce_ft, color = is_outlier, alpha = .6)) +
  geom_point() +
  ggtitle("Red Barn 2021 USGS vs gage height") +
  theme_classic() +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
  geom_abline(intercept = intercept_val, slope = slope_val, color = "blue", linewidth = 1) +
  geom_abline(intercept = intercept_valNoOutliers, slope = slope_valNoOutliers, linetype = "dashed", color = "cyan")

ggplotly(plot)

rb2021Analyized <- pressureTransducerQAQCFunction(rbOnly2021)

rb2021Analyized$plotIwthModels
rb2021Outliers <- rb2021Analyized$subsetDataWithOutliers %>%
  filter(is_outlier) 
##predicting values ofr 2021 red barn using model without the outliers
rb2021Predicted <- rb2021Analyized$subsetDataWithOutliers %>%
  mutate(predictedValue = predict(rb2021Analyized$noOutliersModelList$noOutliersModel, newdata = .)) 

# #get usgs values
# new_usgs_values <- data.frame(USGSGageHeightFt = rb2021Outliers$USGSGageHeightFt) 
# 
# # Use the model to predict your site's height
# predicted_gage_heights <- predict(rb2021Analyized$noOutliersModelList$noOutliersModel, newdata = new_usgs_values)
# new_usgs_values$PredictedGageheight <- predicted_gage_heights
# predictedVals <- new_usgs_values


# red barn 2022 -----------------------------------------------------------
rbOnly2022 <- rbOnly %>%
  filter(year(dateTime) == 2022, 
         Water_Level_NoIce_ft > 0,
         !is.na(gageDif))

rb2022Analyized <- pressureTransducerQAQCFunction(rbOnly2022)

rb2022Analyized$plotIwthModels
rb2022WithOutliers <- rb2022Analyized$subsetDataWithOutliers

aprilGrouping <- rb2022WithOutliers %>%
  filter(predictedValueDif < -.15)
#date range of april correction
# 2022-04-02 00:00:00
# 2022-04-21 08:00:00
aprilGrouping2 <- rb2022WithOutliers %>%
  filter(dateTime >= "2022-04-02 00:00:00" & dateTime <= "2022-04-21 08:00:00") 

#avg predicted - actual difference 
#this is our constant 
mean(aprilGrouping2$predictedValueDif)
