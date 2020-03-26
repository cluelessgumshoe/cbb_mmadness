
#----------#
# Modeling #
#----------#
#Time frame is last ten years (not including current year (2020))
#Included only teams haveing been to tourey within time frame

#-------#   
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)
# Load packages
p_load(tidyverse,lubridate,purrr,feather,ggplot2, corrplot,DataExplorer)
cbb_clean_c <- feather::read_feather(cbb_team_c,"data/cbb_team_c.feather")
#-------------------#
# exploratory       #
#-------------------#
#goal: predict the highest round 
cbb_team_c %>% DataExplorer::plot_missing()

#-------------------#
# prep for modeling #
#-------------------#
#goal: predict the highest round 
p_load(caret,RANN)
cbb_team_pdf <- cbb_team_c %>% 
  #replace missing data with average for the given team
  group_by(school) %>%
  mutate(orb = case_when(is.na(orb) ~ mean(orb, na.rm = TRUE), TRUE ~ orb),
         orb = case_when(is.na(orb) ~ mean(orb, na.rm = TRUE), TRUE ~ orb),
         conf_l = case_when(is.na(conf_l) ~ mean(conf_l, na.rm = TRUE), TRUE ~ conf_l),
         conf_w = case_when(is.na(conf_w) ~ mean(conf_w, na.rm = TRUE), TRUE ~ conf_w),
         sos = case_when(is.na(sos) ~ mean(sos, na.rm = TRUE), TRUE ~ sos),
         srs = case_when(is.na(srs) ~ mean(srs, na.rm = TRUE), TRUE ~ srs),
         mp = case_when(is.na(mp) ~ mean(mp, na.rm = TRUE), TRUE ~ mp),
         #make row_names
         team_yr = paste0(year,"-",teamid)) %>%
  ungroup() %>% 
  #select(-school,-row_num,-year,-tour) %>%
  distinct() %>%
  column_to_rownames("team_yr")
#check that all missing are gone and see the correlations
cbb_team_pdf %>% DataExplorer::plot_missing()
#there are few few variables here that could be removed, but will leave in for now
cbb_team_pdf %>% as.matrix() %>% cor() %>% corrplot::corrplot()

#create training and test samples
cbb_p_train <- createDataPartition(cbb_team_pdf$result ,p=0.7,list = F)
cbb_train <- cbb_team_pdf[cbb_p_train,]
cbb_test <- cbb_team_pdf[-cbb_p_train,]

#-----------#
#  modeling #
#-----------#

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,savePredictions = TRUE)

#KNN Model
cbb_knn <- caret::train(
  result ~ .,
  data = cbb_train,
  method = "knn",
  preProcess = c("center","scale"),
  trControl = ctrl
)
saveRDS(cbb_knn,"cbb_knn.rds")

cbb_knn
pred_knn = predict(cbb_knn,newdata = cbb_test)
pred_knn_accuracy = round(mean(pred_knn == cbb_test$result)*100,2)
mean(mean(pred_knn != cbb_train$result))

ggplot(data=cbb_knn$results, aes(x=k, y=RMSE)) +
  geom_line() +
  geom_point()


#NN model
cbb_nn <- caret::train(
  result ~ .,
  data = cbb_train,
  method = "nnet",
  preProcess = c("center","scale"),
  trControl = ctrl,
  trace=F
)
saveRDS(cbb_nn,"cbb_nn.rds")


cbb_brnn <- caret::train(
  result ~ .,
  data = cbb_train,
  method = "brnn",
  preProcess = c("center","scale"),
  trControl = ctrl
)
saveRDS(cbb_brnn,"cbb_brnn.rds")

#compare models
results <- resamples(list(KNN=cbb_knn, NN=cbb_nn, BRNN=cbb_brnn))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)

#------------------------#   
# Predictions and Rounds #
#------------------------#
test_preds <- predict(cbb_brnn,newdata = cbb_test,type = "raw")


