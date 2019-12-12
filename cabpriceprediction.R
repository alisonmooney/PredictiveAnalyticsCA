library(ggplot2)
library(dummies)
library(knitr)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(bit64)
library(lubridate)
library(sqldf)
library(glmnet)
library(mgcv)

#Import data
cab_rides_df <- read.csv('cab_rides.csv')
weather_df <- read.csv('weather.csv')
#Temperature column
colnames(weather_df)[1] <- "temp"
#PREPROCESSING
#Converting timestamps
cab_rides_df$timestamp <- as.POSIXct(cab_rides_df$time_stamp/1000,origin='1970-01-01',tz='America/New_York')
cab_rides_df$date <- as.Date(cab_rides_df$timestamp)
cab_rides_df$time <- format(cab_rides_df$timestamp,"%H:%M:%S")
cab_rides_df$weekday <- weekdays(cab_rides_df$date)
weather_df$timestamp<-as.POSIXct(weather_df$time_stamp,origin='1970-01-01',tz='America/New_York')
weather_df$date<-as.Date(weather_df$timestamp)
weather_df$time <- format(weather_df$timestamp,"%H:%M:%S")
weather_df$hour<-hour(hms(weather_df$time))
cab_rides_df$hour<-hour(hms(cab_rides_df$time))
#Add merge key to join two data frames
weather_df$merge<-paste(weather_df$location,weather_df$date,weather_df$hour)
cab_rides_df$merge<-paste(cab_rides_df$source,cab_rides_df$date,cab_rides_df$hour)
#Average the weather data per ID
weatherDF <- sqldf("select AVG(temp) as temp_avg,AVG(clouds) as clouds_avg,AVG(pressure) as pressure_avg,AVG(rain) as rain_avg,AVG(humidity) as humidity_avg,AVG(wind) as wind_avg,merge from weather_df group by merge")
merged_df <- merge(cab_rides_df,weatherDF,by='merge')
#Remove unnecessary columns 
merged_df <- merged_df %>% select (-merge,-time_stamp,-timestamp)
#Investigate NAs
sapply(merged_df, function(x) sum(is.na(x))/length(x)*100)
#Remove rain as >80% are NAs
merged_df <- merged_df %>% select(-rain_avg)
#Split into Uber and Lyft
uber_df <- merged_df %>% filter(cab_type == "Uber")
lyft_df <- merged_df %>% filter(cab_type == "Lyft")
#See where the NAs are 
sum(is.na(uber_df$price))/length(uber_df$price)*100
sum(is.na(lyft_df$price))/length(lyft_df$price)*100
#Investigate NAs
uber_price_nas <- uber_df %>% filter(is.na(uber_df$price))
p1 <- ggplot(data=uber_price_nas, aes(x=source, fill = source)) +
  geom_bar(aes(y = ..count..)) + 
  ylab("source NA frequencies") + 
  theme(legend.position = "none") 
p2 <- ggplot(data=uber_price_nas, aes(x=destination, fill = destination)) +
  geom_bar(aes(y = ..count..)) + 
  ylab("destination NA frequencies") + 
  theme(legend.position = "none")
p3 <- ggplot(data=uber_price_nas, aes(x=weekday, fill = weekday)) +
  geom_bar(aes(y = ..count..)) + 
  ylab("weekday NA frequencies") + 
  theme(legend.position = "none") 
p4 <- ggplot(data=uber_price_nas, aes(x=hour, fill = hour)) +
  geom_bar(aes(y = ..count..)) + 
  ylab("hour NA frequencies") + 
  theme(legend.position = "none")
p5 <- ggplot(data=uber_price_nas, aes(x=distance, fill = distance)) +
  geom_bar(aes(y = ..count..)) + 
  ylab("distance NA frequencies") + 
  theme(legend.position = "none")
grid.arrange(p1, p2, p3,p4,p5, nrow = 3)
#Remove price NAs
uber_df <- uber_df %>% filter(!is.na(price))
#Examine price
print(paste("Minimum Uber Price:",min(uber_df$price)))
print(paste("Maximum Uber Price:",max(uber_df$price)))
print(paste("Mean Uber Price:",mean(uber_df$price)))
print(paste("Minimum Lyft Price:",min(lyft_df$price)))
print(paste("Maximum Lyft Price:",max(lyft_df$price)))
print(paste("Mean Lyft Price:",mean(lyft_df$price)))
#Remove price NAs
merged_df_nona <- merged_df %>% filter(!is.na(price))
#Uber and Lyft relative frequencies
ggplot(data=merged_df[!is.na(merged_df$cab_type),], aes(x=cab_type, fill = cab_type)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent, limits = c(0,0.7)) +
  ylab("relative frequencies") + 
  theme(legend.position = "none")
#Frequency of prices for Uber and Lyft
ggplot(merged_df_nona, aes(x=price, color=cab_type)) +
  geom_histogram(fill="white", alpha=0.3, position="identity")
#Encoding features
Vars <- colnames(merged_df)
numericVars <- which(sapply(uber_df, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(uber_df, is.factor)) #index vector factor variables
characterVars <- which(sapply(uber_df, is.character)) #index vector character variables
dateVars <- which(sapply(uber_df, is.Date)) #index vector date variables
cat('There are', length(Vars), 'total variables,',length(numericVars), 'numeric variables,', length(factorVars), 'categorical variables,', length(characterVars), 'character variables, and', length(dateVars), 'date variables.')
uber_df <- uber_df %>% select(-(time))
lyft_df <- lyft_df %>% select(-(time))
uber_df <- uber_df %>% select(-(date))
lyft_df <- lyft_df %>% select(-(date))
uber_df$weekday<- factor(uber_df$weekday, levels = c("Monday", "Tuesday", "Wednesday", 
                                                     "Thursday", "Friday", "Saturday", "Sunday"),ordered = TRUE)
uber_df$weekday <- dummy(uber_df$weekday)
lyft_df$weekday<- factor(lyft_df$weekday, levels = c("Monday", "Tuesday", "Wednesday", 
                                                     "Thursday", "Friday", "Saturday", "Sunday"),ordered = TRUE)
lyft_df$weekday <- dummy(lyft_df$weekday)
#Surge multiplier only applicable for Lyfts
unique(lyft_df$surge_multiplier)
unique(uber_df$surge_multiplier)
ggplot(lyft_df) +
  geom_bar(aes(x = surge_multiplier))
#Normalise prices and remove surge multiplier
lyft_df$price <- lyft_df$price/lyft_df$surge_multiplier
lyft_df <- lyft_df %>% select(-(surge_multiplier))
uber_df <- uber_df %>% select(-(surge_multiplier))
#Transform categorical features to numerics
uber_df$destination <- dummy(uber_df$destination)
uber_df$source <- dummy(uber_df$source)
uber_df$name <- dummy(uber_df$name)

lyft_df$destination <- dummy(lyft_df$destination)
lyft_df$source <- dummy(lyft_df$source)
lyft_df$name <- dummy(lyft_df$name)
#Remove unnecessary features
uber_df <- uber_df %>% select(-(id))
uber_df <- uber_df %>% select(-(product_id))
uber_df <- uber_df %>% select(-(cab_type))

lyft_df <- lyft_df %>% select(-(id))
lyft_df <- lyft_df %>% select(-(product_id))
lyft_df <- lyft_df %>% select(-(cab_type))
#Normalise features
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
uber_df$distance <- normalize(uber_df$distance)
uber_df$destination <- normalize(uber_df$destination)
uber_df$source <- normalize(uber_df$source)
uber_df$name <- normalize(uber_df$name)
uber_df$weekday <- normalize(uber_df$weekday)
uber_df$hour <- normalize(uber_df$hour)
uber_df$temp_avg <- normalize(uber_df$temp_avg)
uber_df$clouds_avg <- normalize(uber_df$clouds_avg)
uber_df$pressure_avg <- normalize(uber_df$pressure_avg)
uber_df$humidity_avg <- normalize(uber_df$humidity_avg)
uber_df$wind_avg <- normalize(uber_df$wind_avg)

lyft_df$distance <- normalize(lyft_df$distance)
lyft_df$destination <- normalize(lyft_df$destination)
lyft_df$source <- normalize(lyft_df$source)
lyft_df$name <- normalize(lyft_df$name)
lyft_df$weekday <- normalize(lyft_df$weekday)
lyft_df$hour <- normalize(lyft_df$hour)
lyft_df$temp_avg <- normalize(lyft_df$temp_avg)
lyft_df$clouds_avg <- normalize(lyft_df$clouds_avg)
lyft_df$pressure_avg <- normalize(lyft_df$pressure_avg)
lyft_df$humidity_avg <- normalize(lyft_df$humidity_avg)
lyft_df$wind_avg <- normalize(lyft_df$wind_avg)
#Correlation plot of features
numericVars <- which(sapply(uber_df, is.numeric)) #index vector numeric variables
uber_numVar <- uber_df[, numericVars]
lyft_numVar <- lyft_df[, numericVars]
cor_uber_numVar <- cor(uber_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
cor_lyft_numVar <- cor(lyft_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
cor_uber_sorted <- as.matrix(sort(cor_uber_numVar[,'price'], decreasing = TRUE))
cor_lyft_sorted <- as.matrix(sort(cor_lyft_numVar[,'price'], decreasing = TRUE))
corrplot.mixed(cor_uber_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.6,cl.cex = 0.4, number.cex=.7, title(main = "Uber"),   mar=c(0,0,1,0))
corrplot.mixed(cor_lyft_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.6,cl.cex = 0.4, number.cex=.7, title(main = "Lyft"), mar=c(0,0,1,0))
#MODELLING
#Cross validation
uber_smp_size <- floor(0.7 * nrow(uber_df)) 
#Setting the seed to make partition reproducible
set.seed(123)
uber_train_ind <- sample(seq_len(nrow(uber_df)), size = uber_smp_size)

uber_train <- uber_df[uber_train_ind, ]
uber_train_X <- uber_train %>% select(-(price))
uber_train_y <- uber_train %>% select((price))
uber_train_y <- uber_train_y$price
uber_test <- uber_df[-uber_train_ind, ]
uber_test_X <- uber_test %>% select(-(price))
uber_test_y <- uber_test %>% select((price))
uber_test_y <- uber_test_y$price

lyft_smp_size <- floor(0.7 * nrow(lyft_df)) 
#Setting the seed to make partition reproducible
set.seed(123)
lyft_train_ind <- sample(seq_len(nrow(lyft_df)), size = lyft_smp_size)

lyft_train <- lyft_df[lyft_train_ind, ]
lyft_train_X <- lyft_train %>% select(-(price))
lyft_train_y <- lyft_train %>% select((price))
lyft_train_y <- lyft_train_y$price
lyft_test <- lyft_df[-lyft_train_ind, ]
lyft_test_X <- lyft_test %>% select(-(price))
lyft_test_y <- lyft_test %>% select((price))
lyft_test_y <- lyft_test_y$price

#***************************LINEAR MODEL*****************************************
#Uber
uber_lm <- lm(price ~ ., data = uber_train)
summary(uber_lm)
plot(uber_lm, main = "Uber Linear Model") 
uber_lm_predict <- predict(uber_lm, uber_test_X)
uber_actuals_preds <- data.frame(cbind(actuals=uber_test_y, predictions=uber_lm_predict)) 
uber_r2 <- R2(uber_actuals_preds$predictions, uber_actuals_preds$actuals)
uber_rmse <- RMSE(uber_actuals_preds$predictions, uber_actuals_preds$actuals)
uber_MAE <- mean(abs(uber_actuals_preds$actuals - uber_actuals_preds$predictions))
cat('Uber Linear RMSE of the test data: ', round(uber_rmse,3),'\n')
cat('Uber Linear Model r2 of the test data: ', round(uber_r2,3), '\n')
cat("Uber Linear MAE: ", uber_MAE)
ggplot() +
  geom_point(aes(x = uber_actuals_preds$actuals, y = uber_actuals_preds$predictions,
                 colour = 'red')) +
  ggtitle('Uber Linear Regression Actual vs Prediction') +
  xlab('Actual') +
  ylab('Prediction')
#Lyft
lyft_lm <- lm(price ~ ., data = lyft_train)
summary(lyft_lm)
plot(lyft_lm, main = "Lyft Linear Model") 
lyft_lm_predict <- predict(lyft_lm, lyft_test_X)
lyft_actuals_preds <- data.frame(cbind(actuals=lyft_test_y, predictions=lyft_lm_predict))  
lyft_r2 <- R2(lyft_actuals_preds$predictions, lyft_actuals_preds$actuals)
lyft_rmse <- RMSE(lyft_actuals_preds$predictions, lyft_actuals_preds$actuals)
lyft_MAE <- mean(abs(lyft_actuals_preds$actuals - lyft_actuals_preds$predictions))
cat('Lyft Linear RMSE of the test data: ', round(lyft_rmse,3),'\n')
cat('Lyft Linear Model r2 of the test data: ', round(lyft_r2,3), '\n')
cat("Lyft Linear MAE: ", lyft_MAE)

ggplot() +
  geom_point(aes(x = lyft_actuals_preds$actuals, y = lyft_actuals_preds$predictions,
                 colour = 'red')) +
  ggtitle('Lyft Linear Regression Actual vs Prediction') +
  xlab('Actual') +
  ylab('Prediction')
#***************************LASSO MODEL*****************************************
set.seed(12345)
#Control variables and control function
my_control <-trainControl(method="cv", number=5)
#Uber
uber_lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
uber_lasso_mod <- train(x=uber_train_X, y= uber_train_y, method='glmnet', trControl= my_control, tuneGrid=uber_lassoGrid) 
uber_lasso_mod$bestTune
plot(uber_lasso_mod, main = "Uber Lasso Model Lambda") 
min(uber_lasso_mod$results$RMSE)
uber_lassoVarImp <- varImp(uber_lasso_mod,scale=F)
uber_lassoImportance <- uber_lassoVarImp$importance
ubervarsSelected <- length(which(uber_lassoImportance$Overall!=0))
ubervarsNotSelected <- length(which(uber_lassoImportance$Overall==0))
cat('Lasso uses', ubervarsSelected, 'variables in its model, and did not select', ubervarsNotSelected, 'variables.')
UberLassoPred <- predict(uber_lasso_mod, uber_test_X)
uber_LassoActualsPreds <- data.frame(cbind(actuals=uber_test_y, predictions= UberLassoPred))
uber_lasso_residuals = uber_test_y - UberLassoPred
uber_lasso_RMSE = sqrt(mean(uber_lasso_residuals^2))
uber_test_y_mean = mean(uber_test_y)
uber_tss =  sum((uber_test_y - uber_test_y_mean)^2 )
uber_lasso_rss =  sum(uber_lasso_residuals^2)
uber_lasso_rsq  =  1 - (uber_lasso_rss/uber_tss)
uber_lasso_MAE <- mean(abs(uber_LassoActualsPreds$actuals - uber_LassoActualsPreds$predictions))
cat('Uber Lasso RMSE of the test data: ', round(uber_lasso_RMSE,3),'\n')
cat('Uber Lasso Model r2 of the test data: ', round(uber_lasso_rsq,3), '\n')
cat("Uber Lasso MAE: ", uber_lasso_MAE)
ggplot() +
  geom_point(aes(x = uber_LassoActualsPreds$actuals, y = uber_LassoActualsPreds$predictions,
                 colour = 'red')) +
  ggtitle('Uber Lasso Regression Actual vs Prediction') +
  xlab('Actual') +
  ylab('Predicted')

#Lyft
set.seed(12346)
my_control <-trainControl(method="cv", number=5)
lyft_lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
lyft_lasso_mod <- train(x=lyft_train_X, y= lyft_train_y, method='glmnet', trControl= my_control, tuneGrid=lyft_lassoGrid) 
lyft_lasso_mod$bestTune
plot(lyft_lasso_mod, main = "Lyft Lasso Model Lambda") 
min(lyft_lasso_mod$results$RMSE)
lyft_lassoVarImp <- varImp(lyft_lasso_mod,scale=F)
lyft_lassoImportance <- lyft_lassoVarImp$importance
lyft_varsSelected <- length(which(lyft_lassoImportance$Overall!=0))
lyft_varsNotSelected <- length(which(lyft_lassoImportance$Overall==0))
cat('Lasso uses', lyft_varsSelected, 'variables in its model, and did not select', lyft_varsNotSelected, 'variables.')
LyftLassoPred <- predict(lyft_lasso_mod, lyft_test_X)
LyftLassoPred[1:6]
lyft_LassoActualsPreds <- data.frame(cbind(actuals=lyft_test_y, predictions= LyftLassoPred))
lyft_lasso_residuals = lyft_test_y - LyftLassoPred
lyft_lasso_RMSE = sqrt(mean(lyft_lasso_residuals^2))
lyft_test_y_mean = mean(lyft_test_y)
lyft_tss =  sum((lyft_test_y - lyft_test_y_mean)^2 )
lyft_lasso_rss =  sum(lyft_lasso_residuals^2)
lyft_lasso_rsq  =  1 - (lyft_lasso_rss/lyft_tss)
lyft_lasso_MAE <- mean(abs(lyft_LassoActualsPreds$actuals - lyft_LassoActualsPreds$predictions))
cat('Lyft Lasso RMSE of the test data: ', round(lyft_lasso_RMSE,3),'\n')
cat('Lyft Lasso Model r2 of the test data: ', round(lyft_lasso_rsq,3), '\n')
cat("Lyft Lasso MAE: ", lyft_lasso_MAE)
ggplot() +
  geom_point(aes(x = lyft_LassoActualsPreds$actuals, y = lyft_LassoActualsPreds$predictions,
                 colour = 'red')) +
  ggtitle('Lyft Lasso Regression Actual vs Prediction') +
  xlab('Actual') +
  ylab('Predicted')

#***************************STEPWISE MODEL*****************************************
#Uber
#1 : Starts off with just intercept value
uber_null_model <- glm(price ~ 1, data = uber_train,family = "gaussian")
# Specify the full model using all of the potential predictors
uber_full_model <- glm(price ~ ., data = uber_train,family = "gaussian")
# Use a forward stepwise algorithm to build a parsimonious model
uber_step_model <- step(uber_null_model, scope = list(lower = uber_null_model, upper = uber_full_model), direction = "forward")
plot(uber_step_model, main = "Uber Step Model") 
uber_stepwise_predictions <-predict(uber_step_model, uber_test_X)
uber_stepwise_r2 <- R2(uber_stepwise_predictions, uber_test_y)
uber_stepwise_rmse <- RMSE(uber_stepwise_predictions, uber_test_y)
uber_stepwise_MAE <- mean(abs(uber_stepwise_predictions - uber_test_y))
cat('The root mean square error of the test data is ', round(uber_stepwise_rmse,3),'\n')
cat('The R-square of the test data is ', round(uber_stepwise_r2,3), '\n')  
cat("Uber MAE: ", uber_stepwise_MAE)
ggplot() +
  geom_point(aes(x = uber_test_y, y = uber_stepwise_predictions,
                 colour = 'red')) +
  ggtitle('Uber Stepwise Actual vs Prediction') +
  xlab('Actual') +
  ylab('Predicted')

#Lyft
lyft_null_model <- glm(price ~ 1, data = lyft_train,family = "gaussian")
# Specify the full model using all of the potential predictors
lyft_full_model <- glm(price ~ ., data = lyft_train,family = "gaussian")
# Use a forward stepwise algorithm to build a parsimonious model
lyft_step_model <- step(lyft_null_model, scope = list(lower = lyft_null_model, upper = lyft_full_model), direction = "forward")
plot(lyft_step_model, main = "Lyft Step Model") 
lyft_stepwise_predictions <-predict(lyft_step_model, lyft_test_X)
lyft_stepwise_r2 <- R2(lyft_stepwise_predictions, lyft_test_y)
lyft_stepwise_rmse <- RMSE(lyft_stepwise_predictions, lyft_test_y)
lyft_stepwise_MAE <- mean(abs(lyft_stepwise_predictions - lyft_test_y))
cat('The root mean square error of the test data is ', round(lyft_stepwise_rmse,3),'\n')
cat('The R-square of the test data is ', round(lyft_stepwise_r2,3), '\n')  
cat("Lyft MAE: ", lyft_stepwise_MAE)
ggplot() +
  geom_point(aes(x = lyft_test_y, y = lyft_stepwise_predictions,
                 colour = 'red')) +
  ggtitle('Lyft Stepwise Actual vs Prediction') +
  xlab('Actual') +
  ylab('Predicted')

#***************************XGBOOST MODEL*****************************************
#Uber
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)
xgb_caret <- train(x=uber_train_X, y=uber_train_y, method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 
xgb_caret$bestTune
uber_dtrain <- xgb.DMatrix(data = as.matrix(uber_train_X), label= uber_train_y)
uber_dtest <- xgb.DMatrix(data = as.matrix(uber_test_X))

default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.05, #default = 0.3
  gamma=0,
  max_depth=3, #default=6
  min_child_weight=4, #default=1
  subsample=1,
  colsample_bytree=1
)

xgbcv <- xgb.cv( params = default_param, data = uber_dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)
uber_xgb_mod <- xgb.train(data = uber_dtrain, params=default_param, nrounds = 201)
uber_XGBpred <- predict(uber_xgb_mod, uber_dtest)

library(Ckmeans.1d.dp) #required for ggplot clustering
uber_mat <- xgb.importance (feature_names = colnames(uber_train_X),model = uber_xgb_mod)
xgb.ggplot.importance(importance_matrix = uber_mat[1:nrow(uber_mat)], rel_to_first = TRUE)
uber_xgb_ActualsPreds <- data.frame(cbind(actuals=uber_test_y, predictions= uber_XGBpred))

uber_residuals = uber_test_y - uber_XGBpred
uber_RMSE = sqrt(mean(uber_residuals^2))
cat('The root mean square error of the test data is ', round(uber_RMSE,3),'\n')
uber_test_y_mean = mean(uber_test_y)
# Calculate total sum of squares
uber_tss =  sum((uber_test_y - uber_test_y_mean)^2 )
# Calculate residual sum of squares
uber_rss =  sum(uber_residuals^2)
# Calculate R-squared
uber_rsq  =  1 - (uber_rss/uber_tss)
cat('The R-square of the test data is ', round(uber_rsq,3), '\n')

#Lyft
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)
xgb_caret <- train(x=lyft_train_X, y=lyft_train_y, method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 
xgb_caret$bestTune


# put our testing & training data into two seperates Dmatrixs objects
lyft_dtrain <- xgb.DMatrix(data = as.matrix(lyft_train_X), label= lyft_train_y)
lyft_dtest <- xgb.DMatrix(data = as.matrix(lyft_test_X))

lyft_default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.05, #default = 0.3
  gamma=0,
  max_depth=3, #default=6
  min_child_weight=4, #default=1
  subsample=1,
  colsample_bytree=1
)

lyft_xgbcv <- xgb.cv( params = lyft_default_param, data = lyft_dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)
lyft_xgb_mod <- xgb.train(data = lyft_dtrain, params=default_param, nrounds = 201)
lyft_XGBpred <- predict(lyft_xgb_mod, lyft_dtest)

library(Ckmeans.1d.dp) #required for ggplot clustering
lyft_mat <- xgb.importance (feature_names = colnames(lyft_train_X),model = lyft_xgb_mod)
xgb.ggplot.importance(importance_matrix = lyft_mat[1:nrow(lyft_mat)], rel_to_first = TRUE)
lyft_xgb_ActualsPreds <- data.frame(cbind(actuals=lyft_test_y, predictions= lyft_XGBpred))

residuals = lyft_test_y - lyft_XGBpred
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')
lyft_test_y_mean = mean(lyft_test_y)
tss =  sum((lyft_test_y - lyft_test_y_mean)^2 )
rss =  sum(residuals^2)
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')


                          
