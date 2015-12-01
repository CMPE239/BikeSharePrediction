#################################################################################
############ Prediction Algorithm III: Gradient Boosting Machine ################
#################################################################################
require(caret)
require(klaR)
require(gbm)

args <- commandArgs(trailingOnly = TRUE)
zip.code = as.factor(args[1])
start.terminal = as.factor(args[2])
end.terminal = as.factor(args[3])
event = as.factor(args[4])
subscriber.type = as.factor(args[5])
input.date.time <- paste(args[6], args[7], sep=" ")
input.date.time <- as.POSIXct(input.date.time)


#Step 1: Use subset of processed dataframe (contains datasets for top 6 Start.Terminals)
load("C:/Users/Rii/Documents/FinalImage.Rdata")
BikeShare.Data.Final.gbm <- BikeShare.Data.Final

#Step 2: Divide data into Train and Test
bikeshare.gbm.indexes <- sample(1:nrow(BikeShare.Data.Final.gbm), 
                                0.75*nrow(BikeShare.Data.Final.gbm), replace=F)

#Train
bikeshare.gbm.train <- BikeShare.Data.Final.gbm[bikeshare.gbm.indexes, ]
#Test
bikeshare.gbm.test <- BikeShare.Data.Final.gbm[-bikeshare.gbm.indexes, ]


#Step 3: Build model
bikeshare.gbm.model <- gbm(Bikes.Available.Per.Hour ~ 
                             Zip.Code + Start.Terminal +
                             End.Terminal+ Start.Date.Hour +
                             Subscription.Type + 
                             Events, 
                           bikeshare.gbm.train, 
                           distribution = "poisson", 
                           n.trees = 1000, 
                           bag.fraction = 0.75)

#Step 4: Predict
bikeshare.gbm.predict <- predict(bikeshare.gbm.model, newdata = bikeshare.gbm.test, n.trees = 1000)

#Step 5: MSE
#sum((bikeshare.gbm.predict - bikeshare.gbm.test$Bikes.Available.Per.Hour)^2) / nrow(bikeshare.gbm.test)

#Step 6: RMSE
rmse <-sqrt(mean((bikeshare.gbm.predict - bikeshare.gbm.test$Bikes.Available.Per.Hour)^2, na.rm = TRUE))
print(rmse)
