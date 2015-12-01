require(caret)
require(klaR)
require(randomForest)

args <- commandArgs(trailingOnly = TRUE)
zip.code = as.factor(args[1])
start.terminal = as.factor(args[2])
end.terminal = as.factor(args[3])
event = as.factor(args[4])
subscriber.type = as.factor(args[5])
input.date.time <- paste(args[6], args[7], sep=" ")
input.date.time <- as.POSIXct(input.date.time)

##step1 : Load the .Rdata file
load("C:/Users/Rii/Documents/FinalImage.Rdata")

##step2: we convert to a factor to indicate that it should be treated as a categorical variable.
BikeShare.Data.Final.Subset.RF$Zip.Code <- factor(BikeShare.Data.Final.Subset.RF$Zip.Code)
BikeShare.Data.Final.Subset.RF$Start.Terminal <- factor(BikeShare.Data.Final.Subset.RF$Start.Terminal)
BikeShare.Data.Final.Subset.RF$Subscription.Type <- factor(BikeShare.Data.Final.Subset.RF$Subscription.Type)

#Step 1: Divide data into train and test dataframes
BikeShare.Data.Final.Subset.RF.indexes <- sample(1:nrow(BikeShare.Data.Final.Subset.RF), 0.75*nrow(BikeShare.Data.Final.Subset.RF), replace=F)
#Train
BikeShare.Data.Final.Subset.RF.train <- BikeShare.Data.Final.Subset.RF[BikeShare.Data.Final.Subset.RF.indexes, ]
#Test
BikeShare.Data.Final.Subset.RF.test <- BikeShare.Data.Final.Subset.RF[-BikeShare.Data.Final.Subset.RF.indexes, ]

#Step 7: Build RF model using -> Start.Terminal, Start.Date.Hour, Zip.Code, Start.Station, 
#        Subscription.Type, Bikes.Available.Per.Hour
BikeShare.Data.Final.Subset.RF.model <- randomForest(Bikes.Available.Per.Hour ~ ., data = BikeShare.Data.Final.Subset.RF.train, ntree = 501)

##create new test data frame for testing the randon forest model
BikeShare.Data.Final.Subset.RF.test <- rbind(BikeShare.Data.Final.Subset.RF.test,data.frame(Zip.Code = zip.code, Start.Terminal = start.terminal,End.Terminal= end.terminal,Events=event , Subscription.Type= subscriber.type, Start.Date.Hour = input.date.time,Start.Station = "2nd at Townsend", End.Station ="Market at Sansome",Bikes.Available.Per.Hour=10))
BikeShare.Data.Final.Subset.RF.test$Start.Date.Hour <- as.POSIXct(BikeShare.Data.Final.Subset.RF.test$Start.Date.Hour, format = "%Y-%m-%d %H:%M:%S")
BikeShare.Data.Final.Subset.RF.test$Start.Station <- factor(BikeShare.Data.Final.Subset.RF.test$Start.Station)
BikeShare.Data.Final.Subset.RF.test$End.Station <- factor(BikeShare.Data.Final.Subset.RF.test$End.Station)


#Step 8: Predict over test dataframe
BikeShare.Data.Final.Subset.RF.test$availability <- predict(BikeShare.Data.Final.Subset.RF.model, newdata = BikeShare.Data.Final.Subset.RF.test)
cat("The predicted bike availability is :", floor(BikeShare.Data.Final.Subset.RF.test$availability), "\n")

