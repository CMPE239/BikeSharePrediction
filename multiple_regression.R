require(caret)
require(klaR)

args <- commandArgs(trailingOnly = TRUE)
zip.code = as.factor(args[1])
start.terminal = as.factor(args[2])
subscriber.type = as.factor(args[3])
input.date.time <- paste(args[4], args[5], sep=" ")
input.date.time <- as.POSIXct(input.date.time)

##step1 : Load the .Rdata file
load("C:/Users/Rii/Documents/FinalImage.Rdata")

##step2: we convert to a factor to indicate that it should be treated as a categorical variable.
BikeShare.Data.Final$Zip.Code <- factor(BikeShare.Data.Final$Zip.Code)
BikeShare.Data.Final$Start.Terminal <- factor(BikeShare.Data.Final$Start.Terminal)
BikeShare.Data.Final$Subscription.Type <- factor(BikeShare.Data.Final$Subscription.Type)

##step3: split the data to train - 95% and test 5%
splitIndex <- createDataPartition(BikeShare.Data.Final$Bikes.Available.Per.Hour, p=0.95, list=FALSE)
data_train <- BikeShare.Data.Final[ splitIndex,]
data_test <- BikeShare.Data.Final[-splitIndex,]

##step4 : Train the model (multiple regression) using train data i.e. 95% of actual data frame
multiple.reg.model <- lm(Bikes.Available.Per.Hour ~ Zip.Code + Start.Terminal + Subscription.Type + Start.Date.Hour, data = data_train )

##step5 : Create new test data frame as per user input to predict bike availability
##new data containing all rows from data_test with specified columns only
predict.data <- data.frame(Zip.Code = zip.code, Start.Terminal = start.terminal, Subscription.Type= subscriber.type, Start.Date.Hour = input.date.time)
predict.data$Start.Date.Hour <- as.POSIXct(predict.data$Start.Date.Hour, format = "%Y-%m-%d %H:%M:%S")


##step6: convert the new partition.data zip code and start terminal to factor and input.date.time to POSIXCT
predict.data$Zip.Code <- factor(predict.data$Zip.Code)
predict.data$Start.Terminal <- factor(predict.data$Start.Terminal)
predict.data$Start.Date.Hour <- as.POSIXct(predict.data$Start.Date.Hour)

##step7: predict the mymodel with predict.data and add a column probability that shows the number of bike availibility
predict.data$availability <- predict(multiple.reg.model, newdata = predict.data, type = "response")


##step8: Print the bike availabilty
cat("The predicted bike availability is :", floor(predict.data$availability), "\n")


##step9: Calculate RMSE to see the root mean square error 
rmse <- sqrt(mean((predict.data$availability- data_test$Bikes.Available.Per.Hour)^2, na.rm = TRUE)) 
cat("The RMSE is :", rmse, "\n")


##Step10: calculate and print R square value 
cat("R Squared value is :", summary(multiple.reg.model)$r.squared, "\n")


