##################################################################################################
####### Data Clean-Up Part I - Basic Cleanup Operation (Merging of CSVs, Join Operations #########
#######         Removal of irrelevant columns, changing format of columns)               #########
##################################################################################################

#load csv files 
#load status data (contains bike availability status per minute at every doc) 
statusdata1 <- read.csv(file.choose(), header = T)
statusdata2 <- read.csv(file.choose(), header = T)
statusdata3 <- read.csv(file.choose(), header = T)

#StatusData CSV1
#Convert statusdata1$station_id from factor to integer
statusdata1$station_id <- factor(statusdata1$station_id)
#Convert statusdata1$time from factor to POSIXct
statusdata1$time <- strptime(x = as.character(statusdata1$time),format = "%Y/%m/%d %H:%M:%S")
statusdata1$time <- as.POSIXct(statusdata1$time, format = "%Y-%m-%d %H:%M:%S")
#Round-off to minutes
statusdata1$time <- trunc(statusdata1$time, "mins")

#StatusData CSV2
#Convert statusdata2$station_id from factor to integer
statusdata2$station_id <- factor(statusdata2$station_id)
#Convert statusdata2$time from factor to POSIXct
statusdata2$time <- strptime(x = as.character(statusdata2$time),format = "%Y-%m-%d %H:%M:%S")
statusdata2$time <- as.POSIXct(statusdata2$time, format = "%Y-%m-%d %H:%M:%S")
#Round-off to minutes
statusdata2$time <- trunc(statusdata2$time, "mins")

#StatusData CSV3
#Convert statusdata3$station_id from factor to integer
statusdata3$station_id <- factor(statusdata3$station_id)
#Convert statusdata3$time from factor to POSIXct
statusdata3$time <- strptime(x = as.character(statusdata3$time),format = "%Y-%m-%d %H:%M:%S")
statusdata3$time <- as.POSIXct(statusdata3$time, format = "%Y-%m-%d %H:%M:%S")
#Round-off to minutes
statusdata2015$time <- trunc(statusdata2015$time, "mins")
statusdata2014$time <- trunc(statusdata2014$time, "mins")

#Combine status data CSV1 and CSV2 to statusdata2014 
statusdata2014 <- rbind(statusdata1, statusdata2)

#Rename statusdata3 to statusdata2015
statusdata2015 <- statusdata3
rm(statusdata3)

#load trip data (contains bike availability status per minute at every doc) 
tripdata1 <- read.csv(file.choose(), header = T)
tripdata2 <- read.csv(file.choose(), header = T)
tripdata3 <- read.csv(file.choose(), header = T)

#Trip Data CSV1
#Change Tripdata1$Start.Date from factor to POSIXct
tripdata1$Start.Date <- strptime(x = as.character(tripdata1$Start.Date),format = "%m/%d/%Y %H:%M")
tripdata1$Start.Date <- as.POSIXct(tripdata1$Start.Date, format = "%Y-%m-%d %H:%M:%S")

#Change TripData1$Start.Terminal from Integer to factor 
tripdata1$Start.Terminal <- as.factor(tripdata1$Start.Terminal)

#Change TripData1$End.Date from factor to POSIXct
tripdata1$End.Date <- strptime(x = as.character(tripdata1$End.Date),format = "%m/%d/%Y %H:%M")
tripdata1$End.Date <- as.POSIXct(tripdata1$End.Date, format = "%Y-%m-%d %H:%M:%S")

#Change TripData1$End.Station from Integer to factor 
tripdata1$End.Station <- as.factor(tripdata1$End.Station)

#Change TripData1$End.Terminal from Integer to factor 
tripdata1$End.Terminal <- as.factor(tripdata1$End.Terminal)

#Trip Data CSV2
#Change Tripdata2$Start.Date from factor to POSIXct
tripdata2$Start.Date <- strptime(x = as.character(tripdata2$Start.Date),format = "%m/%d/%y %H:%M")
tripdata2$Start.Date <- as.POSIXct(tripdata2$Start.Date, format = "%Y-%m-%d %H:%M:%S")

#Change TripData2$Start.Terminal from Integer to factor 
tripdata2$Start.Terminal <- as.factor(tripdata2$Start.Terminal)

#Change TripData2$End.Date from factor to POSIXct
tripdata2$End.Date <- strptime(x = as.character(tripdata2$End.Date),format = "%m/%d/%y %H:%M")
tripdata2$End.Date <- as.POSIXct(tripdata2$End.Date, format = "%Y-%m-%d %H:%M:%S")

#Change TripData2$End.Station from Integer to factor 
tripdata2$End.Station <- as.factor(tripdata2$End.Station)

#Change TripData2$End.Terminal from Integer to factor 
tripdata2$End.Terminal <- as.factor(tripdata2$End.Terminal)

#Bind TripData CSV1 and TripData CSV2 to tripdata2014
tripdata2014 <- rbind(tripdata1,tripdata2)

#Trip Data CSV3
#Change Tripdata3$Start.Date from factor to POSIXct
tripdata3$Start.Date <- strptime(x = as.character(tripdata3$Start.Date),format = "%m/%d/%y %H:%M")
tripdata3$Start.Date <- as.POSIXct(tripdata3$Start.Date, format = "%Y-%m-%d %H:%M:%S")

#Change TripData3$Start.Terminal from Integer to factor 
tripdata3$Start.Terminal <- as.factor(tripdata3$Start.Terminal)

#Change TripData3$End.Date from factor to POSIXct
tripdata3$End.Date <- strptime(x = as.character(tripdata3$End.Date),format = "%m/%d/%y %H:%M")
tripdata3$End.Date <- as.POSIXct(tripdata3$End.Date, format = "%Y-%m-%d %H:%M:%S")

#Change TripData3$End.Station from Integer to factor 
tripdata3$End.Station <- as.factor(tripdata3$End.Station)

#Change TripData3$End.Terminal from Integer to factor 
tripdata3$End.Terminal <- as.factor(tripdata3$End.Terminal)

#Rename tripdata3 to tripdata2015
tripdata2015 <- tripdata3
rm(tripdata3)

weatherdata1 <- read.csv(file.choose(), header = T)
weatherdata2 <- read.csv(file.choose(), header = T)
weatherdata3 <- read.csv(file.choose(), header = T)

#Weather Data CSV1
#Change weatherdata1$zip from int to factor
weatherdata1$Zip <- as.factor(weatherdata1$Zip)

#Change weatherdata1$PDT from factor to POSIXcT
weatherdata1$PDT <- strptime(x = as.character(weatherdata1$PDT),format = "%m/%d/%y")
weatherdata1$PDT <- as.POSIXct(weatherdata1$PDT, format = "%Y-%m-%d")

#Weather Data CSV2
#Change weatherdata2$zip from int to factor
weatherdata2$Zip <- as.factor(weatherdata2$Zip)

#Change weatherdata2$PDT from factor to POSIXcT
weatherdata2$PDT <- strptime(x = as.character(weatherdata2$PDT),format = "%m/%d/%Y")
weatherdata2$PDT <- as.POSIXct(weatherdata2$PDT, format = "%Y-%m-%d")

#Weather Data CSV3
#Change weatherdata3$zip from int to factor
weatherdata3$Zip <- as.factor(weatherdata3$Zip)

#Change weatherdata3$PDT from factor to POSIXcT
weatherdata3$PDT <- strptime(x = as.character(weatherdata3$PDT),format = "%m/%d/%Y")
weatherdata3$PDT <- as.POSIXct(weatherdata3$PDT, format = "%Y-%m-%d")

#Bind weatherdata1 CSV1 and weatherdata2 CSV2 to weatherdata2014
weatherdata2014 <- rbind(weatherdata1,weatherdata2)

#Rename weatherdata3 to weatherdata2015
weatherdata2015 <- weatherdata3
rm(weatherdata3)

#merge trip and status dataframes on Start.Terminal and Start.Date features
tripstatus2014 <- merge(tripdata2014, statusdata2014, by.x = c("Start.Terminal","Start.Date"), by.y = c("station_id","time"), all.x = TRUE)
tripstatus2015 <- merge(tripdata2015, statusdata2015, by.x = c("Start.Terminal","Start.Date"), by.y = c("station_id","time"), all.x = TRUE)

#Omit NA values in merged dataframes above
tripstatus2014 <- na.omit(tripstatus2014)
tripstatus2015 <- na.omit(tripstatus2015)

#Merge Weather Data with TripStatus Dataframes
#Step 1: Extract Start.Date column from tripstatus
Trip_Date <- trunc(tripstatus2014$Start.Date, "days")
tripstatus2014 <- cbind(tripstatus2014, Trip_Date)
Trip_Date <- trunc(tripstatus2015$Start.Date, "days")
tripstatus2015 <- cbind(tripstatus2015, Trip_Date)

#Step 2: Combine tripstatus2014 and tripstatus2015 to tripstatus2014.2015
tripstatus2014.2015 <- rbind(tripstatus2014, tripstatus2015)

#Step 3: Combine Weather2014 and Weather2015 to weather2014.2015 
weatherdata2014.2015 <- rbind(weatherdata2014, weatherdata2015)

#Step 4: Merge tripstatus2014.2015 and weather2014.2015 on Trip_Date and ZipCode
BikeShare.Data.2014.2015 <- merge(x = tripstatus2014.2015,y = weatherdata2014.2015, by.x = c("Trip_Date","Zip.Code"), by.y = c("PDT","Zip"), all.x = TRUE)

#Step 5: Remove unwanted columns
BikeShare.Data.2014.2015 <- subset(BikeShare.Data.2014.2015, select = -c(14:34))
BikeShare.Data.2014.2015 <- subset(BikeShare.Data.2014.2015, select = -c(15))
BikeShare.Data.2014.2015 <- subset(BikeShare.Data.2014.2015, select = -c(5))
BikeShare.Data.2014.2015 <- subset(BikeShare.Data.2014.2015, select = -c(10))

#Step 6: Clean BikeShare.Data.2014.2015$Event column -> Replace NAs by 'No Event'
levels(BikeShare.Data.2014.2015$Events) <- c(levels(BikeShare.Data.2014.2015$Events), "No Event")
BikeShare.Data.2014.2015$Events[BikeShare.Data.2014.2015$Events == ''] <- 'No Event'
BikeShare.Data.2014.2015$Events <- factor(BikeShare.Data.2014.2015$Events)
BikeShare.Data.2014.2015$Events[BikeShare.Data.2014.2015$Events=='rain'] <- 'Rain'
BikeShare.Data.2014.2015$Events <- factor(BikeShare.Data.2014.2015$Events)
BikeShare.Data.2014.2015 <- na.omit(BikeShare.Data.2014.2015)
BikeShare.Data.2014.2015$Zip.Code <- factor(BikeShare.Data.2014.2015$Zip.Code)


##################################################################################
####### Data Clean-up Part II - Calculating Availability of Bikes Per Hour #######
##################################################################################

#Step 1: Extracting Date Hour from column Start.Date to Start.Date.Hour
Start.Date.Hour <- as.POSIXct(strftime(BikeShare.Data.2014.2015$Start.Date, 
                                       format="%Y-%m-%d %H"), format="%Y-%m-%d %H")

#Step 2: Adding Start.Date.Hour column to bikeshare.subset.rf.data.cleaned dataframe 
BikeShare.Data.2014.2015 <- cbind(BikeShare.Data.2014.2015, Start.Date.Hour)

#Step 3: Getting max bike availability for each hour of the day
Bike.Available.Per.Hour.of.Day <- aggregate(BikeShare.Data.2014.2015$bikes_available, 
    by = list(BikeShare.Data.2014.2015$Start.Terminal,BikeShare.Data.2014.2015$Start.Date.Hour), 
    max)

BikeShare.Data.Final <- merge(BikeShare.Data.2014.2015, 
                                                  Bike.Available.Per.Hour.of.Day, 
                                                  by.x = c("Start.Terminal","Start.Date.Hour"), 
                                                  by.y = c("Group.1","Group.2"), 
                                                  all.x = TRUE)

#Step 4: Rename 'x' column appended in the previous step to 'Bikes.Available.Per.Hour'
names(BikeShare.Data.Final)[names(BikeShare.Data.Final)=="x"] <- "Bikes.Available.Per.Hour"

#Step 5: Remove columns Trip_Date, Start.Date, Duration, bikes_available, End.Date
BikeShare.Data.Final <- subset(BikeShare.Data.Final, select = -c(Trip_Date,
                                                                  Start.Date,
                                                                  Duration,
                                                                  bikes_available, 
                                                                  End.Date))

#################################################################################
###################### Prediction Algorithm I: Multpile Regression###############
#################################################################################
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



#################################################################################
###################### Prediction Algorithm II: Random Forest ###################
#################################################################################

#Step 1: Extract top 6 Start Terminal data rows
bikeshare.top.6.startterminals <- c("69","61","65","64","70","62")
BikeShare.Data.Final.Subset.RF <- BikeShare.Data.Final[BikeShare.Data.Final$Start.Terminal 
                                                 %in% 
                                                   bikeshare.top.6.startterminals, ]

#Step 2: Re-factor columns with more than 53 categories so that Random Forest can be used
BikeShare.Data.Final.Subset.RF$Start.Terminal <- factor(BikeShare.Data.Final.Subset.RF$Start.Terminal)
BikeShare.Data.Final.Subset.RF$Start.Station <- factor(BikeShare.Data.Final.Subset.RF$Start.Station)
BikeShare.Data.Final.Subset.RF$End.Station <- factor(BikeShare.Data.Final.Subset.RF$End.Station)
BikeShare.Data.Final.Subset.RF$End.Terminal <- factor(BikeShare.Data.Final.Subset.RF$End.Terminal)


#Step 3: Divide data into train and test dataframes
bikeshare.rf.indexes <- sample(1:nrow(BikeShare.Data.Final.Subset.RF), 
                               0.75*nrow(BikeShare.Data.Final.Subset.RF), replace=F)

#Train
bikeshare.rf.train <- BikeShare.Data.Final.Subset.RF[bikeshare.rf.indexes, ]
#Test
bikeshare.rf.test <- BikeShare.Data.Final.Subset.RF[-bikeshare.rf.indexes, ]

#Step 4: Build RF model
require(randomForest)
bikeshare.rf.model <- randomForest(Bikes.Available.Per.Hour ~ ., 
                                                             data = bikeshare.rf.train, 
                                                             ntree = 501)

#Step 5: Predict Values using Test Data
bikeshare.rf.pred <- predict(bikeshare.rf.model, newdata = bikeshare.rf.test)

#Step 6: MSE:
sum((bikeshare.rf.pred - bikeshare.rf.test$Bikes.Available.Per.Hour)^2) / nrow(bikeshare.rf.test)
#[1] 20.49726

#Step 7: RMSE:
sqrt(mean((bikeshare.rf.pred - bikeshare.rf.test$Bikes.Available.Per.Hour)^2, na.rm = TRUE)) 
#[1] 4.52739


#################################################################################
############ Prediction Algorithm III: Gradient Boosting Machine ################
#################################################################################

#Step 1: Use subset of processed dataframe (contains datasets for top 6 Start.Terminals)   
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
                             Zip.Code + 
                             Subscription.Type + 
                             Events, 
                           bikeshare.gbm.train, 
                           distribution = "poisson", 
                           n.trees = 1000, 
                           bag.fraction = 0.75)

#Step 4: Predict
bikeshare.gbm.predict <- predict(bikeshare.gbm.model, newdata = bikeshare.gbm.test, n.trees = 1000)

#Step 5: MSE
sum((bikeshare.gbm.predict - bikeshare.gbm.test$Bikes.Available.Per.Hour)^2) / nrow(bikeshare.gbm.test)

#Step 6: RMSE
sqrt(mean((bikeshare.gbm.predict - bikeshare.gbm.test$Bikes.Available.Per.Hour)^2, na.rm = TRUE))

