library(shiny)

shinyServer(function(input, output) {
  
  
  
  observeEvent(input$predict,{
    
    require(caret)
    require(klaR)
    require(ggplot2)
    require(randomForest)
    end.terminal = as.factor(input$end)
    start.terminal = as.factor(input$start)
    zip.code = as.factor(input$zip)
    subscriber.type = as.factor(input$sub)
    event = as.factor(input$event)
    input.date.time <- paste(input$date, input$time, sep=" ")
    input.date.time <- as.POSIXct(input.date.time)
    load("C:/Users/Rii/Documents/FinalImage.Rdata")
    
    if(input$Algo == "Multiple Regression"){
      
      print("Inside Multiple Regression Block")
      
      ##step1: we convert to a factor to indicate that it should be treated as a categorical variable.
      BikeShare.Data.Final$Zip.Code <- factor(BikeShare.Data.Final$Zip.Code)
      BikeShare.Data.Final$Start.Terminal <- factor(BikeShare.Data.Final$Start.Terminal)
      BikeShare.Data.Final$Subscription.Type <- factor(BikeShare.Data.Final$Subscription.Type)
      
      ##step2: split the data to train - 95% and test 5%
      splitIndex <- createDataPartition(BikeShare.Data.Final$Bikes.Available.Per.Hour, p=0.95, list=FALSE)
      data_train <- BikeShare.Data.Final[ splitIndex,]
      data_test <- BikeShare.Data.Final[-splitIndex,]
      
      ##step3 : Train the model (multiple regression) using train data i.e. 95% of actual data frame
      multiple.reg.model <- lm(Bikes.Available.Per.Hour ~ Zip.Code + Start.Terminal +End.Terminal + Events + Subscription.Type+ Start.Date.Hour , data = data_train )
      
      ##step4 : Create new test data frame as per user input to predict bike availability
      ##new data containing all rows from data_test with specified columns only
      predict.data <- data.frame(Zip.Code = zip.code, Start.Terminal = start.terminal,End.Terminal= end.terminal,Events=event ,Subscription.Type= subscriber.type, Start.Date.Hour = input.date.time)
      predict.data$Start.Date.Hour <- as.POSIXct(predict.data$Start.Date.Hour, format = "%Y-%m-%d %H:%M:%S")
      
      
      ##step5: convert the new partition.data zip code and start terminal to factor and input.date.time to POSIXCT
      predict.data$Zip.Code <- factor(predict.data$Zip.Code)
      predict.data$Start.Terminal <- factor(predict.data$Start.Terminal)
      predict.data$Start.Date.Hour <- as.POSIXct(predict.data$Start.Date.Hour, format = "%Y-%m-%d %H:%M:%S")
      
      ##step6: predict the mymodel with predict.data and add a column probability that shows the number of bike availibility
      predict.data$availability <- predict(multiple.reg.model, newdata = predict.data, type = "response")
      
      ##step7: Return Predicted bike availability value and related graphs
      
      #input$zip <- floor(predict.data$availability)
      output$response <- renderText({
        out <- paste("Bike availability using Multiple Regression is",ceiling(predict.data$availability), sep=" ")})
    
      output$plotdesc1 <- renderText({"Plot for Start terminal versus Bike Availability: "})
      
      output$plot1<- renderPlot({
        plot(data_test$Start.Terminal,data_test$Bikes.Available.Per.Hour)
      })
      
      output$plotdesc2 <- renderText({"Plot for Events versus Bike Availability: "})
      
      output$plot2<- renderPlot({
        plot(data_test$Events,data_test$Bikes.Available.Per.Hour)
      })
      
      output$plotdesc3 <- renderText({"Plot for Zip Code versus Bike Availability: "})
      
      output$plot3<- renderPlot({
        plot(data_test$Zip.Code,data_test$Bikes.Available.Per.Hour)
      })
      output$plotdesc4 <- renderText({"Plot for Subscription Type versus Bike Availability: "})
      
      output$plot4<- renderPlot({
        plot(data_test$Subscription.Type,data_test$Bikes.Available.Per.Hour)
      })
      
      
      
    } else if(input$Algo == "Random Forest"){
      
      print("Inside Random Forest Block")
      
      ##step1: we convert to a factor to indicate that it should be treated as a categorical variable.
      BikeShare.Data.Final.Subset.RF$Zip.Code <- factor(BikeShare.Data.Final.Subset.RF$Zip.Code)
      BikeShare.Data.Final.Subset.RF$Start.Terminal <- factor(BikeShare.Data.Final.Subset.RF$Start.Terminal)
      BikeShare.Data.Final.Subset.RF$Subscription.Type <- factor(BikeShare.Data.Final.Subset.RF$Subscription.Type)
      
      #Step 2: Divide data into train and test dataframes
      BikeShare.Data.Final.Subset.RF.indexes <- sample(1:nrow(BikeShare.Data.Final.Subset.RF), 0.75*nrow(BikeShare.Data.Final.Subset.RF), replace=F)
      #Train
      BikeShare.Data.Final.Subset.RF.train <- BikeShare.Data.Final.Subset.RF[BikeShare.Data.Final.Subset.RF.indexes, ]
      #Test
      BikeShare.Data.Final.Subset.RF.test <- BikeShare.Data.Final.Subset.RF[-BikeShare.Data.Final.Subset.RF.indexes, ]
      
      #Step 3: Build RF model using -> Start.Terminal, Start.Date.Hour, Zip.Code, Start.Station, 
      #        Subscription.Type, Bikes.Available.Per.Hour
      BikeShare.Data.Final.Subset.RF.model <- randomForest(Bikes.Available.Per.Hour ~ ., data = BikeShare.Data.Final.Subset.RF.train, ntree = 50)
      
      ##create new test data frame for testing the randon forest model
      ##create new test data frame for testing the randon forest model
      #BikeShare.Data.Final.Subset.RF.test <- rbind(BikeShare.Data.Final.Subset.RF.test,data.frame(Zip.Code = zip.code, Start.Terminal = start.terminal,End.Terminal= end.terminal,Events=event , Subscription.Type= subscriber.type, Start.Date.Hour = input.date.time,Start.Station = "2nd at Townsend", End.Station ="Market at Sansome",Bikes.Available.Per.Hour=10))
      #BikeShare.Data.Final.Subset.RF.test$Start.Date.Hour <- as.POSIXct(BikeShare.Data.Final.Subset.RF.test$Start.Date.Hour, format = "%Y-%m-%d %H:%M:%S")
      #BikeShare.Data.Final.Subset.RF.test$Start.Station <- factor(BikeShare.Data.Final.Subset.RF.test$Start.Station)
      #BikeShare.Data.Final.Subset.RF.test$End.Station <- factor(BikeShare.Data.Final.Subset.RF.test$End.Station)
      
      ##Step4: Return the Legend for Random Forest Plot in R
      output$response <- renderText({"Legend for Random Forest Plot in R: "})
      output$plot1<- renderPlot({
        plot(BikeShare.Data.Final.Subset.RF.model,log="y")
      })
      output$plotdesc1 <- renderText({" "})
      output$plotdesc2 <- renderText({" "})
      output$plotdesc4 <- renderText({" "})
      output$plotdesc3 <- renderText({" "})
      output$plot2<- renderPlot({
        plot(BikeShare.Data.Final.Subset.RF.test$Events,BikeShare.Data.Final.Subset.RF.test$Bikes.Available.Per.Hour)
      })
      
      output$plot3<- renderPlot({
        plot(BikeShare.Data.Final.Subset.RF.test$Zip.Code,BikeShare.Data.Final.Subset.RF.test$Bikes.Available.Per.Hour)
      })
      
      output$plot4<- renderPlot({
        plot(BikeShare.Data.Final.Subset.RF.test$Subscription.Type,BikeShare.Data.Final.Subset.RF.test$Bikes.Available.Per.Hour)
      })
      
      
      
    } else {
       
      print("Inside GBM block...")
      #Step 1: Use subset of processed dataframe (contains datasets for top 6 Start.Terminals)
      BikeShare.Data.Final.gbm <- BikeShare.Data.Final
      
      #Step 2: Divide data into Train and Test
      bikeshare.gbm.indexes <- sample(1:nrow(BikeShare.Data.Final.gbm), 
                                      0.95*nrow(BikeShare.Data.Final.gbm), replace=F)
      
      #Train
      bikeshare.gbm.train <- BikeShare.Data.Final.gbm[bikeshare.gbm.indexes, ]
      #Test
      bikeshare.gbm.test <- BikeShare.Data.Final.gbm[-bikeshare.gbm.indexes, ]
      
      
      #Step 3: Build model
      bikeshare.gbm.model <- gbm(Bikes.Available.Per.Hour ~ 
                                   Zip.Code + Start.Terminal +
                                   End.Terminal+
                                   Subscription.Type + 
                                   Events, 
                                 bikeshare.gbm.train, 
                                 distribution = "poisson", 
                                 n.trees = 1000, 
                                 bag.fraction = 0.75)
      
      ##create test data using inputs from the user to predict bike availability
      bikeshare.gbm.predict <- data.frame(Zip.Code = zip.code, Start.Terminal = start.terminal, Subscription.Type= subscriber.type, End.Terminal = end.terminal, Events = event)
      
      #Step 4: Predict
      bikeshare.gbm.predict$availability <- predict(bikeshare.gbm.model, newdata = bikeshare.gbm.predict, n.trees = 1000)
      output$response <- renderText({
        out <- paste("Bike availability using GBM is",ceiling(bikeshare.gbm.predict$availability), sep=" ")})
      output$plotdesc1 <- renderText({"Plot for Start terminal: "})
      
      output$plot1<- renderPlot({
        plot(bikeshare.gbm.model, 'Start.Terminal')
      })
      
      output$plotdesc2 <- renderText({"Plot for Events: "})
      
      output$plot2<- renderPlot({
        plot(bikeshare.gbm.model, 'Events')
      })
      
      output$plotdesc3 <- renderText({"Plot for Zip Code: "})
      
      output$plot3<- renderPlot({
        plot(bikeshare.gbm.model, 'Zip.Code')
      })
      output$plotdesc4 <- renderText({"Plot for Subscription Type: "})
      
      output$plot4<- renderPlot({
        plot(bikeshare.gbm.model, 'Subscription.Type')
      })
    }
    
  })
  
  
  
})
