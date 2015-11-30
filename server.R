library(shiny)

shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested bike availability prediction. This is 
  # called whenever the submit button is clicked.
  #data <- reactive({
  #  input$algo
  #(input$zip)
  #(input$start)
  #(input$sub)
  #(input$date)
  #(input$time)
  #})
  
  observeEvent(input$predict,{
    #using the input from UI
    print(input$Algo)
    print(input$zip)
    print(input$start)
    print(input$sub)
    print(input$date)
    print(input$time)
    require(caret)
    require(klaR)
    end.terminal = as.factor(input$end)
    start.terminal = as.factor(input$start)
    zip.code = as.factor(input$zip)
    subscriber.type = as.factor(input$sub)
    event = as.factor(input$event)
    input.date.time <- paste(input$date, input$time, sep=" ")
    input.date.time <- as.POSIXct(input.date.time)
    load("C:/Users/Rii/Documents/FinalImage.Rdata")
    
    if(input$Algo == "Multiple Regression"){
      
      ##step2: we convert to a factor to indicate that it should be treated as a categorical variable.
      BikeShare.Data.Final$Zip.Code <- factor(BikeShare.Data.Final$Zip.Code)
      BikeShare.Data.Final$Start.Terminal <- factor(BikeShare.Data.Final$Start.Terminal)
      BikeShare.Data.Final$Subscription.Type <- factor(BikeShare.Data.Final$Subscription.Type)
      
      ##step3: split the data to train - 95% and test 5%
      splitIndex <- createDataPartition(BikeShare.Data.Final$Bikes.Available.Per.Hour, p=0.95, list=FALSE)
      data_train <- BikeShare.Data.Final[ splitIndex,]
      data_test <- BikeShare.Data.Final[-splitIndex,]
      
      ##step4 : Train the model (multiple regression) using train data i.e. 95% of actual data frame
      multiple.reg.model <- lm(Bikes.Available.Per.Hour ~ Zip.Code + Start.Terminal +End.Terminal + Events + Subscription.Type+ Start.Date.Hour , data = data_train )
      
      ##step5 : Create new test data frame as per user input to predict bike availability
      ##new data containing all rows from data_test with specified columns only
      predict.data <- data.frame(Zip.Code = zip.code, Start.Terminal = start.terminal,End.Terminal= end.terminal,Events=event ,Subscription.Type= subscriber.type, Start.Date.Hour = input.date.time)
      predict.data$Start.Date.Hour <- as.POSIXct(predict.data$Start.Date.Hour, format = "%Y-%m-%d %H:%M:%S")
      
      
      ##step6: convert the new partition.data zip code and start terminal to factor and input.date.time to POSIXCT
      predict.data$Zip.Code <- factor(predict.data$Zip.Code)
      predict.data$Start.Terminal <- factor(predict.data$Start.Terminal)
      predict.data$Start.Date.Hour <- as.POSIXct(predict.data$Start.Date.Hour, format = "%Y-%m-%d %H:%M:%S")
      
      ##step7: predict the mymodel with predict.data and add a column probability that shows the number of bike availibility
      predict.data$availability <- predict(multiple.reg.model, newdata = predict.data, type = "response")
      
      
      
      #input$zip <- floor(predict.data$availability)
      output$response <- renderText({
        out <- paste("Bike availability using Multiple Regression is",ceiling(predict.data$availability), sep=" ")})
      
      
      
    } else {
      
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
      ##create new test data frame for testing the randon forest model
      BikeShare.Data.Final.Subset.RF.test <- rbind(BikeShare.Data.Final.Subset.RF.test,data.frame(Zip.Code = zip.code, Start.Terminal = start.terminal,End.Terminal= end.terminal,Events=event , Subscription.Type= subscriber.type, Start.Date.Hour = input.date.time,Start.Station = "2nd at Townsend", End.Station ="Market at Sansome",Bikes.Available.Per.Hour=10))
      BikeShare.Data.Final.Subset.RF.test$Start.Date.Hour <- as.POSIXct(BikeShare.Data.Final.Subset.RF.test$Start.Date.Hour, format = "%Y-%m-%d %H:%M:%S")
      BikeShare.Data.Final.Subset.RF.test$Start.Station <- factor(BikeShare.Data.Final.Subset.RF.test$Start.Station)
      BikeShare.Data.Final.Subset.RF.test$End.Station <- factor(BikeShare.Data.Final.Subset.RF.test$End.Station)
      
      
      #Step 8: Predict over test dataframe
      BikeShare.Data.Final.Subset.RF.test$availability <- predict(BikeShare.Data.Final.Subset.RF.model, newdata = BikeShare.Data.Final.Subset.RF.test)
      
      
      #output$response <- renderText({
      #  out <- paste("Bike availability using Random Forest is",ceiling(BikeShare.Data.Final.Subset.RF.pred$availability), sep=" ")})
    }
    
  })
  
  
  
})
