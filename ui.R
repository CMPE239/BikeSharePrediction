library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("CMPE 239 - BikeShare Prediction"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("Algo", "Choose an Algorithm:",
                  choices = c("Multiple Regression", "Random Forest")),
      textInput("zip", "Zip Code:", "94107"),
      textInput("start", "Start Terminal:", "61"),
      textInput("sub", "Subscription Type:", "Customer"),
      textInput("event", "Event:", "Fog"),
      textInput("end", "End Terminal:", "31"),
      textInput("date", "Date:", "2015-11-29"),
      textInput("time", "Time:", "15:00:00"),
      actionButton("predict", "Predict")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("response")
      
    )
  )
))
