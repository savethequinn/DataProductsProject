#ui.R

library(shiny)
library(data.table)
library(lubridate)
library(plotly)
library(ggplot2)

# Define UI for application
ui = fluidPage(
  
  # Application title
  titlePanel("Online Influence on Store Sales"),
  
  hr(),
  
  h4("Application Decription:"),
  
  h5("This application attributes in-store sales made on a particular date to digital marketing tactics within a specified number of days of online browsing activity prior to the purchase.
     The goal is to provide weighted credit for each in-store purchase based on the customers' prior online browsing behavior."),
  
  hr(),
  
  h4("Attribution Methodology:"),
  
  h5("The in-store sales and online traffic activity must be identified to the same customer to be considered an influenced purchase. The sales are distributed
     based on the unique browsing sessions identified for each customer. Given the identified store purchase and prior online activity, each tactic
     is attributed it's proportion of the in-store sales amount."),
  
  h5("For example, selecting a 10-day look-back window will proportionally attribute the in-store sales made on 4/13/2018 to the customers' browsing activity from 4/3/2018 to 4/12/2018."),
  
  
  hr(),
  
  h4("How to use the Online Influence Application:"),
  
  h5("Use the slider below to select the number of look-back days. The pie chart will update accordingly to re-attribute the sales based on the
     online browsing activity within the specified look-back window. Hover over the pie chart to see the total sales amounts attributed to each tactic."),
  
  hr(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("lookback_window",
                  "Number of lookback days:",
                  min = 1,
                  max = 10,
                  value = 5,
                  animate = FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plotly_plot")
    )
  ),
  
  hr(),
  
  h4("Application Info:"),
  
  h5("Author: Justin Owens"),
  
  h5("Version: 1.0"),
  
  h5("Create Date: 4/13/2018"),
  
  h5("Disclaimer: Underlying data were created manually for the purposes of this project and are not reflective of any actual customer browsing and purchase data."),
  
  hr()
  )