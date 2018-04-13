#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#trace(utils:::unpackPkgZip, edit=TRUE)

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
   
   h4("Disclaimer:"),
   
   h5("Underlying data were created manually for the purposes of this project and are not reflective of any actual customer browsing and purchase data."),
   
   hr(),
   
   h5("Author: Justin Owens"),
   
   h5("Version: 1.0"),
   
   h5("Create Date: 4/13/2018"),
   
   hr()
)

# Define server logic required to draw the plot
server = function(input, output) {
   # generate graph based on input$lookback_window ins from ui.R
   output$plotly_plot <- renderPlotly({
     
      # read in the order and session data
      order_data = fread('./ord_data.txt', sep = '\t')
      order_data[,order_date:=mdy(order_date)]
      
      session_data = fread('./session_data.txt', sep = '\t')
      session_data[,date:=mdy(date)]
      
      # get the order date and calculate the requested lookback date range
      order_date = max(order_data$order_date)
      session_max_date = order_date - 1
      session_min_date = order_date - input$lookback_window
      traffic_date_list = seq.Date(from = session_min_date, to = session_max_date, by = 'day')
      
      # subset the data as dictated by the requested date range
      target_traffic_data = session_data[date %in% traffic_date_list]
      
      # count distinct session touch points for each tactic by customer from traffic data
      traffic_agg = setDT(aggregate(session_id ~ cust_id + tactic, target_traffic_data, function(x) length(unique(x))))
      # calculate total sessions for each customer
      traffic_agg[,total_cust_sessions:=.(total_session=sum(session_id)),by=cust_id]
      traffic_agg[,tactic_pct:=(session_id/total_cust_sessions)]
      
      #merge sales and traffic aggs, then calculate the attributed sales for each tactic touchpoint/session
      rpt_layer = merge(traffic_agg, order_data, by.x = 'cust_id', by.y = 'cust_id', all = TRUE)
      rpt_layer[,attr_sales:=(tactic_pct*sales)]
      
      #aggregate attributed sales to tactic
      rpt_agg = setDT(aggregate(attr_sales ~ tactic, rpt_layer, FUN = function(x) round(sum(x),2)))
      
      # create pie chart of influenced sales
      p <- plot_ly(rpt_agg, labels = ~tactic, values = ~attr_sales, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste0('$', format(round(attr_sales, digits=2), nsmall = 2), ' M'),
                   marker = list(line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = FALSE) %>%
        layout(title = paste0('Store Sales on: ', order_date),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
               )
      
      # call plotly plot
      p
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

