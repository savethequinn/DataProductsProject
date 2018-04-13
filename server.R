#server.R

library(shiny)
library(data.table)
library(lubridate)
library(plotly)
library(ggplot2)

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