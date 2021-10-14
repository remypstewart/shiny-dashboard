library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(rsconnect)

calls <- read_csv("call_records.csv")

ui <- fluidPage(fluidRow(useShinydashboard(),
                         column (6, box(width=12,
                                        titlePanel(div(style = "font-size:45px", "Call Center KPIs Dashboard")),
                                        selectInput("callcenter", label = div(style = "font-size:20px", "Choose Call Center"), 
                                                    choices = c("Washington DC", "Cleveland", 
                                                                "Seattle", "El Paso", "Miami",
                                                                "Anchorage", "Denver")),
                                        tags$head(tags$style(HTML(".selectize-input height: 40px; width: 500px; font-size: 25px;}"))),
                                        tags$head(tags$style(HTML(".radio-inline {margin-left: 20px;}"))),
                                        prettyRadioButtons("survey", label = div(style = "font-size:20px", "Choose Survey"), 
                                                           choices = c("Market Research", "Longitunidal Panel"), 
                                                           inline = T, bigger = T),
                                        actionButton("submit", "Submit", style="padding:10px; font-size:20px; display:center-align;"))),
                         column (6, status = "primary", plotlyOutput("areaplot") %>% withSpinner(type = 4, color = "#000000"))),
                (fluidRow(column (6, plotlyOutput("boxplot") %>% withSpinner(type = 4, color = "#000000")),
                          column (6, plotlyOutput("barchart") %>% withSpinner(type = 4, color = "#000000")))))

server <- function(input, output) {
  observeEvent((input$submit), {
    if (input$survey == "Market Research") {
      ## Rendering stacked area chart- market research  
      areaplot <- calls %>% filter(center==input$callcenter) %>%
        group_by(hour) %>% mutate(sumin = mean(marketin)) %>% 
        mutate(sumout = mean(marketout)) %>% 
        plot_ly(x = ~hour, y = ~sumin, type = 'scatter', 
                mode = 'lines', name = 'Inbound Calls', stackgroup = 'one',
                fillcolor= '#e5cbf5', line= list(width=0.5, color = '#e5cbf5')) %>%
        layout(title = list(text = 'Average Call Length by Hour', xanchor = "right"),
               xaxis = list(title = 'Call Hour',
                            linecolor = "black",
                            linewidth = 0.5,
                            mirror = T,
                            showgrid = F,
                            dtick = 2, 
                            tick0 = 0, 
                            tickmode = "linear"),
               yaxis = list(title = 'Call Length',
                            linecolor = "black",
                            linewidth = 0.5,
                            mirror = T,
                            showgrid = F)) %>% hide_legend()
      
      areaplot <- areaplot %>% add_trace(y = ~sumout, name = 'Outbound Calls', stackgroup = 'one', fillcolor= '#cbe3f5', line= list(color = '#cbe3f5'))
      output$areaplot <- renderPlotly(areaplot)
      
      ## Rendering weekly box pot- market research
      output$boxplot <- renderPlotly(calls %>% 
                                       filter(center==input$callcenter) %>%
                                       group_by(day, hour) %>% 
                                       mutate(sumall = sum(marketall)) %>%
                                       plot_ly(y = ~sumall, color = ~day, type = "box") %>%
                                       layout(title = list(text = 'Hourly Call Time by Day', xanchor = "right"), 
                                              xaxis = list(title = 'Week Day',
                                                           linecolor = "black",
                                                           linewidth = 0.5,
                                                           mirror = T,
                                                           showgrid = F),
                                              yaxis = list(title = 'Total Call Time',
                                                           linecolor = "black",
                                                           linewidth = 0.5,
                                                           mirror = T,
                                                           showgrid = F)) %>%
                                       hide_legend())
      
      ## Rendering hourly bar chart- market research
      output$barchart <- renderPlotly(calls %>% 
                                        filter(center==input$callcenter) %>%
                                        group_by(hour) %>%  
                                        summarize(sum = sum(marketall)) %>%
                                        plot_ly(x = ~hour, 
                                                y = ~sum, 
                                                color = ~hour,
                                                type = 'bar') %>%
                                        layout(title = list(text = 'Total Call Minutes By Hour',  xanchor = "right"), 
                                               xaxis = list(title = 'Call Hour', 
                                                            linecolor = "black",
                                                            linewidth = 0.5,
                                                            mirror = T,
                                                            showgrid = F,
                                                            dtick = 2, 
                                                            tick0 = 0, 
                                                            tickmode = "linear"),
                                               yaxis = list(title = 'Call Minutes',
                                                            linecolor = "black",
                                                            linewidth = 0.5,
                                                            mirror = T,
                                                            showgrid = F)) %>%
                                        hide_colorbar())
      
    } else if (input$survey == "Longitunidal Panel") {
      ## Rendering stacked area chart- longitunidal panel
      areaplot <- calls %>% filter(center==input$callcenter) %>%
        group_by(hour) %>% mutate(sumin = mean(marketin)) %>% 
        mutate(sumout = mean(marketout)) %>% 
        plot_ly(x = ~hour, y = ~sumin, type = 'scatter', 
                mode = 'lines', name = 'Inbound Calls', stackgroup = 'one',
                fillcolor= '#e5cbf5', line= list(width=0.5, color = '#e5cbf5')) %>%
        layout(title = list(text = 'Average Call Length by Hour', xanchor = "right"),
               xaxis = list(title = 'Call Hour',
                            linecolor = "black",
                            linewidth = 0.5,
                            mirror = T,
                            showgrid = F,
                            dtick = 2, 
                            tick0 = 0, 
                            tickmode = "linear"),
               yaxis = list(title = 'Call Length',
                            linecolor = "black",
                            linewidth = 0.5,
                            mirror = T,
                            showgrid = F)) %>% hide_legend()
      
      areaplot <- areaplot %>% add_trace(y = ~sumout, name = 'Outbound Calls', stackgroup = 'one', fillcolor= '#cbe3f5', line= list(color = '#cbe3f5'))
      output$areaplot <- renderPlotly(areaplot)
      
      ## Rendering weekly box pot- longitunidal panel
      output$boxplot <- renderPlotly(calls %>% 
                                       filter(center==input$callcenter) %>%
                                       group_by(day, hour) %>% 
                                       mutate(sumall = sum(panelall)) %>%
                                       plot_ly(y = ~sumall, color = ~day, type = "box") %>%
                                       layout(title = list(text = 'Hourly Call Time by Day', xanchor = "right"), 
                                              xaxis = list(title = 'Week Day',
                                                           linecolor = "black",
                                                           linewidth = 0.5,
                                                           mirror = T,
                                                           showgrid = F),
                                              yaxis = list(title ='Total Call Time',
                                                           linecolor = "black",
                                                           linewidth = 0.5,
                                                           mirror = T,
                                                           showgrid = F)) %>%
                                       hide_legend())
      
      ## Rendering hourly bar chart- longitunidal panel
      output$barchart <- renderPlotly(calls %>% 
                                        filter(center==input$callcenter) %>%
                                        group_by(hour) %>%  
                                        summarize(sum = sum(panelall)) %>%
                                        plot_ly(x = ~hour, 
                                                y = ~sum, 
                                                color = ~hour,
                                                type = 'bar') %>%
                                        layout(title = list(text = 'Total Call Minutes By Hour',  xanchor = "right"),
                                               xaxis = list(title = 'Call Hour',
                                                            linecolor = "black",
                                                            linewidth = 0.5,
                                                            mirror = T,
                                                            showgrid = "false",
                                                            dtick = 2, 
                                                            tick0 = 0, 
                                                            tickmode = "linear"),
                                               yaxis = list(title = 'Call Minutes',
                                                            linecolor = "black",
                                                            linewidth = 0.5,
                                                            mirror = T,
                                                            showgrid = F)) %>%
                                        hide_colorbar()) 
    }
  })
}

shinyApp(ui, server)
