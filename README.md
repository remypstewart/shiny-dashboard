# R Shiny KPI Dashboard
![alt text](/layout.jpg)

[Live dashboard is available here- the figures will take a moment to load after you hit submit. ](https://remypstewart.shinyapps.io/shinyapps/) 

During the summer of 2020, I interned with the [National Opinion Research Center (NORC)](https://www.norc.org/Pages/default.aspx), a nonpartisan independent research institution that is an industry leader on providing empirically robust findings to guide private and public sector clients. NORC’s daily operations involves over a million annual calls to US households to administer population-level surveys for clients across industries including the [National Immunization Survey]( https://www.norc.org/Research/Projects/Pages/national-immunization-survey.aspx) for the US Center for Disease Control and [AmeriSpeak]( https://amerispeak.norc.org/Pages/default.aspx) in collaboration with Associated Press news. The organization has multiple call centers that ongoingly stream hourly records to a centralized SQL Server database that cover core KPIs including the number of outbound and inbound calls, successful participant contacts, call time lengths, and beyond. 

An ongoing problem within NORC was the lack of a centralized system to preprocess, analyze, and visualize said metrics for stakeholders across the organization who consistently referred to these data streams to guide projects and key business decisions. To address this need for an efficient and user-friendly KPI visualization site for NORC, I developed an R Shiny interactive web dashboard based on Plotly data visualizations as one of my internship assignments. Due to the confidential nature of the internal data I featured within my NORC dashboard, I will be demoing my Shiny program with a test dataset adapted from call records sourced from Milan, Italy provided by Barlacchi et al. (2015). This public dataset has variables conceptually aligned with my internal NORC data source and therefore serves as an ideal substitute. I separately preprocessed the 2 million call records before loading it into my Shiny application through Tidyverse methods such as dplyr and lubridate. 

The following code structures the User Interface (UI) that organizational affiliates interact with to select filtering parameters for specific locations and survey metrics as well as manipulate the dynamic Plotly visuals to view averages, minimum and maximum values, and beyond. I designed a 2-by-2 grid layout with the top left tile featuring the selection options and the remaining grid subsets each featuring a designated plot. This includes a bar chart highlighting the aggregate call minutes in each center by hour, a stacked area plot contrasting the proportion of average hourly inbound over outbound calls, and box plots of the range of hourly call minutes distributed over the week. I use additional CSS and HTML parameters to configure the UI appearance as well.

```r
library(shiny)
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

```

The server component of the dashboard script employs hierarchically nested reactivity where initial input selections automatically filter the overarching call dataset each figure draws from. The Plotly object rendering is grounded in a central if/else statement dividing the two survey options that is prompted by a user clicking the final Submit button. I include a loading feature for each visual through the shinycssloader package due to longer rendering times of Plotly figures.

```r
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
```

This framework is parallel to the live version my team at NORC now has in production following the end of my summer internship. This dashboard serves as a key resource to monitor call center KPIs and alert stakeholders of potential issues regarding center operations as highlighted by the Plotly visuals. Being able to meet an ongoing need of the organization through my developed R Shiny dashboard was a wonderful opportunity to make a long-lasting impact within NORC and further develop my data visualization and web application design skills. 

Reference: 
Barlacchi, Gianni, Marco De Nadai, Roberto Larcher, Antonio Casella, Christiana Chitic… Bruno Lepri. 2015. “A multi-source dataset of urban life in the city of Milan and Province of Trentino.” Scientific Data 2 (150055): 1-15. 
