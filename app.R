library(tidyverse)
library(leaflet)
library(htmltools)
library(janitor)
library(shiny)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(plotly)
library(shinythemes)
library(lubridate)


care_homes = read_csv("data/care_homes_2019.csv")

care_homes = care_homes %>%
  mutate(year = year(publication_date),
         month = month(publication_date), 
         dimension = case_when(
           care_homes_beds < 10 ~ "< 10", 
           care_homes_beds %in% c(10:25) ~ "10 - 25", 
           care_homes_beds %in% c(25:50) ~ "26 - 50", 
           care_homes_beds >50 ~ "> 50")) 

ui <- navbarPage("Quality in care homes",theme = shinytheme("sandstone"),
                 
      
                 tabPanel("Data",
                          br(),
                          DT::dataTableOutput(outputId = "carehomestable"),
         h6("Source: Care Quality Commission")
         ),
                 
                 
                 
                 tabPanel("Inspections per year (regions)",
                          fluidRow(
                            mainPanel(plotlyOutput("inspectionsplot",width = "150%",height="600px"),
                                      column(12,
                                             h6("*Double click on a Region to isolate or single click to remove it from graph")
                                             
                                      )
                            )
                          )
                 ),
                 
                 
                 
                 tabPanel("Ratings per quality dimension (local authorities)",
                          fluidPage(
                            selectInput("localauthorityInput","Local Authority",choices = unique(care_homes$district_name),
                                        selected="Newcastle upon Tyne")
                            
                          ),
                          
                          mainPanel(plotlyOutput("care_home_ratings"),width="130%",height="200%")
                          
                          
                          
                 )
              )


#--------SERVER--------#
server <- function(input, output) {
  
  
  care_homes_selected = care_homes %>%
    select(location_name, location_post_code, district_name, publication_date, key_question, latest_rating, number_inspection, total_inspections_category)
    
  

  
  
  yearly_inspections = care_homes %>% 
    group_by(year, region_name) %>%
    summarise(inspections = n()) %>%
    filter(year != 2014) %>%
    ungroup()
  
  yearly_inspections$year = as.factor(yearly_inspections$year) 
  

  #line chart
  output$inspectionsplot <- renderPlotly({ 
    plot_ly(yearly_inspections,x=~year,y=~inspections, color=~region_name,type="scatter",mode='lines+markers') %>% 
      layout(legend=list(x=100,y=0.7), 
             yaxis=list(title='Number of inspections'),
             xaxis=list(title=''))
  })
  
  
  
  ratings = care_homes %>%
    group_by(location_id, key_question) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    group_by(district_name, key_question, latest_rating) %>%
    tally() %>%
    spread(latest_rating, n)
    
    
  ratings = ratings %>% replace(., is.na(.), 0) 
    
  

  
  #make data reactive
  ratings_react <- reactive({ratings %>% 
      filter(district_name==input$localauthorityInput)
    
  })
  
  
  #bar chart
  output$care_home_ratings <- renderPlotly({
    plot_ly(ratings_react(),x = ~key_question, y = ~Outstanding, type = 'bar', name = 'Outstanding') %>%
      add_trace(y = ~Good, name = 'Good') %>%
      add_trace(y=~`Requires improvement`,name="Requires improvement") %>% 
      add_trace(y=~`Inadequate`,name="Inadequate") %>% 
      layout(yaxis=list(title='Number of care homes inspected'),barmode='group',
             xaxis = list(title = ""),
             legend=list(x=100, y=0.7))
  })
  
  output$carehomestable = DT::renderDataTable({
    care_homes_selected
  })
  

  
}

shinyApp(ui = ui, server = server)
