#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reactable)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(ggthemes)
library(shinyWidgets)

# Load Data ---------------------------------------------------------------
CSU_Transfers = read_csv("data/CSU_Transfers.csv",locale = locale(encoding = 'Latin1'))#because of Canada college.


# Define UI for application that draws a histogram
ui <- dashboardPage(
        dashboardHeader(title = "CSU Transfers"),
        dashboardSidebar(
        selectInput("CC",
                    "Select Community College:",
                    choices = unique(sort(CSU_Transfers$`Institution Name`)),
                    selected = "NE"),
        prettyCheckboxGroup("Gender", 
                            label = "Select Gender", 
                            choices = unique(CSU_Transfers$Sex),
                            selected  = unique(CSU_Transfers$Sex),
                            icon = icon("check")),
        
        prettyCheckboxGroup("Race", 
                            label = "Select Ethnicity", 
                            choices = unique(CSU_Transfers$`Ethnic Group Desc`),
                            selected = unique(CSU_Transfers$`Ethnic Group Desc`),
                            icon = icon("check")),
        prettyCheckboxGroup("Year",
                    "Select Academic Year:",
                    choices = unique(CSU_Transfers$`College Year`),
                    selected = c("2013-2014","2014-2015","2015-2016","2016-2017","2017-2018")),
                    icon = icon("check")
        ),
dashboardBody(
            fluidRow(
                  box(title = "Transfers Overall", solidHeader=T,
                     width = 12,
                     collapsible = T,
                     plotlyOutput("TransfersAll")),
                   box(title = "Transfers by College", solidHeader=T,
                       width = 12,
                       collapsible = T,
                         plotlyOutput("Transfers"))#,
                  # box(title = "Transfers by College", solidHeader=T,
                  #     width = 12,
                  #     collapsible = T,
                  #     plotlyOutput("TransfersTable"))
                     )

                    ) # body
        
)

# Define server logic required to draw a histogram
server <- function(input, output) {

# Chart 1 -----------------------------------------------------------------
  dataset2 <- reactive({
    CSU = CSU_Transfers %>%
      filter(`Institution Name` == input$CC) %>%
      filter(`College Year` %in% input$Year) %>%
      filter(`Ethnic Group Desc` %in% input$Race) %>%
      filter(Sex %in% input$Gender) %>%
      select(Year = `College Year`,Count = `Number of Transfers`) %>%
      group_by(Year) %>%
      summarise(Count = sum(Count)) %>%
      arrange(Year)
    
    
    
    
  })
  
  output$TransfersAll <- renderPlotly({
    
    pdf(NULL)
    p = dataset2() %>% 
      ggplot(aes(x = Year, y = Count, group=1)) +
      geom_point() +
      geom_line() +
      theme_fivethirtyeight() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    
    ggplotly(p)
  })

# Chart 2 -----------------------------------------------------------------
dataset1 <- reactive({
    CSU = CSU_Transfers %>%
      filter(`Institution Name` == input$CC) %>%
      filter(`College Year` %in% input$Year) %>%
      filter(`Ethnic Group Desc` %in% input$Race) %>%
      filter(Sex %in% input$Gender) %>%
      select(Year = `College Year`,CSU = `CSU Campus`,Count = `Number of Transfers`) %>%
      group_by(Year, CSU ) %>%
      summarise(Count = sum(Count)) %>%
      arrange(Year)



    
  })


output$Transfers <- renderPlotly({
    
    pdf(NULL)
    p = dataset1() %>% 
      ggplot(aes(x = Year, y = Count,group = CSU )) +
      geom_point() +
      geom_line(aes(col = CSU)) +
      theme_fivethirtyeight() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    
    ggplotly(p)
  })

# # Chart 3 -----------------------------------------------------------------
# output$TransfersTable <- renderReactable({
#   
#   pdf(NULL)
#   p = dataset1() %>% 
#     pivot_wider(names_from = Year,values_from = Count)
#   
#   
#   reactable(p)
# })

}

# Run the application
shinyApp(ui = ui, server = server)

# rsconnect::forgetDeployment()
# rsconnect::setAccountInfo(name='wilmer-tejada',
#                           token='CB50E5128527907D30087FA8EEF03354',
#                           secret='3Nq0SeUZyRt5OUrXkf1HC/eOV3vSDPYj9nTsX1qp')
# rsconnect::deployApp(appName = "CSU_Transfers_from_CCCs")
#options(shiny.reactlog = TRUE)
