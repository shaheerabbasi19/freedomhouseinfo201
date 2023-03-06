#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
fr <- read_csv("freedom.csv")
# Define UI for application that draws a histogram
ui <- navbarPage("Freedom in the World",
                 tabPanel("Plot",
                          uiOutput("page1")
                 ),
                 tabPanel("Interactive",
                          uiOutput("page2"))
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot1 <- renderPlot({
    if (input$graph == 1) {
      fr %>% filter (year == input$yr) %>% filter (status == "F") %>% 
        ggplot(aes(x = pr, y = cl)) + geom_point() + 
        geom_smooth(se = FALSE, method = "lm") + ggtitle ("Political Rights vs. Civil Liberty") +
        labs(x = "Political Rights Index Score", y = "Civil Liberty Index Score")
    } else if(input$graph == 2) {
      fr %>% filter (year == input$yr) %>% filter(status == "F") %>% 
        ggplot(aes(x = pr)) + geom_histogram() + 
        ggtitle ("Political Rights Count") +
        labs(x = "Political Rights Index Score", y = "counts")
    } else if(input$graph == 3){
      fr %>% filter (year == input$yr) %>% filter(status == "F") %>% 
        ggplot(aes(x = cl)) + geom_histogram() + 
        ggtitle ("Civil Liberty Count") +
        labs(x = "Civil Liberty Index Score", y = "counts")
    } else {
      fr %>% filter (year == input$yr) %>% filter(status == "F") %>% 
        ggplot(aes(x = total, y = gdp_pc)) + geom_point() + 
        geom_smooth(se = FALSE, method = "lm") + ggtitle ("Freedom Index Score vs. GDP per capita") +
        labs(x = "Freedom Index Score", y = "GDP per Capita")
    }
  })
  
  output$text1 <- renderText({
    if(input$graph == 1) {
      print("Based on the graphs from 2005 - 2021, it seems like that the political rights and civil 
        liberty of those free countries tend to have linear relationship, which as the index score
        of political rights gets higher, the index score of civil rights also gets higher. And also
        as the index score of political rights gets lower, the index score of civil rights also 
        gets lower.")
    } else if(input$graph == 2) {
      print("Based on the histograms from 2005 - 2021 of political rights count, the free countries
            have the index score of political rights mostly higher than 25.")
    } else if(input$graph == 3) {
      print("Based on the histograms from 2005 - 2021 of civil liberty count, most of the free 
            countries have the index score of civil liberty higher than 40.")
    } else {
      print("Based on the graphs from 2005 - 2021, although there do exist some outliers in the graph,
            it seems most of the countries follow the pattern that as the index score of political 
            freedom, the higher that the GDP per capita will be.")
    }
  })
  
  output$page1 <- renderUI({
    sidebarLayout(
      sidebarPanel(
        radioButtons("graph", "What grpah?", 
                     list("Political Rights vs. Civil Liberty" = 1,
                          "Freedom vs. GDP per capita" = 4,
                          "Political Rights" = 2,
                          "Civil Liberty" = 3)),
        
        
        fluidRow(
          column(6, sliderInput("yr", "Which Year:",
                                min = 2005,
                                max = 2021,
                                value = 1),),
          
        )
      ),
      mainPanel(
        plotOutput("plot1"),
        textOutput("text1")
      )
    )
  })
  
  output$table <- renderTable({
    fr %>% 
      filter(year == input$time) %>% 
      filter(country_territory == input$place) %>% 
      select(country_territory, year, pr, cl, total, gdp_pc) %>%
      rename("Country" = "country_territory", 
             "Year" = "year",
             "Political Rigts Index Score" = "pr",
             "Civil Liberty Index Score" = "cl",
             "Freedom Index Score" = "total",
             "GDP per capita" = "gdp_pc") %>% tibble()
  })
  
  output$page2 <- renderUI({
    sidebarLayout(
      sidebarPanel(
        selectInput("place", "What country?", 
                     unique(fr$country_territory)),
        
        
        fluidRow(
          column(6, sliderInput("time", "Which Year:",
                                min = 2005,
                                max = 2021,
                                value = 1),),
          
        )
      ),
      mainPanel(
        tableOutput("table"),
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
