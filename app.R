

library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)

freedom <- read.csv("fiw_gdp.csv")

ui <- navbarPage("Freedom in the World",
  tabPanel("About Data", 
           uiOutput("page1")
           ),
  
  tabPanel("Economics and Country - Plot",
           uiOutput("page2")
           ),
  
  tabPanel("Table",
           uiOutput("page3"))
  
)

server <- function(input, output) {

  
    output$plot <- renderPlot ({
      
      if(input$xaxis == "gdp") {
      freedom %>% filter (year == input$yr) %>% filter(region == input$region) %>% 
        ggplot(aes(x = log(gdp), y = total, color = status)) + geom_point() + 
          geom_smooth(se = FALSE, method = "lm") + ggtitle ("The Relationship Between GDP and Freedom House Freedom Index") +
          labs(x = "Gross Domestic Product", y = "Freedom Index Score", color = "Status", caption = "F = Free, PF = Partially Free, NF = Not Free")
        }
      
     else{ freedom %>% filter (year == input$yr) %>% filter(region == input$region) %>%  
        ggplot(aes(x = log(gdp_pc), y = total, color = status)) + geom_point() + 
         geom_smooth(se = FALSE, method = "lm") + ggtitle ("The Relationship Between GDP Per Capita and Freedom House Freedom Index") +
         labs(x = "Gross Domestic Product, Per Capita", y = "Freedom Index Score", color = "Status", caption = "F = Free, PF = Partially Free, NF = Not Free")
       
       }
      
          }) # Plot on Page 2
    
    output$table <- renderTable ({
      if(input$order == "desc") { 
      freedom %>% filter(year == input$year1) %>% filter(region == input$regions) %>% arrange(desc(total)) %>%
          head(5) %>%
        select(country_territory, region, year, status, total, gdp_pc) %>%
          rename("Country" = "country_territory", 
                 "Region" = "region",
                 "Year" = "year",
                 "Status" = "status",
                 "Freedom Index" = "total",
                 "GDP Per Capita" = "gdp_pc") %>% tibble()
      }
      else(
        freedom %>% filter(year == input$year1) %>% filter(region == input$regions) %>% arrange(total) %>%
          select(country_territory, region, year, status, total, gdp_pc) %>% head(5) %>% 
          rename("Country" = "country_territory", 
                 "Region" = "region",
                 "Year" = "year",
                 "Status" = "status",
                 "Freedom Index" = "total",
                 "GDP Per Capita" = "gdp_pc") %>% tibble()
      )
    }) # Plot on Page 3
    
    output$table_intro <- renderTable  ({
      freedom %>% filter(year == 2020) %>% sample_n(20) %>%
        select(country_territory, region, year, status, pr, cl, total, gdp, gdp_pc) %>% 
        rename("Country" = "country_territory", 
               "Region" = "region",
               "Year" = "year",
               "Status" = "status",
               "Freedom Index" = "total",
               "GDP Per Capita" = "gdp_pc",
               "Political Rights Score" = "pr",
               "Civil Liberties Score" = "cl",
               "GDP" = "gdp") %>% sample_n(20) %>%tibble()
    })
    
  
    output$page1 <- renderUI({
      mainPanel(
        h2("Motivation", align = "center"),
        p("Prominent Political Scientists argued for most of the 20th century that a country becomes a democracy after a certain amount of economic prosperity is achieved.
           While theories abound for why, the primary argument is that economic growth leads to an empowered middle class and business interests which then seek to gain political power for their class. 
          This was the argument used by the Clinton administration to admit the People's Republic of China into the World Trade Organization."),
        hr(),
        h2("Research Question", align = "center"),
        p("I hope to assess the relationship between democracy and economic prosperity. I make this assessment using Freedom House data which measures democratization across the world based on 
          GDP per Capita Data. This data is from 2006 to 2022."),
        hr(),
        h2("Definition of Values", align = "center"),
        h5("total: this is a freedom index score assigned to every country in the world based on the political rights and civil liberties available to citizens."),
        h5("country_territory: The name of each country in the dataset."),
        h5("status: this represents whether a country is deemed, free, not free, or partially free by Freedom House."),
        br(),
        h2("Sample of Data from 2020"),
        tableOutput("table_intro"),
        
)
    })
    
    output$page2 <- renderUI({
      sidebarLayout(
        sidebarPanel(
          sliderInput("yr", "Which Year:",
                      min = 2005,
                      max = 2021,
                      value = 1),
          fluidRow(
            column(6,
                   radioButtons("region", "Region:", 
                                c("Asia", "Europe", "Africa", "Americas", "Eurasia", "Middle East"))),
            column(6,
                   selectInput("xaxis","X - Axis",
                               c("GDP" = "gdp", "GDP Per Capita" = "gdp_pc"))
                   )
          )

        ),
      
        mainPanel(
        plotOutput("plot")
      )
      
      )
    })
    
    output$page3 <- renderUI({
      sidebarLayout(
        sidebarPanel(
          sliderInput("year1", "Which Year:",
                      min = 2005, 
                      max = 2021, 
                      value = 1),
          fluidRow(
            column(6,
                   radioButtons("regions", "Region: ",
                                c("Asia", "Europe", "Africa", "Americas", "Eurasia", "Middle East"))),
            column(6,
                   selectInput("order", "Ranking of Freedom Order:", 
                               c("Bottom 5" = "asc", "Top 5" = "desc")))
          )
        ),
        mainPanel(
          tableOutput("table")
        )
      )
    })
    
    
    
    
  }

shinyApp(ui = ui, server = server)
