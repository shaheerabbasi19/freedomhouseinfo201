
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)

freedom <- read.csv("fiw_gdp.csv")

#Shaheer's Project

ui <- navbarPage("Freedom in the World", #shaheer
                 tabPanel("Wealth and Freedom", #SHaheer
                          uiOutput("economic")
                 ),
                 
)

server <- function(input, output) {
  
  
  #This table shows the change in average GDP per Capita in free, not free, and partially free countries by region
  #Plot w/ GDP changing by status and region
  output$gdp_plot <- renderPlot ({
    
    if (input$xaxis == 0) { #if the user wants to see the graph in percentage terms
    
      freedom %>% group_by(status, year) %>%  filter(!is.na(gdp_pc)) %>% 
      filter(region == input$regions) %>%
      summarize (avg_gdp_pc = mean(log(gdp_pc))) %>% #calculating log gdp_pc instead of normal gd per capita
      ggplot(aes(x = year, y = avg_gdp_pc, color = status)) + 
        geom_point() + #points on graph
        geom_smooth(se = FALSE) + #trend line on graph
      labs(x = "Year", y = "Average GDP Per Capita (logarithmic)", status = "Status",  #labelling graph
           caption = "F = Free, PF = Partially Free, NF = Not Free") +
      ggtitle("Average GDP Per Capita by Region Amongst Free, Partially Free, and Not Free Countries") #title
    }
    
    else{ #All the same as above but with non-percentage gdp per capita
      freedom %>% group_by(status, year) %>%  filter(!is.na(gdp_pc)) %>% 
      filter(region == input$regions) %>%
        summarize (avg_gdp_pc = mean(gdp_pc)) %>%
        ggplot(aes(x = year, y = avg_gdp_pc, color = status)) + 
        geom_point() + 
        geom_smooth(se = FALSE) +
        labs(x = "Year", y = "Average GDP Per Capita", status = "Status", 
             caption = "F = Free, PF = Partially Free, NF = Not Free") +
        ggtitle("Average GDP Per Capita by Region Amongst Free, Partially Free, and Not Free Countries")
      
    }
     }) #Shaheer

  #Table that will go in the side panel of the econ page.
  #This table will show the GDP and Freedom Index score of every country. It ranks from from richest to poorest.
  output$side_table <- renderTable({
    
    freedom %>% 
      filter(!is.na(gdp_pc)) %>% #removing na variables. It screws with my calcualtion
      filter (year == 2021) %>%  #setting they year
      select(country_territory, total, status, gdp_pc) %>% #selecting the variables I wnat to display
      arrange(desc(gdp_pc)) %>% #arranging it from richest to poorest
      head(60) %>% rename( #selecting the first 50 richest countries and making the variable names more displayable
        "Country" = "country_territory",
        "Freedom Index Score" = "total",
        "Status" = "status",
        "GDP Per Capita" = "gdp_pc"
        )%>% tibble()
      
    
  }) #Shaheer

  #This Table Shows the Average GDP Per Capita and Freedom Index Score by Region in 2005 and 2021.
  output$gdp_freedom_region_table <- renderTable({
    freedom %>% 
      filter(!is.na(gdp_pc)) %>% #removing na variables 
      group_by(region, year) %>% #grouping by region and year so I can see what these variables are for every region for set years
      summarize("Average Freedom Index Score" = mean(total), #calculating average Freedom Index Score and GDP Per Capita  
                "Average GDP Per Capita" = mean(gdp_pc)) %>%
      filter (year == 2005 | year == 2021) %>% tibble() #selecting years of interest
    
  }) #Shaheer
  
  #Shaheer
  #Code used to publish all economic page.
  output$economic <- renderUI({
  
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, #This widget takes region inputs from the user to put towards the plot.
                 radioButtons("regions", "Region: ",
                              c("Asia", "Europe", "Africa", "Americas", "Eurasia", "Middle East"))),
          column(6, #This widget takes whether the user wants to see their GDP Per Capita in Percentage or not perceentage terms.
                 selectInput("xaxis", "GDP Per Capita", 
                             c("percentage" = 0, "not percentage" = 1)))
        ),
        
        h3("Richest Countries in 2021"), #Title for my Side Table
        tableOutput("side_table") #Side Table shows richest countries in 2021 and their freedom status and score.

        ),

      mainPanel( 
        h1("The Relationship Between Wealth and Freedom"),
        plotOutput("gdp_plot"), #my main graph. Explained above. Just Average GDP Per Capita across time for free, not free, and partially free countries 
        br(),
        
        h2("Motivation", align = "center"),
        br(),
        p("Prominent Political Scientists argued for most of the 20th century that a country becomes a democracy after a certain amount of economic prosperity is achieved.
           While theories abound for why, the primary argument is that economic growth leads to an empowered middle class and business interests which then seek to gain political power for their class. 
          This was the argument used by the Clinton administration to admit the People's Republic of China into the World Trade Organization."),
        br(),
        p("The graph above shows the average GDP Per Capita for Free, Not Free, and Partially Free countries in different regions from 2005 to 2021. It produces three key interesting findings.
          When the GDP is logarithmic, it is to make changes in GDP Per Capita easier to see."),
        
        br(),
        hr(),
        br(),
        
        h3("Finding 1: Free Countries are Wealthier"),
        br(),
        p("In all regions other than Eurasia, Free countries are wealthier than partially free or not free countries. 
          This gap in wealth is sustained across all regions but most prominent in Europe and the Americas. 
          Eurasia seems to be an outlier in this trend because there are no free countries in Eurasia after 2008."),
        br(),
        p("This finding is reinforced by statistical evidence because the average GDP per capita amongst rich countries is ~$27,000,
          compared to ~$6,500 for not free countries and ~$4,800 for partially free countries. This result is partially skewed by Europe because
          most countries in Europe are free and Europe is a richer continent. The difference in wealth still holds, despite Europe, because the average GDP per Capita
          for free countries is ~$13,500."),
        
        br(),
        hr(),
        br(),
        
        h3("Finding 2: Freedom in Asia"),
        br(),
        p("Asia is interesting because Asia has experienced immense economic growth from the 1990s till now. Based on the graph, free countries are much richer than not free or partially free countries in Asia.
          It is interesting to see that partially free countries are getting wealthier than not free countries. Moreover, the gap in GDP per capita between free countries and other countries is closing.
          This result suggests that hope for democracy is strongest in Asia right now. As seen in the table below, freedom scores across the regions are not rising while they get wealthier.
          While the table is skewed because  advances in the freedom of individual countries are drowned out by the average, it does show that freedom is not decreasing in Asia."),
        
        br(),
        hr(),
        br(),
        
        h4("Freedom Index Score and GDP Per Capita by Region between 2005 and 2021"),
        br(),
        tableOutput("gdp_freedom_region_table"),
        
        br(),
        hr(),
        br(),
        
        h3("Finding 3: Trends in Growth"),
        br(),
        p("As seen from the table, the entire world is getting richer. While this speed of this growth is spread unevenly across regions, it is also spread unevenly across free, not free, and partially free countries.
          Most pronounced in the Middle East, the Americas, and Africa, free countries are getting richer faster than not free or partially free countries. The gap in wealth is expanding.
          This is not true for all regions, mainly Asia.
          The table also shows that freedom is not rising across the world. It is not a fair analysis to look at average freedom scores across regions to make this judgement because the assessment of freedom is context-specific. 
          Regardless, major world events in countries around the world show that democracy might be in the retreat."),
        
        br(),
        hr(),
        br(),
        
        h4(strong("Key Finding: free countries are usually richer than not free or partially free countries. A country getting richer does not perfectly correlate with it getting freerer."))
        
        ))
  
  })
  
}

shinyApp(ui = ui, server = server)
