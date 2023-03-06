library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)

freedom <- read.csv("freedom.csv")

ui <- navbarPage("Freedom in the World",
                 tabPanel("Motivation/About Data", 
                          uiOutput("page1")
                 ),
                
                 tabPanel("Conclusion/Takwaways", 
                          uiOutput("page4")
                          )
                 
)

server <- function(input, output) {

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
  
  output$plot2 <- renderPlot({
    freedom_diff <- freedom %>%
      filter(year %in% c(2006, 2020)) %>%
      spread(key = year, value = total) %>%
      mutate(diff = `2020` - `2006`) %>%
      summarise(country_territory, diff) %>%
      drop_na()
    
    ggplot(freedom_diff, aes(x = country_territory, y = diff)) +
      geom_point(fill = "steelblue") +
      ggtitle("Difference in Freedom Index Scores (2020-2006)") +
      xlab("Country") + ylab("Difference in Freedom Index Scores")
  })
  
  output$page1 <- renderUI({
    mainPanel(
      h2("Motivation/Purpose", align = "center"),
      tags$img(src = "https://images.indianexpress.com/2021/04/hh.jpg", alt = "Image shows the many 
               fundamental freedom of speech sources and a hand that shows the people of the country 
               and how it is important to have access to freedom of speech.", 
               align = "left", height = 300, width = 500),
      
      p("Prominent Political Scientists argued for most of the 20th century that a country becomes
      a democracy after a certain amount of economic prosperity is achieved. The freedom index is a metric used to 
      measure the level of freedom enjoyed by individuals in a country. This index is calculated using a number of
      factors, including political rights, civil liberties, and economic freedom. Over time, the level of freedom enjoyed
      by individuals in different countries around the world has changed significantly. While some countries have become 
      more democratic and free, others have become more authoritarian and oppressive. To better understand the 
      relationship between democracy and economic prosperity, we use data from Freedom House that measures 
      democratization across the world based on GDP per Capita data from 2006 to 2022. Additionally, we analyze the 
      relationship between regions and the average freedom index in those regions to determine if a country's location 
      is a significant factor in its freedom level. Finally, we assess countries classified as free and what 
      they have in common that allows them to have a higher freedom index. Through analyzing multiple nations' 
      freedom index, we aim to gain insight into the factors that contribute to a country's level of freedom."),
      
      br(),
      p("Our data is named", strong("Freedom in the World"), "and was first obtained through the
      open source data project", em("tidytuesday"), ". Arthur Cheib constructed the dataset, however
      the data he gathered came from the United Nations and Freedom House. The dataset is simply a collection
      of nations with data spanning from 2005 to 2021 that show whether a country is free or not, 
      covering factors such as the country name, region, status, civil freedoms, 
      political rights, total freedom index, GDP, and GDP per Capita!"),
      
      p("Here is the link to the orgins of the dataset: "),
      a(href = "https://freedomhouse.org/report/freedom-world", target = "_blank", 
        "Dataset 1"),
      a(href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD", target = "_blank", 
        "Dataset 2"),
      a(href = "https://data.worldbank.org/indicator/NY.GDP.MKTP.CD", target = "_blank", 
        "Dataset 3"),
      p("We then combined all of these datasets into one large csv file. A sample can be seen in the 
        later section!"),

      hr(),
      h2("Research Questions", align = "center"),
      
      p("1) We hope to assess the relationship between democracy and economic prosperity. 
      We make this assessment using Freedom House data which measures democratization across the world based on 
      GDP per Capita Data. This data is from 2006 to 2022."),
      
      p("2) We hope to assess the relationship between regions and the average freedom index in those
      regions to analyze whether a region a country is in is a significant factor to determining its freedom level."),
    
      p("3) We hope to assess countries that are classified as free and what these countries have that are 
        allowing them to have a higher freedom index. We hope to assess the relationship between countries and the freedom 
        index through out certain years and analyze multiple nation's freedom index to other nations."),
      
      p("We believe that these questions can help us find a pattern as to where the most freedom is found and
        what factors those countries with higher freedom index have that the lower freedom index countries don't."),
      
      hr(),
      
      h2("Definition of Values", align = "center"),
      h5("Country: The name of the country"),
      h5("Region: The region of the country"), 
      h5("Year: The year the data was aggregated"), 
      h5("Status: The status of how free a nation is. F = Free. PF = Partially Free. 
         NF = Not Free"),
      h5("Political Rights Score: Nation's politial rights rating."),
      h5("Civil Liberties Score: Nation's civil liberty ratings."),
      
      h5("Freedom Index: Freedom index score assigned to every country in the world based on the
         political rights and civil liberties available to citizens."),
      
      h5("GDP: The nation's gross domestic product"),
      h5("GDP Per Capita: country's economic output per person"),
      br(),
      hr(),
      h2("Sample of Data from 2020", align = "center"),
      p("Here is a quick look into how the dataset is formatted:", align = "center"),
      tableOutput("table_intro"),
    )
  })
  
  output$page4 <- renderUI({
      mainPanel(
        tags$img(src = "https://imageio.forbes.com/specials-images/imageserve/622787564f26c2250a237581/A-yellow-light-bulb-floating-in-the-air-above-four-white-light-bulbs-resting-on-a/960x0.png?format=png&width=960", 
                 alt = "Image shows a lightbulb with the word insight written over it.", 
                 align = "left", height = 300, width = 500),
        
        h2("Notable Insights", align = "center"),
        ("Insights: Couple of insights that we gained from the charts was that those
         countries that had a higer freedom index tended to have a higher GDP per capita. 
         Another insight we see from our graphs is that those countries who do not use the
         democratic style of government have a lower freedom index, and this can be because
         of the strict policies of civil liberties as seen in North Korea and China where 
         communism is more popular. Based on region, the countries which are surrounded 
         by regions which have a higher freedom level also tend to have a higher freedom level.
         For example, the region of the Europe and Americas has more Free and Partially free than all other
         regions and this could be because the west has more democracy than other regions. However we have
         also noticed that for regions like Europe, Americas, Africa, Eurasia and the Middle East, the 
         average total of freedom levels for the regions are dropping slightly which may be a concern. This 
         could also be a national security reason on why these averages are dropping as more governments are 
         more focused on privacy, especially in the tech world we are in now, and are making more policy changes
         which may lower civil liberties score.  
         "),
        hr(),
        
        h2("Chart that Demonstrates the Pattern/Insight", align = "center"),
        plotOutput("plot2"),
        hr(),
        
        h2("Broader Implications of the Insight", align = "center"),
        p("The insights gained from the charts have several broader implications for policymakers, academics, and the general public.
           Firstly, the correlation between a higher freedom index and a higher GDP per capita suggests that increasing political 
           freedoms and civil liberties can have a positive impact on a country's economic development. Therefore, policymakers can use 
           this information to prioritize policies that promote democratic values and human rights.
           
           Secondly, the observation that countries with non-democratic styles of government tend to have lower freedom 
           indices highlights the importance of promoting democratic values and practices worldwide. By supporting 
           democratic institutions and civil society organizations, countries can help promote human rights and 
           freedom around the world. Thirdly, the observation that the average freedom levels in certain regions 
           are dropping slightly raises concerns about the state of democracy and human rights in those regions. 
           Policymakers and civil society organizations can use this information to target their efforts towards 
           promoting democracy and civil liberties in those regions.
          
          Finally, the observation that privacy concerns may be contributing to the drop in freedom levels 
          highlights the need to balance national security concerns with civil liberties. Policymakers and 
          tech companies can use this information to design policies and products that protect privacy while also 
          promoting freedom and democracy. Overall, these insights have important implications for promoting 
          freedom and human rights around the world."),
        hr(),
        
        h2("Data Quality", align = "center"),
        p("The first thing we did as a group was check out the research methodologies 
          of freedom house to see how they obtained the data and how they ranked each 
          country. In this document, Freedom House has a series of questions and breaks
          down each question to points and sees if that country fuilfils that category 
          in order to receive a point in that category. While the research methodologies
          seem to be adminsitered correctly, there still might be biases involved that 
          can be assumed by media representation. In terms of the quality of the data, 
          the Freedom House has only null values for those countries which they cannot 
          get an accurate data of. For example, those viewing the visualizations may 
          encounter some GDP missing due to the lack of information from that country, which 
          also could be a factor in the scorings of the country's freedom index. The group 
          came up with some biases that we think plays a crucial role: 
          Freedom House Index is biased towards Western-style democracy, as it tends to measure 
          freedom based on a specific model of democracy that may not be appropriate for all countries and cultures. 
          the Index can be influenced by political biases, particularly when it comes to countries that are 
          perceived as being allies or adversaries of the United States. The bias in the Freedom House Index can harm various 
          population groups, particularly those in non-Western countries or countries with different cultural 
          and political traditions. For example, if the Index is biased towards Western-style democracy, 
          it may undervalue the level of freedom in countries with different forms of government, 
          potentially harming populations in those countries. Similarly, if the Index is influenced 
          by political biases, it could lead to unfair assessments of countries that are perceived 
          as adversaries of the United States, potentially harming populations in those countries as well (causing more
          discrimination)"),
        hr(),
        
        h2("Future Ideas", align = "center"),
        p("We want to expand on this project by building a graph that can change based on certain events,
          such as a country lowering the number of severe regulations on freedom of speech/religion and 
          observing how it affects the country's freedom score. Using this, we can also expand the project 
          to include more detailed data on nations and determine which policies are causing the freedom index 
          score that it provides. By deciding which policies to implement in a country, the freedom index score 
          will change, allowing governments to know what they can do to improve their freedom index score."),
        hr(),
      )
  })
}

shinyApp(ui = ui, server = server)
