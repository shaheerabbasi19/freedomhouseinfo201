library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
freedom <- read_delim("freedom.csv")

# Define UI for application that draws a histogram
ui <- navbarPage("Freedom in the World",
                 tabPanel("Motivation/About Data", 
                          uiOutput("page1")),
                 tabPanel("Wealth and Freedom",
                          uiOutput("economic")),
                 tabPanel("Political Rights, Civil Liberties, & GDP",
                          uiOutput("page3")),
                 tabPanel("Freedom Across Regions",
                          uiOutput("page4")),
                 tabPanel("Conclusion",
                          uiOutput("page5")))
                 

# Define server logic required to draw a histogram
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
             "GDP" = "gdp") %>% sample_n(10) %>%tibble()
  })
  
  output$page1 <- renderUI({
    mainPanel(
      h2("Motivation/Purpose", align = "center"),
      tags$img(src = "https://images.indianexpress.com/2021/04/hh.jpg", alt = "Image shows the many 
               fundamental freedom of speech sources and a hand that shows the people of the country 
               and how it is important to have access to freedom of speech.", 
               align = "left", height = 300, width = 500),
      
      p(" The Freedom Index is a metric used to 
      measure the level of freedom enjoyed by individuals in a country. This index is calculated using a number of
      factors, including political rights, civil liberties, and economic freedom. Over time, the level of freedom enjoyed
      by individuals in different countries around the world has changed significantly. While some countries have become 
      more democratic and free, others have become more authoritarian and oppressive."),
      p("To better understand the relationship between democracy and economic prosperity, we use data from",
        strong("Freedom House" ),
        "that measures democratization across the world based on GDP per Capita data from 2006 to 2022. Additionally, we analyze the 
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
      
      p("Below are the links to the orgins of the dataset. For reference, we then combined all of these datasets into one large csv file."),
      
      a(href = "https://freedomhouse.org/report/freedom-world", target = "_blank", 
        "Dataset 1"),
      a(href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD", target = "_blank", 
        "Dataset 2"),
      a(href = "https://data.worldbank.org/indicator/NY.GDP.MKTP.CD", target = "_blank", 
        "Dataset 3"),
      hr(),

      h2("Research Questions", align = "center"),
      br(),
      
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
      
      br(),
  
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
      br(),

      tableOutput("table_intro"),
    )
  })

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
        
        hr(),
        
        h3("Finding 2: Freedom in Asia"),
        br(),
        p("Asia is interesting because Asia has experienced immense economic growth from the 1990s till now. Based on the graph, free countries are much richer than not free or partially free countries in Asia.
          It is interesting to see that partially free countries are getting wealthier than not free countries. Moreover, the gap in GDP per capita between free countries and other countries is closing.
          This result suggests that hope for democracy is strongest in Asia right now. As seen in the table below, freedom scores across the regions are not rising while they get wealthier.
          While the table is skewed because  advances in the freedom of individual countries are drowned out by the average, it does show that freedom is not decreasing in Asia."),

        hr(),
        
        h4("Freedom Index Score and GDP Per Capita by Region between 2005 and 2021"),
        br(),
        tableOutput("gdp_freedom_region_table"),
        
        hr(),
        
        h3("Finding 3: Trends in Growth"),
        br(),
        p("As seen from the table, the entire world is getting richer. While this speed of this growth is spread unevenly across regions, it is also spread unevenly across free, not free, and partially free countries.
          Most pronounced in the Middle East, the Americas, and Africa, free countries are getting richer faster than not free or partially free countries. The gap in wealth is expanding.
          This is not true for all regions, mainly Asia.
          The table also shows that freedom is not rising across the world. It is not a fair analysis to look at average freedom scores across regions to make this judgement because the assessment of freedom is context-specific. 
          Regardless, major world events in countries around the world show that democracy might be in the retreat."),
        
        br(),
        hr(),
        
        h4(strong("Key Finding: free countries are usually richer than not free or partially free countries. A country getting richer does not perfectly correlate with it getting freerer."))
        
      ))
    
  })
  
  output$plot1 <- renderPlot({
    if (input$graph == 1) {
      freedom %>% filter (year == input$yr) %>% filter (status == "F") %>% 
        ggplot(aes(x = pr, y = cl)) + geom_point() + 
        geom_smooth(se = FALSE, method = "lm") + ggtitle ("Political Rights vs. Civil Liberty") +
        labs(x = "Political Rights Index Score", y = "Civil Liberty Index Score")
    } else if(input$graph == 2) {
      freedom %>% filter (year == input$yr) %>% filter(status == "F") %>% 
        ggplot(aes(x = pr)) + geom_histogram() + 
        ggtitle ("Political Rights Count") +
        labs(x = "Political Rights Index Score", y = "counts")
    } else if(input$graph == 3){
      freedom %>% filter (year == input$yr) %>% filter(status == "F") %>% 
        ggplot(aes(x = cl)) + geom_histogram() + 
        ggtitle ("Civil Liberty Count") +
        labs(x = "Civil Liberty Index Score", y = "counts")
    } else {
      freedom %>% filter (year == input$yr) %>% filter(status == "F") %>% 
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
  
  output$page3 <- renderUI({
    sidebarLayout(
      sidebarPanel(
        radioButtons("graph", "Which graph?", 
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
  
  freedom1 <- reactive({
    freedom %>%
      filter(region %in% input$regions)
  })
  
  output$plotgraydon <- renderPlot ({
    p1 <- freedom1() %>%
      filter(!is.na(year), !is.na(total)) %>%
      group_by(year, region) %>%
      summarize(avg_total = mean(total)) %>%
      ggplot(aes(x = year, y = avg_total, col = region)) +
      geom_point() + 
      labs(title = "Freedom Index by Region",
           x = "Years (2005 - 2021)",
           y = "Freedom Index") 
    
    p2 <- p1 + geom_smooth(method = "lm")
    
    if (input$cb) {
      p2
    }
    else{
      p1
    }
    
  })
  
  output$page4 <- renderUI({
    sidebarLayout(sidebarPanel(
      p("You can analyze the average Freedom Index for different regions. 
      Select the regions you are interested in. 
      To the right, you'll see a yearly scatterplot of the afformentioned data:",
        checkboxInput(
          inputId = "cb",
          label = "Display Trend Lines:",
          value = FALSE),
        checkboxGroupInput(
          inputId = "regions",
          label = "Regions to show:",
          choices = c(unique(freedom$region)),
          selected = "Asia"),
      )),
      mainPanel(plotOutput("plotgraydon"),
                h3("Findings:", align = "center"),
                p("Based on the scatterplot, we can see that there are marked differences between the different regions as it pertains to their Freedom Index score,
                  indicating that internal measures like civil rights & political liberties don't just differ on a country basis, they differ based on the region a 
                  country is located in."),

                p("Intuitively, this makes sense, since countries in the same location, although often having conflict due to things like shared interest or even something
                  as simple as general proxiity, generally share the same core values and interests. There are a variety of reasons we could postulate as to why this is the case
                  (inter-country trade making an overall region more wealthy, diffusion of values/interests between close countries, etc.), but we can see from the scatterplot
                  that differences in freedom appear on a region-wide level."),
                
                p("With that being said, let's describe what we see from the plot. Essentially, ", 
                  strong("Europe "),
                  "and the ",
                  strong("Americas"),
                  "have higher Freedom Index scores than the rest of the countries. ",
                  strong("Africa "),
                  "and ",
                  strong("Asia "),
                  "follow them, as they are more middle-of-the-pack in terms of Freedom Index scores. Finally, the ",
                  strong("Middle East "),
                  "and ",
                  strong("Eurasia"),
                  "alost completely overlap, meaning that on a region-scale, they have ",
                  em("very"),
                  "similar Freedom Index scores.")
              ))
  })
  
  output$plotlast <- renderPlot({
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
  
  output$page5 <- renderUI({
    mainPanel(
      h2("Notable Insights", align = "center"),
      br(),
      p("Insights: There are a couple of insights that we gained from the charts we have just presented"),
       p(strong("First, "),
       "countries that had a higer freedom index tended to have a higher GDP per capita. "),
       p(strong("Second, "),
         "another insight we see from our graphs is that those countries who do not use the
         democratic style of government have a lower freedom index. This can be because
         of the strict policies of civil liberties as seen in North Korea and China where 
         more authoritarian forms of governance reign supreme."),
       p(strong("Thirdly, "),
              "based on region, the countries which are surroundedby regions which have a higher freedom level also tend to have a higher freedom level.For example, the region of the Europe and Americas has more Free and Partially free than all other
         regions and this could be because the west has more democracy than other regions."), 
      p(strong("However,"),
        "we have also noticed that for regions like Europe, Americas, Africa, Eurasia and the Middle East, the 
         average total of freedom levels for the regions are dropping slightly which may be a concern. This 
         could also be a national security reason on why these averages are dropping as more governments are 
         more focused on privacy, especially in the tech world we are in now, and are making more policy changes
         which may lower civil liberties score."),
      hr(),
      
      h2("Chart that Demonstrates the Pattern/Insight", align = "center"),
      plotOutput("plotlast"),
      hr(),
      
      h2("Broader Implications of the Insight", align = "center"),
      p("The insights gained from the charts have several broader implications for policymakers, academics, and the general public."),
      p(strong("Firstly, "),
        "the correlation between a higher freedom index and a higher GDP per capita suggests that increasing political 
           freedoms and civil liberties can have a positive impact on a country's economic development. Therefore, policymakers can use 
           this information to prioritize policies that promote democratic values and human rights."),
      p(strong("Secondly, "),
        "the observation that countries with non-democratic styles of government tend to have lower freedom 
           indices highlights the importance of promoting democratic values and practices worldwide. By supporting 
           democratic institutions and civil society organizations, countries can help promote human rights and 
           freedom around the world."),
      p(strong("Thirdly, "),
        "the observation that the average freedom levels in certain regions 
           are dropping slightly raises concerns about the state of democracy and human rights in those regions. 
           Policymakers and civil society organizations can use this information to target their efforts towards 
           promoting democracy and civil liberties in those regions."),
      p(strong("Finally, "),
        "the observation that privacy concerns may be contributing to the drop in freedom levels 
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
          can be assumed by media representation."),
      p("In terms of the quality of the data, the Freedom House has only null values for those countries which they cannot 
          get an accurate data of. For example, those viewing the visualizations may 
          encounter some GDP missing due to the lack of information from that country, which 
          also could be a factor in the scorings of the country's freedom index."),
      p("Additionally, the group came up with some biases that we think plays a crucial role: 
          Freedom House Index is biased towards Western-style democracy, as it tends to measure 
          freedom based on a specific model of democracy that may not be appropriate for all countries and cultures. 
          the Index can be influenced by political biases, particularly when it comes to countries that are 
          perceived as being allies or adversaries of the United States."),
      p("The bias in the Freedom House Index can harm various population groups, particularly those in non-Western countries or countries with different cultural 
          and political traditions. For example, if the Index is biased towards Western-style democracy, 
          it may undervalue the level of freedom in countries with different forms of government, 
          potentially harming populations in those countries. Similarly, if the Index is influenced 
          by political biases, it could lead to unfair assessments of countries that are perceived 
          as adversaries of the United States, potentially harming populations in those countries as well (causing more
          discrimination)."),
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

# Run the application 
shinyApp(ui = ui, server = server)
