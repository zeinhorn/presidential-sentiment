#presidents

#Loading libraries
library(shiny)
library(tidyverse)
library(Rcrawler)
library(rvest)
library(textdata)
library(SentimentAnalysis)
library(dplyr)
library(tidytext)
library(lubridate)
library(ggrepel)
library(shinycssloaders)

#Reading in and preparing speech + political party data
speech.party <- read_csv("speech_party_summary.csv") 
speech.party <- speech.party [ ,-1] %>%
  distinct()
speech.party$date <- strptime(as.character(speech.party$date), "%B %d, %Y")
speech.party$date <- as.Date(speech.party$date)

#Graph of speech + political party 
speech.party %>%
  ggplot(aes(x = date,
             y = sentiment.value)) +
  geom_col(aes(color = name))

#Reading in and preparing major events data
events <- read_csv("Major_events.csv")
events$Year <- strptime(as.character(events$Year), "%F")
events$Year <- as.Date(events$Year)
events.test <- events[1:2, ]

#Graph of speech/political party with major events
speech.party %>%
  ggplot() +
  geom_col(data= speech.party,
           aes(x = date,
               y = sentiment.value,
               color = name)) +
  geom_point(data = events,
             aes(x = Year,
                 y = 0))

#Finding average sentiment value by president
speech.by.pres <- speech.party %>%
  group_by(name) %>%
  filter(!is.na(sentiment.value)) %>%
  mutate(president.sentiment = mean(sentiment.value))
speech.by.pres <- speech.by.pres %>%
  select(name, president.sentiment) %>%
  distinct()

#Making new data frames with highest and lowest average sentiment values
pres.high.5 <- speech.by.pres %>%
  arrange(desc(president.sentiment)) %>%
  head(5)
pres.low.5 <- speech.by.pres %>%
  arrange(president.sentiment) %>%
  head(5)

#Finding average sentiment for each political party
speech.party.2 <- speech.party %>%
  select(sentiment.value, Party) %>%
  group_by(Party) %>%
  filter(!is.na(sentiment.value)) %>%
  mutate(party.sentiment = mean(sentiment.value)) %>%
  mutate(min.party.sentiment = min(sentiment.value)) %>%
  mutate(max.party.sentiment = max(sentiment.value)) %>%
  mutate(med.party.sentiment = median(sentiment.value))
speech.party.final <- speech.party.2 %>%
  select(Party, party.sentiment, min.party.sentiment, max.party.sentiment,
         med.party.sentiment) %>%
  distinct() %>%
  arrange(desc(party.sentiment))

ui <- navbarPage("Sentiment Analysis of US Presidents' Speeches",
                 tabPanel("Introduction",
                          verbatimTextOutput(outputId = "intro")),
                 tabPanel("Graph",
                          
                          #Directions for the graph
                          verbatimTextOutput(outputId = "directions"),
                          
                          #Choosing to color the graph by president or by political party
                          selectizeInput(inputId = "var1",
                                         label = "Choose What to Color By",
                                         choices = c("President" = "name",
                                                     "Political Party" = "Party"),
                                         selected = "name"),
                          
                          #Choosing what events to show on the graph
                          selectizeInput(inputId = "var2",
                                         label = "Choose One Major Event to Show",
                                         choices = levels(factor(events$Event)),
                                         selected = "Declaration of Independence"),
                          
                          #Creating the two graphs - second one is the first zoomed in on one part
                          fluidRow(h4("The Top Plot Controls The Bottom Plot")),
                          fluidRow(plotOutput("plot2", height = 600,
                                              brush = brushOpts(
                                                id = "plot2_brush",
                                                resetOnNew = TRUE
                                              )) %>% withSpinner(color = "lightblue")),
                          fluidRow(plotOutput("plot3", height = 600, brush = "plot3_brush")),
                          fluidRow(
                            verbatimTextOutput(outputId = "info", placeholder = TRUE)
                          )
                 ),
                 tabPanel("Table",
                          verbatimTextOutput(outputId = "results"),
                          selectizeInput(inputId = "var3",
                                         label = "Choose Two or More Presidents' Sentiment Values to Compare",
                                         choices = levels(factor(speech.by.pres$name)),
                                         multiple = TRUE,
                                         selected = "George Washington"),
                          tableOutput(outputId = "interactive.table"),
                          textOutput(outputId = "toptitle"),
                          tableOutput(outputId = "topfive"),
                          textOutput(outputId = "bottomtitle"),
                          tableOutput(outputId = "bottomfive"),
                          textOutput(outputId = "partytitle"),
                          tableOutput(outputId = "partysentiment")),
                 tabPanel("Conclusion",
                          verbatimTextOutput(outputId = "conclusion"))
)

server <- function(input, output, session) {
  
  output$intro <- renderText({
    "Maggie Reynolds, Mia Kojima, Zack Einhorn
   MATH0216 - Final Project
   12/09/20
   
   The data set we made consists of all U.S. presidents by name, all their speeches,
   the date the speeches were given, the average AFINN sentiment value of the speeches, 
   the presidents' political party, and the presidents' overall average sentiment score.
   
   The goals of this report are to compare how positive or negative different presidents' 
   speeches were over time, identify the overall most positive and negative presidents, 
   recognize any relation between political party and average AFINN score (if any), 
   and observe how significant events may have shifted speech sentiment. We also hypothesize
   that the sentiment values will provide insight to the emotional state at the time
   a speech was given.

   
   'The AFINN lexicon is a list of English terms manually rated for valence with 
   an integer between -5 (negative) and +5 (positive) by Finn Årup Nielsen between 
   2009 and 2011.'
    
   Finn Årup Nielsen, “A new ANEW: evaluation of a word list for sentiment analysis 
   in microblogs”, Proceedings of the ESWC2011 Workshop on ‘Making Sense of Microposts’:
   Big things come in small packages. Volume 718 in CEUR Workshop Proceedings: 93-98.
   2011 May. Matthew Rowe, Milan Stankovic, Aba-Sah Dadzie, Mariann Hardey (editors)"
   })
    
  output$directions <- renderText({
    "     Directions: Highlight sections of the first graph with your mouse to see a 
     zoomed in image on the second graph. Highlight sections of the second graph 
     to see the president's name, the date the speech was given, and a summary.
     
     The graph can be colored by either presidents' names or political parties by
     using the first drop down menu. You can also choose from a short list of important
     American events to plot on the graph.
     
     Note: There are only 44 names listed due to Grover Cleveland serving two 
     nonconsecutive terms. The speeches from both terms are combined."
  })
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  #Setting up for graph of speeches and selected events 
  output$plot2 <- renderPlot({
    cdChoice <-input$var2
    eventselected <- events %>%
      filter(Event == cdChoice)
    
    #Creating graph of speeches and selected events
    ggplot(speech.party, aes(x = date,
                             y = sentiment.value)) +
      geom_col(aes_string(color = input$var1)) +
      geom_point(data = eventselected,
                 aes(x = Year, y = 0),
                 size = 3) +
      xlab("Year") +
      ylab("Sentiment Value") +
      ggtitle("President Speeches Over Time")
  })
  
  #Setting up for the zoomed in graph of speeches and selected events
  output$plot3 <- renderPlot({
    cdChoice <-input$var2
    eventselected <- events %>%
      filter(Event == cdChoice)
    
    #Creating zoomed in graph of speeches and selected events
    ggplot(speech.party, aes(x = date,
                             y = sentiment.value)) +
      geom_col(aes_string(color = input$var1),
               show.legend = FALSE) +
      geom_point(data = eventselected,
                 aes(x = Year, y = 0),
                 size = 3)+
      xlab("Year") +
      ylab("Sentiment Value") +
      ggtitle("Zoomed In: President Speeches Over Time") +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })
  
  #Using plot brush to look for selections on top graph
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(as.Date(brush$xmin, origin="1970-01-01"), as.Date(brush$xmax, origin="1970-01-01"))
      ranges2$y <- c(-2.8, 5.5)
    } 
    else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  #Using plot brush to look for selections on bottom graph & displaying relevant
  #speeches in a text output
  observe({
    poi <- speech.party %>% filter(name == "Zachary Taylor")
    min <- 8700
    max <- 8800
    if(!is.null(input$plot3_brush)){
      min <- floor(as.numeric(input$plot3_brush$xmin))
      max <- floor(as.numeric(input$plot3_brush$xmax))
    }
    poi <- speech.party %>% filter(date > min & date < max) %>% arrange(date)
    
    
    #Making for loop to display date and summary for person of interest (poi)
    printText <- "Select dates on graph above to view summary of the speeches."
    for(i in 1:as.numeric(nrow(poi))){
      date.number <- as.numeric(poi[i,3])
      date.text <- as.Date(date.number, origin="1970-01-01")
      name <- toString(poi[i,2])
      summary <- toString(poi[i,6])
      printText <- paste(paste(paste(printText, "\n"),name,paste(date.text,summary)))
    }
    output$info <- renderText({
      printText
    })
  })
  
  output$results <- renderText({
    "     This page includes a tool for users to quantitatively compare the average 
    sentiment values of individual presidents of their choice.
    
    There is also a quantitative analysis that ranks individual presidents by sentiment 
    value. Below that is a table that ranks average political party sentiment value. 
    The parties' minimum, maximum, and median sentiment values are also listed."
  })
  #Making a table of each president's average sentiment
  output$interactive.table <- renderTable({
    speech.by.pres %>%
      filter(c(name %in% input$var3)) %>%
      arrange(president.sentiment) %>%
      rename("Name" = "name",
             "Sentiment Value" = "president.sentiment")
  })
  
  #Making a title for highest presidents' sentiments
  output$toptitle <- renderText({
    "Presidents With Five Highest Average Sentiment Values"
  })
  
  #Making a table of highest and lowest five president's sentiments
  output$topfive <- renderTable({
    pres.high.5 %>%
      rename("Name" = "name",
             "Sentiment Value" = "president.sentiment")
  })
  
  #Making a title for lowest presidents' sentiments
  output$bottomtitle <- renderText({
    "Presidents With Five Lowest Average Sentiment Values"
  })
  
  output$bottomfive <- renderTable({
    pres.low.5 %>%
      rename("Name" = "name",
             "Sentiment Value" = "president.sentiment")
  })
  
  #Making a title and table for average sentiment by party
  output$partytitle <- renderText({
    "Average Sentiment Value of Each Political Party"
  })

    output$partysentiment <- renderTable({
      speech.party.final %>%
        rename("Political Party" = "Party",
               "Sentiment Value" = "party.sentiment",
               "Minimum Party Sentiment" = "min.party.sentiment",
               "Median Party Sentiment" = "med.party.sentiment",
               "Maximum Party Sentiment" = "max.party.sentiment")
    })

  
  
  #Conclusion statement
  output$conclusion <- renderText({
    "     Each president has a different personality, speaking style, and brand.
    However, based on our examination of each president's sentiment value and the 
    impact of major events on their sentiment value, we concluded that major events 
    have a greater influence on sentiment value than a president's disposition does. 
    For example, Lincoln and FDR were president through two of the most stressful 
    periods in our country's history and had two of the lowest sentiment values. 
    
    The presidents with the most positive and negative sentiment values were all
    over 75 years ago; FDR is the most recent president in either the five highest or 
    five lowest sentiment values. This suggests that presidents have become
    more emotional and extreme in their style of speeches. They tend to give speeches
    that are both extremely positive and negative, giving them average sentiment values
    closer to zero. This wider range of emotions is also supported by comparing minimum and 
    maximum values of the two modern parties (Democrats and Republicans) to the four older
    ones. Modern parties skew higher in both their mins and maxes while their medians
    trend closer to zero.
    
    Our biggest takeaway is that based on their AFINN scores, presidential speeches are 
    limited in their emotions. The speeches range from around -2.5 to 2.5, so they only
    take up half of the scale (-5 to 5). Even the most emotional speeches stick to this range.
    However, we are looking at these scores in a comparison of their raw text. An audience
    member would not pick up on a small change of a speech's AFINN score because delivery
    and context are also powerful determinants of emotional receptance. So, while this 
    measure is useful for analyzing the text of speeches, and the speeches do react slightly
    to current events, sentiment value is not an accurate measure of the emotional state of
    America when the speech was given."
    
  })
}

shinyApp(ui, server)
