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

ui <- fluidPage(
  
  #Choosing to color the graph by president or by political party
  selectizeInput(inputId = "var1",
                 label = "Choose What to Color By",
                 choices = c("President" = "name",
                             "Political Party" = "Party"),
                 selected = "name"),
  
  #Choosing what events to show on the graph
  selectizeInput(inputId = "var2",
                 label = "Choose up to Two Major Events to Show",
                 multiple=TRUE,
                 choices = levels(factor(events$Event)),
                 selected = "Declaration of Independence"),
  
  #Creating the two graphs - second one is the first zoomed in on one part
  fluidRow(h4("The Top Plot Controls The Bottom Plot")),
  fluidRow(plotOutput("plot2", height = 600,
                      brush = brushOpts(
                        id = "plot2_brush",
                        resetOnNew = TRUE
                      ))),
  fluidRow(plotOutput("plot3", height = 600, brush = "plot3_brush")),
  fluidRow(
    verbatimTextOutput(outputId = "info", placeholder = TRUE)
  )
)

server <- function(input, output, session) {
  
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
                 aes(x = Year, y = 0))+
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
                 aes(x = Year, y = 0))+
      xlab("Year") +
      ylab("Sentiment Value") +
      ggtitle("Zoomed In: President Speeches Over Time") +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
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
  
  
  
  observe({
    poi <- speech.party %>% filter(name == "Zachary Taylor")
    min <- 8700
    max <- 8800
    if(!is.null(input$plot3_brush)){
      min <- floor(as.numeric(input$plot3_brush$xmin))
      max <- floor(as.numeric(input$plot3_brush$xmax))
    }
    
    
    
    print("change")
    print(as.numeric(nrow(poi)))
    poi <- speech.party %>% filter(date > min & date < max)
    
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
  
  
  
  
  as.Date(-44193, origin="1970-01-01")
  
  
}

shinyApp(ui, server)
