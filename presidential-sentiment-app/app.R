#presidents

#install.packages("SentimentAnalysis")

library(shiny)
library(tidyverse)
library(Rcrawler)
library(rvest)
library(textdata)
library(SentimentAnalysis)
library(dplyr)
library(tidytext)
library(lubridate)

#write.csv(speeches.info, "/Users/ingridsorensen/Desktop/DataScience/FoodDeserts/presidential-sentiment/speeches_info.csv")
speech <- read.csv("speeches_info_2.csv") %>%
  distinct()

speech$date <- strptime(as.character(speech$date), "%B %d, %Y")
speech$date <- as.Date(speech$date)

#Making a graph of the dates and sentiment
speech %>%
  ggplot(aes(x = date,
             y = sentiment.value)) +
  geom_col(aes(color = name))

ui <- fluidPage(
  fluidRow(h4("Top plot controls bottom plot")),
  fluidRow(plotOutput("plot2", height = 600,
                      brush = brushOpts(
                        id = "plot2_brush",
                        resetOnNew = TRUE
                      ))),
  fluidRow(plotOutput("plot3", height = 600, brush = "plot3_brush")),
  verbatimTextOutput("info")
)

server <- function(input, output, session) {
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    ggplot(speech, aes(x = date,
                       y = sentiment.value)) +
      geom_col(aes(color = name))
  })
  
  output$plot3 <- renderPlot({
    ggplot(speech, aes(x = date,
                       y = sentiment.value)) +
      geom_col(aes(color = name)) +
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
  
  output$info <- renderText({
    # x <- input$plot3_click$x
    # print(speech %>% filter(date > floor(as.numeric(x-2)) & date < floor(as.numeric(x+2))))
    min <- floor(as.numeric(input$plot3_brush$xmin))
    max <- floor(as.numeric(input$plot3_brush$xmax))
    toString(speech %>% filter(date > min & date < max))
    
  })
  
  as.Date(-44193, origin="1970-01-01")
  
  
}

shinyApp(ui, server)