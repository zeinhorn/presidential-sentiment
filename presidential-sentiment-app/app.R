#presidents

library(shiny)
library(tidyverse)
library(Rcrawler)
library(rvest)
library(textdata)
#install.packages("SentimentAnalysis")
library(SentimentAnalysis)
library(dplyr)
library(tidytext)

#write.csv(speeches.info, "/Users/ingridsorensen/Desktop/DataScience/FoodDeserts/presidential-sentiment/speeches_info.csv")
speech <- read.csv("speeches_info_2.csv")
speech <- speech %>%
  distinct()
#Changing format of dates column
as.Date("December 06, 2020", format = "%B %d, %Y")
for (i in 1:as.numeric(nrow(speech))){
  speech[i, 2] <- as.Date(toString(speech[i, 2]), format = "%B %d, %Y")
  #as.Date(as.numeric(useful.date), origin = "1970-01-01")
}

#Making a graph of the dates and sentiment
speech$date <- as.numeric(speech$date)
speech %>%
  ggplot(aes(x = date,
             y = sentiment.value)) +
  geom_col(aes(color = name))
ui <- fluidPage(
  fluidRow(h4("Left plot controls right plot")),
  fluidRow(plotOutput("plot2", height = 300,
                      brush = brushOpts(
                        id = "plot2_brush",
                        resetOnNew = TRUE
                      ))),
  fluidRow(plotOutput("plot3", height = 300)
  )
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
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(-2.8, 5)
    } 
    else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
}

shinyApp(ui, server)