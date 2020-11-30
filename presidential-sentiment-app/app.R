#presidents

library(shiny)
library(tidyverse)
library(Rcrawler)

Rcrawler("https://millercenter.org/the-presidency/presidential-speeches")

filtered <- INDEX %>% filter(str_detect(Url, "https://millercenter.org/the-presidency/presidential-speeches/.*"))
filtered <- filtered %>% select(Url)

ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)