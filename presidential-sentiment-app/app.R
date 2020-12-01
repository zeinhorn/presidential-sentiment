#presidents

library(shiny)
library(tidyverse)
library(Rcrawler)

Rcrawler("https://millercenter.org/the-presidency/presidential-speeches")

filtered <- INDEX %>% filter(str_detect(Url, "https://millercenter.org/the-presidency/presidential-speeches/.*"))
filtered <- filtered %>% select(Url)
write.csv(filtered, "/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/speech-urls.csv")

ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)