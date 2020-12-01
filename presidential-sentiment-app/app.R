#presidents

library(shiny)
library(tidyverse)
library(Rcrawler)

Rcrawler(Website = "https://millercenter.org/the-presidency/presidential-speeches", dataUrlfilter = "https://millercenter.org/the-presidency/presidential-speeches/.*")

filtered <- INDEX %>% filter(str_detect(Url, "https://millercenter.org/the-presidency/presidential-speeches/.*"))
filtered <- filtered %>% select(Url)
write.csv(filtered, "/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/speech-urls.csv")

speech_urls <- read_csv("/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/speech-urls.csv")

filtered2 <- INDEX %>% filter(str_detect(Url, "https://millercenter.org/the-presidency/presidential-speeches/.*"))
filtered2 <- filtered2 %>% select(Url)
write.csv(filtered2, "/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/speech-urls2.csv")

speech_urls2 <- read_csv("/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/speech-urls2.csv")

filtered3 <- INDEX %>% filter(str_detect(Url, "https://millercenter.org/the-presidency/presidential-speeches/.*"))
filtered3 <- filtered3 %>% select(Url)
write.csv(filtered3, "/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/speech-urls3.csv")


ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)