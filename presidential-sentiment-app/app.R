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

#Rcrawler(Website = "https://millercenter.org/the-presidency/presidential-speeches", dataUrlfilter = "https://millercenter.org/the-presidency/presidential-speeches/.*")
#filtered3 <- INDEX %>% filter(str_detect(Url, "https://millercenter.org/the-presidency/presidential-speeches/.*"))
#filtered3 <- filtered3 %>% select(Url)
#write.csv(filtered3, "/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/speech-urls3.csv")
speeches <- read.csv("speech-urls3.csv")
speeches <- speeches%>%
  select(Url)
Url <- speeches[1, ]

find.sentiment.value <- function(Url){
html.text <- Url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text()
html.data <- data.frame(text = html.text,
                        stringsAsFactors = FALSE)
unnested.text <- html.data %>%
  unnest_tokens("word", "text") %>%
  anti_join(stop_words, by = "word")
sentiment.by.word <- unnested.text %>%
  count(word) %>%
  left_join(get_sentiments("afinn"), by = "word") %>% 
  filter(!is.na(value)) %>%
  mutate(total.value = n*value)
avg.sentiment <- sentiment.by.word %>%
  summarise(average.sentiment = mean(total.value))
}
find.sentiment.value(speeches[1, ])




ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)