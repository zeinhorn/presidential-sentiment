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

#find.sentiment.value(url)
#in: url representing a president's speech from https://millercenter.org/the-presidency/
#out: data frame containing the name of the president, the date the speech was given, and the speech's sentiment value 
find.sentiment.value <- function(Url){
  html.text <- Url %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text()
  html.data <- data.frame(text = html.text,
                          stringsAsFactors = FALSE)
  
  last.row <- as.numeric(nrow(html.data))
  
  #clean up data
  html.data <- data.frame(text = html.data[2:(last.row-2),],
                          stringsAsFactors = FALSE)
  
  #grab name and date
  name.date <- data.frame(text = html.data[1:2,],
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
  avg.sentiment.value <- avg.sentiment[1,1]
  
  return(data.frame(name = name.date[1,], date = name.date[2,], sentiment.value = avg.sentiment.value,
                    stringsAsFactors = FALSE))
}

speeches.info <- find.sentiment.value(speeches[1, ])

for (i in 2:as.numeric(nrow(speeches))){
  new.row <- find.sentiment.value(speeches[i, ])
  speeches.info <- rbind(speeches.info, new.row)
  print(i/as.numeric(nrow(speeches)))
}
write.csv(speeches.info, "/Users/ingridsorensen/Desktop/DataScience/FoodDeserts/presidential-sentiment/speeches_info.csv")
#rename folders if it doesn't work
ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)