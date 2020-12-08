#Rcrawler(Website = "https://millercenter.org/the-presidency/presidential-speeches", dataUrlfilter = "https://millercenter.org/the-presidency/presidential-speeches/.*")
#filtered3 <- INDEX %>% filter(str_detect(Url, "https://millercenter.org/the-presidency/presidential-speeches/.*"))
#filtered3 <- filtered3 %>% select(Url)
#write.csv(filtered3, "/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/speech-urls3.csv")
speeches <- read.csv("speech-urls3.csv")
speeches <- speeches%>%
  select(Url)
Url <- speeches[190, ] #50

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

for (i in 2:20){
  new.row <- find.sentiment.value(speeches[i, ])
  speeches.info <- rbind(speeches.info, new.row)
  print(i/as.numeric(nrow(speeches)))
}

#grab.summary(url)
#in: url representing a president's speech from https://millercenter.org/the-presidency/
#out: data frame containing a summary of the speech, if there, along with the date the speech was given
grab.summary <- function(Url){
  html.text <- Url %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text()
  html.data <- data.frame(text = html.text,
                          stringsAsFactors = FALSE)
  
  #clean up data
  summary <- data.frame(text = html.data[1:4,],
                        stringsAsFactors = FALSE)
  char3 <- as.numeric(nchar(summary[3,1]))
  char4 <- as.numeric(nchar(summary[4,1]))
  if(char3<1000 & char4<1000 & (char3>60 | char4>60)){
    if(char4>char3){
      value <- data.frame(date = summary[3,1], text = summary[4,1], 
                          stringsAsFactors = FALSE)
    }else{
      value <- data.frame(date = summary[2,1], text = summary[3,1],
                          stringsAsFactors = FALSE)
    }
  }else{
    value <- data.frame(date = summary[3,1], text = "There is no summary for this speech.",
                        stringsAsFactors = FALSE)
  }
  return(value)
}

speeches.summary <- grab.summary(speeches[1, ])

for (i in 2:as.numeric(nrow(speeches))){
  new.row <- grab.summary(speeches[i, ])
  speeches.summary <- rbind(speeches.summary, new.row)
  if(i %% 10 == 0){
    print(i)
  }
}

speeches.summary.backup <- speeches.summary
speeches.summary <- speeches.summary %>% distinct()

write.csv(speeches.summary, "/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/speech-summary.csv")


#Read in president table
pres.party.url <- "https://enchantedlearning.com/history/us/pres/list.shtml#:~:text=1%20George%20Washington%20%281732-1799%29%20None%2C%20Federalist%201789-1797%20John,1809-1817%20George%20Clinton%2C%20Elbridge%20Gerry%20More%20items...%20s"
pres.party.list <- pres.party.url%>%
  read_html() %>%
  html_nodes(xpath= '/html/body/font/table[2]')%>%
  html_table(fill= TRUE)
pres.party.data <- pres.party.list[[1]]
pres.party.data <-pres.party.data [ , 1:2]

pres.party.data$President <- str_replace_all(pres.party.data$President,
                                             "\\([:digit:]{4}.*\\)",
                                             "")
pres.party.data$President <- str_replace_all(pres.party.data$President,
                                             "[:digit:]+\\.",
                                             "")
write.csv(pres.party.data,"C:/Users/imias/OneDrive/MATH0216/Final Project/presidential-sentiment/presidential-sentiment-app/pres_party.csv")

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


#Joinging party and speech
speeches <- read_csv("speeches_info_2.csv")
pres.party.data<- read_csv("pres_party.csv")
speech.party <- speeches%>%
  left_join(pres.party.data,
            by= c("name" = "President"))
write.csv(speech.party,"C:/Users/imias/OneDrive/MATH0216/Final Project/presidential-sentiment/presidential-sentiment-app/speech_party.csv")

#joining party/speech with summary
speech.party <- read_csv("speech_party.csv") 
speech.party <- speech.party [ ,-1] %>%
  distinct()

speech.summary <- read_csv("speech-summary.csv") 
speech.summary <- speech.summary [ ,-1] %>%
  distinct()

speech.party.summary <- speech.party%>%
  left_join(speech.summary,
            by= "date")

speech.party.summary <- speech.party.summary %>% distinct(date, .keep_all = TRUE)

write.csv(speech.party.summary,"/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/presidential-sentiment-app/speech_party_summary.csv")


