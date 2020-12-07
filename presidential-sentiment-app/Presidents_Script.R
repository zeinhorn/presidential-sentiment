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

for (i in 2:20){
  new.row <- find.sentiment.value(speeches[i, ])
  speeches.info <- rbind(speeches.info, new.row)
  print(i/as.numeric(nrow(speeches)))
}



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
