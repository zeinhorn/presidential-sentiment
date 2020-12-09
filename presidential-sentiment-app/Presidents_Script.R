#This script is the code we wrote to make the data set we used in our shiny app.

#Scraping in all speeches from website and saving as a csv
#Rcrawler(Website = "https://millercenter.org/the-presidency/presidential-speeches", dataUrlfilter = "https://millercenter.org/the-presidency/presidential-speeches/.*")
#filtered3 <- INDEX %>% filter(str_detect(Url, "https://millercenter.org/the-presidency/presidential-speeches/.*"))
#filtered3 <- filtered3 %>% select(Url)
#write.csv(filtered3, "/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/speech-urls3.csv")

#Reading in csv of speeches and choosing relevant columns
speeches <- read.csv("speech-urls3.csv")
speeches <- speeches%>%
  select(Url)
Url <- speeches[190, ]

#Writing a function for president speeches and sentiment value
#find.sentiment.value(url)
#in: url representing a president's speech from https://millercenter.org/the-presidency/
#out: data frame containing the name of the president, the date the speech was given, and the speech's sentiment value 
find.sentiment.value <- function(Url){
  
  #Reading the html file of urls and converting to a data frame
  html.text <- Url %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text()
  html.data <- data.frame(text = html.text,
                          stringsAsFactors = FALSE)
  
  #Making the data numeric
  last.row <- as.numeric(nrow(html.data))
  
  #Cleaning data to only include desired information
  html.data <- data.frame(text = html.data[2:(last.row-2),],
                          stringsAsFactors = FALSE)
  
  #Selecting names and dates of presidents from data frame
  name.date <- data.frame(text = html.data[1:2,],
                          stringsAsFactors = FALSE)
  
  #Unnesting tokens and joining text with the "afinn" sentiments package
  unnested.text <- html.data %>%
    unnest_tokens("word", "text") %>%
    anti_join(stop_words, by = "word")
  sentiment.by.word <- unnested.text %>%
    count(word) %>%
    left_join(get_sentiments("afinn"), by = "word") %>% 
    filter(!is.na(value)) %>%
    mutate(total.value = n*value)
  
  #Finding the average sentiment in each speech
  avg.sentiment <- sentiment.by.word %>%
    summarise(average.sentiment = mean(total.value))
  avg.sentiment.value <- avg.sentiment[1,1]
  
  #Output from function
  return(data.frame(name = name.date[1,], date = name.date[2,], sentiment.value = avg.sentiment.value,
                    stringsAsFactors = FALSE))
}

#Selecting relevant column from data frame
speeches.info <- find.sentiment.value(speeches[1, ])

#Making a for loop for all of the speeches
for (i in 2:as.numeric(nrow(speeches))){
  new.row <- find.sentiment.value(speeches[i, ])
  speeches.info <- rbind(speeches.info, new.row)
  print(i/as.numeric(nrow(speeches)))
}

#Making a function that grabs the president's name and speech's summary
#grab.summary(url)
#in: url representing a president's speech from https://millercenter.org/the-presidency/
#out: data frame containing a summary of the speech, if there, along with the date the speech was given
grab.summary <- function(Url){
  
  #Reading the html file of urls and converting to a data frame
  html.text <- Url %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text()
  html.data <- data.frame(text = html.text,
                          stringsAsFactors = FALSE)
  
  #Identifying all possible columns with summaries of speeches
  summary <- data.frame(text = html.data[1:4,],
                        stringsAsFactors = FALSE)
  char3 <- as.numeric(nchar(summary[3,1]))
  char4 <- as.numeric(nchar(summary[4,1]))
  
  #Finding summaries by selecting text between a certain character count
  if(char3<1000 & char4<1000 & (char3>60 | char4>60)){
    if(char4>char3){
      value <- data.frame(date = summary[3,1], text = summary[4,1], 
                          stringsAsFactors = FALSE)
      
      #Saves the summary if between the character count
    }else{
      value <- data.frame(date = summary[2,1], text = summary[3,1],
                          stringsAsFactors = FALSE)
    }
    
    #Says no summary if nothing is found between the character count
  }else{
    value <- data.frame(date = summary[3,1], text = "There is no summary for this speech.",
                        stringsAsFactors = FALSE)
  }
  
  #Returns a data frame with the date and the summary
  return(value)
}

#For loop to get the summaries of all speeches
speeches.summary <- grab.summary(speeches[1, ])
for (i in 2:as.numeric(nrow(speeches))){
  new.row <- grab.summary(speeches[i, ])
  speeches.summary <- rbind(speeches.summary, new.row)
  if(i %% 10 == 0){
    print(i)
  }
}

#Filtering for distinct summaries and saving as a csv
speeches.summary.backup <- speeches.summary
speeches.summary <- speeches.summary %>% distinct()
write.csv(speeches.summary, "/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/speech-summary.csv")


#Reading in and cleaning up political party data
pres.party.url <- "https://enchantedlearning.com/history/us/pres/list.shtml#:~:text=1%20George%20Washington%20%281732-1799%29%20None%2C%20Federalist%201789-1797%20John,1809-1817%20George%20Clinton%2C%20Elbridge%20Gerry%20More%20items...%20s"
pres.party.list <- pres.party.url%>%
  read_html() %>%
  html_nodes(xpath= '/html/body/font/table[2]')%>%
  html_table(fill= TRUE)
pres.party.data <- pres.party.list[[1]]
pres.party.data <-pres.party.data [ , 1:2]

#Removing unnecessary dates and numbers from data and saving as a csv
pres.party.data$President <- str_replace_all(pres.party.data$President,
                                             "\\([:digit:]{4}.*\\)",
                                             "")
pres.party.data$President <- str_replace_all(pres.party.data$President,
                                             "[:digit:]+\\.",
                                             "")
write.csv(pres.party.data,"C:/Users/imias/OneDrive/MATH0216/Final Project/presidential-sentiment/presidential-sentiment-app/pres_party.csv")
#write.csv(speeches.info, "/Users/ingridsorensen/Desktop/DataScience/FoodDeserts/presidential-sentiment/speeches_info.csv")

#Cleaning speeches data: filtering for distinct speeches and re-formatting dates
speech <- read.csv("speeches_info_2.csv") %>%
  distinct()
speech$date <- strptime(as.character(speech$date), "%B %d, %Y")
speech$date <- as.Date(speech$date)

#Making a graph of the dates and sentiment
speech %>%
  ggplot(aes(x = date,
             y = sentiment.value)) +
  geom_col(aes(color = name))


#Joining political party and speeches data, converting to a csv
speeches <- read_csv("speeches_info_2.csv")
pres.party.data<- read_csv("pres_party.csv")
speech.party <- speeches%>%
  left_join(pres.party.data,
            by= c("name" = "President"))
write.csv(speech.party,"C:/Users/imias/OneDrive/MATH0216/Final Project/presidential-sentiment/presidential-sentiment-app/speech_party.csv")

#Joining political party/speech data with summaries of the speeches
speech.party <- read_csv("speech_party.csv") 
speech.party <- speech.party [ ,-1] %>%
  distinct()
speech.party.summary <- speech.party%>%
  left_join(speech.summary,
            by= "date")

#Cleaning joined data to only include distinct elements and saving as a csv
speech.summary <- read_csv("speech-summary.csv") 
speech.summary <- speech.summary [ ,-1] %>%
  distinct()
speech.party.summary <- speech.party.summary %>% distinct(date, .keep_all = TRUE)

write.csv(speech.party.summary,"/Users/zeinhorn/Documents/school/college/senior/ds/Final_project/presidential-sentiment/presidential-sentiment-app/speech_party_summary.csv")


