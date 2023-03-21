### Ordoptælling Jobopslag ### 

setwd("path")

library(pdftools)
library(tidytext)
library(dplyr)
library(stringr)
library(stopwords)
library(tidyverse)

#Henter filen fra computeren 

files <- list.files(pattern = "pdf$")

opinions <- lapply(files, pdf_text)

texts<-pdf_text("job_beskrivelser.pdf") %>% readr::read_file()


#### Ordoptælling #### 

#Laver en tibble
text_df <- tibble(line = 1:4, text = texts)

#Laver alle ord til 1 observation
job_beskrivelser<-text_df %>%
  unnest_tokens(word, text)

#Laver en liste med danske stopord

word_dk<-stopwords("da", source = "stopwords-iso") %>%
  head(n = 170)
stopord<-as.data.frame(word_dk)

#Laver en liste med engelske stopord

word<-stopwords("en", source = "stopwords-iso") %>%
  head(n = 170)
stopwords<-as.data.frame(word)

#Fjerner stopord fra teksten
tekst_enstopwords<-job_beskrivelser %>% anti_join(stopwords)

#Optæller ord
ordoptælling<-tekst_enstopwords %>%
  count(word, sort = TRUE) 

#Henter den ned som csv 
write.csv(ordoptælling, "ordoptælling.csv", row.names = FALSE)

#### Data cleaning ####

#Henter csv filen ind 
ordoptælling <- read_delim("ordoptælling.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Tager de første 100 ord 
ordoptælling<-ordoptælling[1:100,]

# Erstatter "power" med "power-bi"
ordoptælling$word <- gsub("power", "power-bi", ordoptælling$word)

## Finder variationer af det samme ord ##

matching_words_team <- grep("^team\\w*", ordoptælling$word, value = TRUE)
matching_words_analy <- grep("^analy\\w*", ordoptælling$word, value = TRUE)
matching_words_udvikl<- grep("^udvikl\\w*", ordoptælling$word, value = TRUE)
matching_words_løs<-grep("^løs\\w*", ordoptælling$word, value = TRUE)
matching_words_customer<-grep("^customer\\w*", ordoptælling$word, value = TRUE)

# Matcher alle variationer af ordet "team"
team_pattern <- "\\bteam\\w*\\b"
team_rows <- grep(team_pattern, ordoptælling$word, value = TRUE)

# Sum af ordet "team" variationer
team_count <- sum(ordoptælling$n[ordoptælling$word %in% team_rows])

# Laver ny række med summen
new_row_team <- data.frame(word = "team (alle variationer)", n = team_count)

# Matcher alle variationer af ordet "analy"
analy_pattern <- "\\banaly\\w*\\b"
analy_rows <- grep(analy_pattern, ordoptælling$word, value = TRUE)

# Sum af ordet "analy" variationer
analy_count <- sum(ordoptælling$n[ordoptælling$word %in% analy_rows])

# Laver ny række med summen
new_row_analy <- data.frame(word = "analyse (alle variationer)", n = analy_count)

# Matcher alle variationer af ordet "udvikl"
udvikl_pattern <- "\\budvikl\\w*\\b"
udvikl_rows <- grep(udvikl_pattern, ordoptælling$word, value = TRUE)

# Sum af ordet "udvikl" variationer
udvikl_count <- sum(ordoptælling$n[ordoptælling$word %in% udvikl_rows])

# Laver ny række med summen
new_row_udvikl <- data.frame(word = "udvikle (alle variationer)", n = udvikl_count)

# Matcher alle variationer af ordet "løse"
løs_pattern <- "\\bløs\\w*\\b"
løs_rows <- grep(løs_pattern, ordoptælling$word, value = TRUE)

# Sum af ordet "løs" variationer
løs_count <- sum(ordoptælling$n[ordoptælling$word %in% løs_rows])

# Laver ny række med summen
new_row_løs <- data.frame(word = "løse (alle variationer)", n = løs_count)

# Matcher alle variationer af ordet "customer"
customer_pattern <- "\\bcustomer\\w*\\b"
customer_rows <- grep(customer_pattern, ordoptælling$word, value = TRUE)

# Sum af ordet "customer" variationer
customer_count <- sum(ordoptælling$n[ordoptælling$word %in% customer_rows])

# Laver ny række med summen
new_row_customer <- data.frame(word = "customer (alle variationer)", n = customer_count)

## Fjerner ordvariationerne fra dataframen ##

#team
team_rep <- grepl(team_pattern, ordoptælling$word)
ordoptælling<-ordoptælling[!team_rep,]

#analyse
analyse_rep <- grepl(analy_pattern, ordoptælling$word)
ordoptælling<-ordoptælling[!analyse_rep,]

#udvikl
udvikl_rep <- grepl(udvikl_pattern, ordoptælling$word)
ordoptælling<-ordoptælling[!udvikl_rep,]

#løs
løs_rep <- grepl(løs_pattern, ordoptælling$word)
ordoptælling<-ordoptælling[!løs_rep,]

#customer 
customer_rep <- grepl(customer_pattern, ordoptælling$word)
ordoptælling<-ordoptælling[!customer_rep,]


## Tilføjer de sammenlagte ord til dataframen ##
ordoptælling<-rbind(ordoptælling, new_row_team)
ordoptælling<-rbind(ordoptælling, new_row_analy)
ordoptælling<-rbind(ordoptælling, new_row_udvikl)
ordoptælling<-rbind(ordoptælling, new_row_løs)
ordoptælling<-rbind(ordoptælling, new_row_customer)

#Henter den ned som csv 
write.csv(ordoptælling, "ordoptælling_final.csv", row.names = FALSE)

#### Wordcloud #### 

library(wordcloud)
set.seed(1234)

wordcloud(words = ordoptælling$word, freq = ordoptælling$n,
          min.freq = 1, max.words=50, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))


#### Plot ####
library(ggplot2)

ggplot(data=ordoptælling, 
       aes(x = word, y = n, group=1))+
  geom_line(color="black")+
  ggtitle("Titel")+
  labs(x="Ord",y="Antal")
