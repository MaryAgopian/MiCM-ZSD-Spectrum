#-------------------------Unique words in ERGCom
#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)

#------------ERGCom Word frequencies
ERGComtext = c("Flat", "Not detectable", "Diminished", "Not detectable", "Not detectable", "Not detectable", "Diminished", "Not detectable", "Flat", 
               "Diminished", "Not detectable", "Diminished", "Not Detectable", "Not detectable", "Diminished", "Diminished", "Diminished", 
               "Diminished", "Diminished", "Not detectable", "Not detectable", "Diminished", "Diminished", "Normal", "Not detectable", "Not detectable")

ERGComTextframe=as.data.frame(ERGComtext)

#Tokenize
ERGComTexttoken <- tibble(ERGComTextframe)

ERGComTextCount <- count(ERGComTexttoken, ERGComtext)

wordcloud(words = ERGComTextCount$ERGComtext, freq = ERGComTextCount$n, min.freq = 1)

library("writexl")
write_xlsx(ERGComTextCount,"/Users/maryagopi/Desktop/ERGComTextCount.xlsx")

#------------------
library(ggplot2)
ERGComplot <- ggplot(ERGComTextCount, aes(x=ERGComtext, y=n)) + 
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.3, color="black", size=3)+
  theme_minimal()+ 
  xlab("ERG Comments Diagnosis") + 
  ylab("Frequency")+
  ggtitle("Frequency of ERG Comments Diagnosis (Unique ID)")+
  coord_flip()

ERGComplot