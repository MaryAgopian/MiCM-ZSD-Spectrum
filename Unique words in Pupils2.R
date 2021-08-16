#-------------------------Unique words in Pupils2
#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)

#------------Pupils2 Word frequencies
Pupils2text = c("Reactive", "Non-reactive", "Reactive", "Reactive", "Reactive", "Reactive", "Reactive", "Reactive", "Reactive", "Poorly Reactive", "Non-Reactive", 
                "Reactive", "Reactive", "Reactive", "Reactive", "Reactive", "Reactive", "Reactive", "Sluggish reaction", "Poorly reactive", "Reactive", 
                "Sluggish reaction", "Reactive", "Reactive", "Sluggish reaction", "Reactive", "Reactive", "Reactive", "Reactive", "Reactive", "Reactive", 
                "Reactive", "Reactive", "Reactive", "Reactive", "Non-reactive", "Sluggish reaction", "Reactive", "Reactive", "Poorly reactive", "Reactive", 
                "Reactive", "Poorly reactive", "Non-reactive", "Reactive", "Reactive", "Reactive", "Non-reactive", "Reactive", "Poorly reactive", "Reactive", 
                "Non-reactive", "Reactive", "Reactive", "Reactive", "Reactive", "Poorly reactive", "Reactive", "Reactive", "Poorly reactive", "Sluggish reaction", "Reactive", "Reactive", 
                "Non-Reactive", "Reactive", "Reactive", "Sluggish reaction", "Poorly reactive", "Reactive", "Reactive", "Reactive", "Sluggish reaction", "Reactive", 
                "Reactive", "Reactive", "Reactive", "Reactive", "Reactive", "Sluggish reaction", "Reactive", "Reactive", "Reactive", "Sluggish reaction", "Reactive")

Pupils2Textframe=as.data.frame(Pupils2text)

#Tokenize
Pupils2Texttoken <- tibble(Pupils2Textframe)

Pupils2TextCount <- count(Pupils2Texttoken, Pupils2text)

wordcloud(words = Pupils2TextCount$Pupils2text, freq = Pupils2TextCount$n, min.freq = 1)

library("writexl")
write_xlsx(Pupils2TextCount,"/Users/maryagopi/Desktop/Pupils2TextCount.xlsx")

#------------------
library(ggplot2)
Pupils2plot <- ggplot(Pupils2TextCount, aes(x=Pupils2text, y=n)) + 
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.3, color="black", size=3)+
  theme_minimal()+ 
  xlab("Pupils2 Diagnosis") + 
  ylab("Frequency")+
  ggtitle("Frequency of Pupils2 Diagnosis (Unique ID)")+
  coord_flip()

Pupils2plot