#-------------------------Unique words in Assist
#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)

#------------Treatment Word frequencies
Assisttext = c("Use of a white cane", "Use of a white cane", "OTT light", "Use of a white cane", "Braille reading", "Telescope", "Use of a white cane", 
               "Large print reading", "Desktop magnifier", "Portable electonic magnifier", "Illuminated dome", "Telescope", "Hand-held telescope", 
               "Handheld magnifier", "Desktop magnifier", "Desktop magnifier", "Handheld magnifier", "Desktop magnifier", "CCTV", 
               "Bifocal mounted microscope", "Handheld magnifier", "Portable electonic magnifier", "Braille reading", "Large print reading", 
               "Use of a white cane")

AssistTextframe=as.data.frame(Assisttext)

#Tokenize
AssistTexttoken <- tibble(AssistTextframe)

AssistTextCount <- count(AssistTexttoken, Assisttext)

wordcloud(words = AssistTextCount$Assisttext, freq = AssistTextCount$n, min.freq = 1)

library("writexl")
write_xlsx(AssistTextCount,"/Users/maryagopi/Desktop/AssistTextCount.xlsx")

#------------------
library(ggplot2)
Assistplot <- ggplot(AssistTextCount, aes(x=Assisttext, y=n)) + 
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.3, color="black", size=3)+
  theme_minimal()+ 
  xlab("Assistive Devices") + 
  ylab("Frequency")+
  ggtitle("Frequency of Assistive Activites (Unique ID)")+
  coord_flip()

Assistplot