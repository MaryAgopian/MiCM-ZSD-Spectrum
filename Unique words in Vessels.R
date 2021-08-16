#-------------------------Unique words in Vessels
#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)

#------------Vessels Word frequencies
Vesselstext = c("Normal", "Thinned vessels", "Attenuated vessels", "Abnormal", "Attenuated vessels", "Attenuated vessels", "Thinned vessels", "Abnormal", 
                "Normal", "Attenuated vessels", "Normal", "Normal", "Normal", "Non-tortuous vessels", "Abnormal", "Normal", "Attenuated vessels", "Normal", 
                "Attenuated vessels", "Normal", "Normal", "Attenuated vessels", "Attenuated vessels", "Thinned vessels", "Normal", "Normal", "Attenuated vessels", 
                "Normal", "Thinned vessels", "Abnormal", "Attenuated vessels", "Abnormal", "Abnormal", "Thinned vessels", "Normal", "Normal", "Normal", 
                "Attenuated vessels", "Normal", "Thinned vessels", "Attenuated vessels", "Attenuated vessels", "Attenuated vessels", "Normal", "Normal", 
                "Normal", "Normal", "Attenuated vessels", "Normal", "Abnormally branching", "Attenuated vessels", "Attenuated vessels", "Attenuated vessels", 
                "Attenuated vessels", "Thinned vessels", "Attenuated vessels", "Normal", "Normal", "Normal", "Normal", "Attenuated vessels", "Normal", 
                "Attenuated vessels", "Attenuated vessels", "Normal", "Normal", "Normal", "Normal", "Normal", "Attenuated vessels", "Normal", "Normal", 
                "Normal", "Normal")

VesselsTextframe=as.data.frame(Vesselstext)

#Tokenize
VesselsTexttoken <- tibble(VesselsTextframe)

VesselsTextCount <- count(VesselsTexttoken, Vesselstext)

wordcloud(words = VesselsTextCount$Vesselstext, freq = VesselsTextCount$n, min.freq = 1)

library("writexl")
write_xlsx(VesselsTextCount,"/Users/maryagopi/Desktop/VesselsTextCount.xlsx")

#------------------
library(ggplot2)
Vesselsplot <- ggplot(VesselsTextCount, aes(x=Vesselstext, y=n)) + 
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.3, color="black", size=3)+
  theme_minimal()+ 
  xlab("Vessel Diagnosis") + 
  ylab("Frequency")+
  ggtitle("Frequency of Vessel Diagnosis (Unique ID)")+
  coord_flip()

Vesselsplot