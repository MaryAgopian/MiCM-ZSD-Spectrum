#-------------------------Unique words in Treatment
#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)

#------------Treatment Word frequencies
Treatmenttext = c("Eyeglasses", "Patching", "Eyeglasses", "Solar filter for glasses", "Eyeglasses", "Eye surgery", "Eyeglasses", "Eye drops", 
                  "Eyeglasses", "Eye drops", "Eyeglasses", "Eyeglasses", "Large print reading", "CCTV", "Eyeglasses", "Eyeglasses", "Eyeglasses", 
                  "Eyeglasses", "Eyeglasses", "Eyeglasses", "Eyeglasses", "Eyeglasses", "Patching", "Eye drops", "Eyeglasses", "Eyeglasses", "Eye surgery", 
                  "Vitamin A", "Eye drops", "Eyeglasses", "Eyeglasses", "Eyeglasses", "Eyeglasses", "Eye drops", "Eyeglasses", "Eyeglasses", "Eye drops", 
                  "Eyeglasses", "Eyeglasses", "Eyeglasses", "Eyeglasses", "Eyeglasses", "Eyeglasses", "Eyeglasses")

TreatmentTextframe=as.data.frame(Treatmenttext)

#Tokenize
TreatmentTexttoken <- tibble(TreatmentTextframe)

TreatmentTextCount <- count(TreatmentTexttoken, Treatmenttext)

wordcloud(words = TreatmentTextCount$Treatmenttext, freq = TreatmentTextCount$n, min.freq = 1)

library("writexl")
write_xlsx(TreatmentTextCount,"/Users/maryagopi/Desktop/TreatmentTextCount.xlsx")

#------------------
library(ggplot2)
Treatmentplot <- ggplot(TreatmentTextCount, aes(x=Treatmenttext, y=n)) + 
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.3, color="black", size=3)+
  theme_minimal()+ 
  xlab("Treatments") + 
  ylab("Frequency")+
  ggtitle("Frequency of Treatmentx (Unique ID)")+
  coord_flip()

Treatmentplot