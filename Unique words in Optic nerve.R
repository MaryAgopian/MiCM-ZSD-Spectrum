#-------------------------Unique words in Optic nerve
#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)

#------------Optic nerve Word frequencies
OpticNervetext = c("Optic nerve atrophy"," Optic nerve atrophy", "Small optic nerve", "Optic nerve atrophy", "Optic nerve pallor", "Normal", "Normal", "Abnormal", "Sharp", "Optic nerve drusen", 
                   "Burried drusen", "Normal", "Small optic nerve", "Abnormal", "Optic nerve atrophy", "Small optic nerve", "Optic nerve pallor", "Grayish", "Decreased pigmentation", 
                   "Abnormal", "Normal", "Normal", "Waxy pallor", "Optic nerve pallor", "Abnormal", "Small optic nerve", "Optic nerve atrophy", "Normal", "Optic nerve pallor", "Abnormal", 
                   "Optic nerve atrophy", "Optic nerve hypoplasia", "Normal", "Abnormal", "Optic nerve hypoplasia", "Optic nerve atrophy", "Decreased pigmentation", "Optic nerve atrophy", 
                   "Optic nerve pallor", "Flat", "Sharp", "Normal", "Optic nerve drusen", "Pale", "Waxy pallor", "Pale", "Optic nerve atrophy", "Small optic nerve", "Abnormal", "Normal", "Normal", "Normal", 
                   "Normal", "Abnormal", "Normal", "Pink", "Small optic nerve", "Optic nerve atrophy", "Optic nerve pallor", "Optic nerve atrophy", "Normal", "Small optic nerve", "Optic nerve pallor", 
                   "Optic nerve atrophy", "Normal", "Waxy pallor of optic nerve", "Normal", "Optic nerve pallor", "Optic nerve pallor", "Normal", "Optic nerve atrophy", "Normal", "Optic nerve atrophy", 
                   "Optic nerve pallor", "Normal", "Small optic nerve", "Flat", "Optic nerve drusen", "Sharp", "Small optic nerve", "Optic nerve atrophy", "Normal", "Optic nerve pallor", "Normal", 
                   "Optic nerve atrophy", "Normal", "Sharp", "Optic nerve atrophy", "Normal")

OpticNerveTextframe=as.data.frame(OpticNervetext)

#Tokenize
OpticNerveTexttoken <- tibble(OpticNerveTextframe)

OpticNerveTextCount <- count(OpticNerveTexttoken, OpticNervetext)

wordcloud(words = OpticNerveTextCount$OpticNervetext, freq = OpticNerveTextCount$n, min.freq = 1)

library("writexl")
write_xlsx(OpticNerveTextCount,"/Users/maryagopi/Desktop/OpticNerveTextCount.xlsx")

#------------------
library(ggplot2)
OpticNerveplot <- ggplot(OpticNerveTextCount, aes(x=OpticNervetext, y=n)) + 
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.3, color="black", size=3)+
  theme_minimal()+ 
  xlab("Optic Nerve") + 
  ylab("Frequency")+
  ggtitle("Frequency of OpticNerve (Unique ID)")+
  coord_flip()

OpticNerveplot