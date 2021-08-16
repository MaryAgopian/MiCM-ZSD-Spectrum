#-------------------------Unique words in Macula
#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)

#------------Word frequencies
Maculatext = c("Abnormal", "Normal", "Abnormal", "Macular dystrophy", "Maculopathy", "Mottling of macula", "Macular pigmentary changes", "Abnormal", "Normal", "Abnormal", "Macular atrophy", 
                  "Abnormal", "Cystoid macular edema", "Maculopathy", "Macular Edema", "Macular scarring", "Macular thinning", "Abnormal", "Cystoid macular edema", "Macular Edema", 
                  "Dry macula", "Maculopathy", "Normal", "Abnormal", "Macular pigmentary changes", "Macular Edema", "Normal", "Cystoid macular edema", "Even pigment", "Normal", 
                  "Macular pigmentary changes", "Abnormal", "Macular pigmentary changes", "Indistinct fovea", "Normal", "Abnormal", "Normal", "Abnormal", "Blunted fovea", "Macular hyperpigmentation", 
                  "Flat", "Normal", "Abnormal", "Normal color", "Darkening of fovea", "Normal", "Macular hyperpigmentation", "Mottling of macula", "Maculopathy", "Macular atrophy", 
                  "Macular pigmentary changes", "No pigmentary changes", "Flat", "No hemorrhages", "Normal", "Normal", "Macular pigmentary changes", "Maculopathy", "Abnormal", "Normal", 
                  "Macular hyperpigmentation", "Macular pigmentary changes", "Abnormal", "Abnormal", "Macular atrophy", "Macular pigmentary changes", "Abnormal", "Mottling of macula", 
                  "Normal", "Normal", "Macular pigmentary changes", "Normal", "Mottling of macula", "Macular degeneration", "Normal", "Traction syndrome", "Flat", "Dry macula", "Normal", 
                  "Macular pigmentary changes", "Macular pigmentary changes", "Maculopathy", "Cystoid macular edema", "Normal", "Decreased macular reflex", "Macular pigmentary changes", 
                  "Normal", "Normal", "Mottling of macula", "Normal", "Normal", "Normal", "Macular pigmentary changes", "Macular Edema", "Normal", "Macular pigmentary changes", "Normal", "Normal", 
                  "Normal", "Mottling of macula", "Normal", "Maculopathy", "Macular pigmentary changes", "Normal", "Normal")

MaculaTextframe=as.data.frame(Maculatext)

#Tokenize
MaculaTexttoken <- tibble(MaculaTextframe)

MaculaTextCount <- count(MaculaTexttoken, Maculatext)

wordcloud(words = MaculaTextCount$Maculatext, freq = MaculaTextCount$n, min.freq = 1)

library("writexl")
write_xlsx(MaculaTextCount,"/Users/maryagopi/Desktop/MaculaTextCount.xlsx")

#------------------
library(ggplot2)
Maculaplot <- ggplot(MaculaTextCount, aes(x=Maculatext, y=n)) + 
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.3, color="black", size=3)+
  theme_minimal()+ 
  xlab("Macula") + 
  ylab("Frequency")+
  ggtitle("Frequency of Macula (Unique ID)")+
  coord_flip()

Maculaplot