#-------------------------Unique words in Optic Disc
#Opening OnlyCharacter Unique Optic Disc words on R
# Load
#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)


#------------Word frequencies
opticdisctext = c("Small optic discs", "No cupping", "Optic disc pallor", "Abnormal", "Sharp", "Gray", "Atrophic", "Optic disc pallor", "Optic disc pallor", "No disc/indistinguishable discs", "Abnormal", "Drusen", 
                   "Normal", "Normal", "Optic disc pallor", "Optic disc discoloration", "Abnormal", "Pigments on optic disc", "Pale", "Normal", "Pink", "Sharp", "Optic disc pallor", "Normal", "Sharp", "Optic disc swelling", 
                   "Abnormal", "Normal", "Somewhat grayish", "Abnormal", "Optic disc pallor", "Flat grey", "Small optic discs", "Sharp", "Pink", "No cup visible", "Abnormal", "Sharp", "Gray color", "Flat", "Optic disc pallor", 
                   "Optic disc pallor", "Small optic discs", "Normal", "Waxy pallor", "Sharp", "Normal", "Normal", "Normal", "Optic disc pallor", "Abnormal", "Optic disc pallor", "Waxy pallor", "Pigments on optic disc", 
                   "Small optic discs", "No disc/indistinguishable discs", "Optic disc pallor", "Abnormal", "Normal", "Gray", "Normal", "Normal", "Optic disc pallor", "Small optic discs", "Not sharp", "Small optic discs", 
                   "Sharp", "Optic disc pallor", "Pink", "Abnormal", "Gray color", "Small optic discs", "Normal", "Small optic discs", "Optic disc pallor", "Abnormal", "No discernable cup", "Optic disc pallor", "Normal", 
                   "Optic disc swelling", "Normal", "Sharp", "Optic disc pallor", "Normal", "Optic disc swelling", "Optic disc pallor", "Normal", "Sharp", "Not sharp", "Abnormal", "No cup visible", "Waxy pallor", "Optic disc pallor", 
                   "Optic disc pallor", "Normal", "Optic disc discoloration", "Optic disc pallor", "Small optic discs", "Optic disc pallor", "Normal", "Normal", "Normal", "Normal", "Gray color", "Optic disc drusen", 
                   "Optic disc pallor", "Optic disc pallor", "Gliosis", "Tilted", "Optic disc pallor", "Normal", "Small optic discs", "Waxy pallor", "Normal", "Sharp", "Normal", "Normal", "Sharp", "Optic disc discoloration")

opticdiscTextframe=as.data.frame(opticdisctext)

#Tokenize
opticdiscTexttoken <- tibble(opticdiscTextframe)

opticdiscTextCount <- count(opticdiscTexttoken, opticdisctext)

wordcloud(words = opticdiscTextCount$opticdisctext, freq = opticdiscTextCount$n, min.freq = 1)

library("writexl")
write_xlsx(opticdiscTextCount,"/Users/maryagopi/Desktop/opticdiscTextCount.xlsx")

#------------------
library(ggplot2)
opticdiscplot <- ggplot(opticdiscTextCount, aes(x=opticdisctext, y=n)) + 
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.3, color="black", size=3)+
  theme_minimal()+ 
  xlab("Optic Disc") + 
  ylab("Frequency")+
  ggtitle("Frequency of Optic Disc (Unique ID)")+
  coord_flip()

opticdiscplot