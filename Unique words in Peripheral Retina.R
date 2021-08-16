#-------------------------Unique words in Peripheral Retina
#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)

#------------Word frequencies
PeripheralRetinatext = c("Pigmentary changes", "Dark pigment spots/mottling", "Peripheral pigmentary changes", "Abnormal", "Pigmentary changes", 
               "Dark pigment spots/mottling", "Leopard spots", "Bone spicule-shaped pigment deposits", "Pigmentary changes", 
               "Dark pigment spots/mottling", "Abnormal", "Dark pigment spots/mottling", "Dark pigment spots/mottling", "Pigmentary changes", 
               "Abnormal", "Dark pigment spots/mottling", "Pigmentary changes", "Pigmentary changes", "Normal", "Peripheral pigmentary changes", 
               "RPE hypertrophy", "Normal", "Dark pigment spots/mottling", "Blond periphery", "Pigmentary changes", "Bone spicule-shaped pigment deposits", 
               "Normal", "Dark pigment spots/mottling", "Normal", "Abnormal", "Normal", "No bone spicules", "Flat", "Blond periphery", "Fine pigmentary clumping", 
               "Normal", "Dark pigment spots/mottling", "Normal", "Bone spicule-shaped pigment deposits", "Dark pigment spots/mottling", "Abnormal", 
               "Leopard spots", "Pigmentary changes", "Fine pigmentary clumping", "Dark pigment spots/mottling", "Normal", "Normal", 
               "Dark pigment spots/mottling", "Grey periphery", "Bear-track pigmentation", "Peripheral pigmentary changes", "Abnormal", 
               "Peripheral pigmentary changes", "Dark pigment spots/mottling", "Fine pigmentary clumping", "Abnormal", "Peripheral pigmentary changes", 
               "Dark pigment spots/mottling", "Pigmentary changes", "Fine pigmentary clumping", "Peripheral pigmentary changes", 
               "Dark pigment spots/mottling", "Abnormal", "Bone spicule-shaped pigment deposits", "Blond periphery", "Normal", "Pigmentary changes", 
               "Mild pigment deposition", "Dark pigment spots/mottling", "Pigmentary clumps", "Normal", "Dark pigment spots/mottling", 
               "Fine pigmentary clumping", "Dark pigment spots/mottling", "Pigmentary changes", "Blond periphery", "Normal" , "Normal", 
               "Dark pigment spots/mottling", "Normal", "Dark pigment spots/mottling", "Bone spicule-shaped pigment deposits", 
               "Retinal periphery epithelium changes", "Pigmentary changes", "Normal", "Dark pigment spots/mottling", "Dark pigment spots/mottling", 
               "Normal", "Dark pigment spots/mottling", "Blond periphery", "Dark pigment spots/mottling", "Pigmentary changes", "Normal", 
               "Pigmentary changes", "Dark pigment spots/mottling", "Bone spicule-shaped pigment deposits", "Leopard spots", 
               "Pigmentary changes", "Dark pigment spots/mottling", "Bone spicule-shaped pigment deposits", "Normal", "Dark pigment spots/mottling", 
               "Normal", "Fine pigmentary clumping", "Bone spicule-shaped pigment deposits", "Pigmentary changes", "Pale", "Dark pigment spots/mottling", 
               "Pigmentary changes", "Normal", "Normal", "Fine pigmentary clumping", "Normal", "Pigmentary changes", "Pigmentary changes", "Pigmentary changes", 
               "Bone spicule-shaped pigment deposits", "Attached 360 degrees", "Flat", "Dark pigment spots/mottling", "Dark pigment spots/mottling", "Normal", 
               "Normal", "Normal", "Normal", "Normal", "Normal", "Bone spicule-shaped pigment deposits", "Normal", "Normal", "Blond periphery", "Pigmentary changes", 
               "Pigmentary changes")

PeripheralRetinaTextframe=as.data.frame(PeripheralRetinatext)

#Tokenize
PeripheralRetinaTexttoken <- tibble(PeripheralRetinaTextframe)

PeripheralRetinaTextCount <- count(PeripheralRetinaTexttoken, PeripheralRetinatext)

wordcloud(words = PeripheralRetinaTextCount$PeripheralRetinatext, freq = PeripheralRetinaTextCount$n, min.freq = 1)

library("writexl")
write_xlsx(PeripheralRetinaTextCount,"/Users/maryagopi/Desktop/PeripheralRetinaTextCount.xlsx")

#------------------
library(ggplot2)
PeripheralRetinaplot <- ggplot(PeripheralRetinaTextCount, aes(x=PeripheralRetinatext, y=n)) + 
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.3, color="black", size=3)+
  theme_minimal()+ 
  xlab("PeripheralRetina") + 
  ylab("Frequency")+
  ggtitle("Frequency of PeripheralRetina (Unique ID)")+
  coord_flip()

PeripheralRetinaplot