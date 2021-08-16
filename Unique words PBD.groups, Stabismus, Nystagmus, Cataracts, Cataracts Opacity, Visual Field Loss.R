#Opening OnlyCharacter UniqueID data on R
onlycharoptho <- read.csv("Mary_Jun26_Optho_Only Characters.csv")

#Double checking everything is character (except for ID column = integer)
str(onlycharoptho)

#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)
#---------------------------------------------------------------------------------------
##PBD Diagnosis Word Cloud

##isolate out one column at a time for word cloud
PBDGroup <- onlycharoptho$PBD.group
PBDGroupframe=as.data.frame(PBDGroup)
PBDGroup

#Tokenize
PBDtoken <- tibble(PBDGroupframe)

PBDCount <- count(PBDtoken, PBDGroup)
PBDCount

wordcloud(words = PBDCount$PBDGroup, freq = PBDCount$n, min.freq = 1)

library("writexl")
write_xlsx(PBDCount,"/Users/maryagopi/Desktop/Excel parts/June 27/PBDCount.xlsx")

#---------------------------------------------------------------------------------------
##Strabismus  Word Cloud

##isolate out one column at a time for word cloud
StrabismusGroup <- onlycharoptho$Strabismus
StrabismusGroupframe=as.data.frame(StrabismusGroup)
StrabismusGroup

#Tokenize
StrabismusToken <- tibble(StrabismusGroupframe)

StrabismusCount <- count(StrabismusToken, StrabismusGroup)
StrabismusCount

wordcloud(words = StrabismusCount$StrabismusGroup, freq = StrabismusCount$n, min.freq = 1)

library("writexl")
write_xlsx(StrabismusCount,"/Users/maryagopi/Desktop/Excel parts/June 27/StrabismusCount.xlsx")

#---------------------------------------------------------------------------------------
##Nystagmus  Word Cloud

##isolate out one column at a time for word cloud
NystagmusGroup <- onlycharoptho$Nystagmus
NystagmusGroupframe=as.data.frame(NystagmusGroup)

#Tokenize
NystagmusToken <- tibble(NystagmusGroupframe)

NystagmusCount <- count(NystagmusToken, NystagmusGroup)
NystagmusCount

wordcloud(words = NystagmusCount$NystagmusGroup, freq = NystagmusCount$n, min.freq = 1)

library("writexl")
write_xlsx(NystagmusCount,"/Users/maryagopi/Desktop/Excel parts/June 27/NystagmusCount.xlsx")
#---------------------------------------------------------------------------------------
##Cataracts  Word Cloud

##isolate out one column at a time for word cloud
CataractsGroup <- onlycharoptho$Cataracts_type
CataractsGroupframe=as.data.frame(CataractsGroup)
CataractsGroup

#Tokenize
CataractsToken <- tibble(CataractsGroupframe)

CataractsCount <- count(CataractsToken, CataractsGroup)
CataractsCount

wordcloud(words = CataractsCount$CataractsGroup, freq = CataractsCount$n, min.freq = 1)

library("writexl")
write_xlsx(CataractsCount,"/Users/maryagopi/Desktop/Excel parts/June 27/CataractsCount.xlsx")
#---------------------------------------------------------------------------------------
##Cataracts Opacity  Word Cloud

##isolate out one column at a time for word cloud
CataractsOpaGroup <- onlycharoptho$Cataracts_type_of_opacity
CataractsOpaGroupframe=as.data.frame(CataractsOpaGroup)

#Tokenize
CataractsOpaToken <- tibble(CataractsOpaGroupframe)

CataractsOpaCount <- count(CataractsOpaToken, CataractsOpaGroup)
CataractsOpaCount

wordcloud(words = CataractsOpaCount$CataractsOpaGroup, freq = CataractsOpaCount$n, min.freq = 1)

library("writexl")
write_xlsx(CataractsOpaCount,"/Users/maryagopi/Desktop/Excel parts/June 27/CataractsOpaCount.xlsx")
#---------------------------------------------------------------------------------------
##Type of VF loss Opacity  Word Cloud

##isolate out one column at a time for word cloud
VFLossGroup <- onlycharoptho$Type.of.VF.loss
VFLossGroupframe=as.data.frame(VFLossGroup)

#Tokenize
VFLossToken <- tibble(VFLossGroupframe)

VFLossCount <- count(VFLossToken, VFLossGroup)
VFLossCount

wordcloud(words = VFLossCount$VFLossGroup, freq = VFLossCount$n, min.freq = 1)

library("writexl")
write_xlsx(VFLossCount,"/Users/maryagopi/Desktop/Excel parts/June 27/VFLossCount.xlsx")



