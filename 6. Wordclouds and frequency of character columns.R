#Opening Ophthalmology data on R
optho <- read.csv("June19_optho.csv")

#isolate out useful character columns
Onlycharactercolumns <- optho[c(1:882),c(4,7,17:29,43:46,55,56,57,59:66,79:82)]
str(Onlycharactercolumns)

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

##isolate out one row at a time for word cloud
PBDGroup <- Onlycharactercolumns$PBD.group
PBDGroupframe=as.data.frame(PBDGroup)

#Tokenize
PBDtoken <- tibble(PBDGroupframe)

PBDCount <- count(PBDtoken, PBDGroup)

wordcloud(words = PBDCount$PBDGroup, freq = PBDCount$n, min.freq = 1)

#---------------------------------------------------------------------------------------
##Diagnosis Word Cloud

##isolate out one row at a time for word cloud
DiagnosisGroup <- Onlycharactercolumns$Diagnosis
DiagnosisGroupframe=as.data.frame(DiagnosisGroup)

#Tokenize
DiagnosisToken <- tibble(DiagnosisGroup)
WordDiagnosis <- DiagnosisToken %>%
  unnest_tokens(word, DiagnosisGroup, token = "words")

DiagnosisCount <- count(WordDiagnosis, word)

wordcloud(words = DiagnosisCount$word, freq = DiagnosisCount$n, min.freq = 1)

#---------------------------------------------------------------------------------------
##Macula Word Cloud

##isolate out one row at a time for word cloud
MaculaGroup <- Onlycharactercolumns$Macula
MaculaGroupframe=as.data.frame(MaculaGroup)

#Tokenize
MaculaToken <- tibble(MaculaGroupframe)

MaculaCount <- count(MaculaToken, MaculaGroup)

wordcloud(words = MaculaCount$MaculaGroup, freq = MaculaCount$n, min.freq = 1)

#---------------------------------------------------------------------------------------
##Optic Disc Word Cloud

##isolate out one row at a time for word cloud
OpticGroup <- Onlycharactercolumns$Optic.disc
OpticGroupframe=as.data.frame(OpticGroup)

#Tokenize
OpticToken <- tibble(OpticGroupframe)

OpticCount <- count(OpticToken, OpticGroup)

wordcloud(words = OpticCount$OpticGroup, freq = OpticCount$n, min.freq = 1)

#---------------------------------------------------------------------------------------
##Optic Disc Word Cloud

##isolate out one row at a time for word cloud
OpticNGroup <- Onlycharactercolumns$Optic.nerve
OpticNGroupframe=as.data.frame(OpticNGroup)

#Tokenize
OpticNToken <- tibble(OpticNGroupframe)

OpticNCount <- count(OpticNToken, OpticNGroup)

wordcloud(words = OpticNCount$OpticNGroup, freq = OpticNCount$n, min.freq = 1)
#---------------------------------------------------------------------------------------
##Peripheral Retina Word Cloud

##isolate out one row at a time for word cloud
Peripheral.retinaGroup <- Onlycharactercolumns$Peripheral.retina
Peripheral.retinaGroupframe=as.data.frame(Peripheral.retinaGroup)

#Tokenize
Peripheral.retinaToken <- tibble(Peripheral.retinaGroupframe)

Peripheral.retinaCount <- count(Peripheral.retinaToken, Peripheral.retinaGroup)

wordcloud(words = Peripheral.retinaCount$Peripheral.retinaGroup, freq = Peripheral.retinaCount$n, min.freq = 1)

#---------------------------------------------------------------------------------------
##Vessels Word Cloud

##isolate out one row at a time for word cloud
VesselsGroup <- Onlycharactercolumns$Vessels
VesselsGroupframe=as.data.frame(VesselsGroup)

#Tokenize
VesselsToken <- tibble(VesselsGroupframe)

VesselsCount <- count(VesselsToken, VesselsGroup)

wordcloud(words = VesselsCount$VesselsGroup, freq = VesselsCount$n, min.freq = 1)

#---------------------------------------------------------------------------------------
##Type of VF loss Word Cloud

##isolate out one row at a time for word cloud
Type.of.VF.lossGroup <- Onlycharactercolumns$Type.of.VF.loss
Type.of.VF.lossGroupframe=as.data.frame(Type.of.VF.lossGroup)

#Tokenize
Type.of.VF.lossToken <- tibble(Type.of.VF.lossGroupframe)

Type.of.VF.lossCount <- count(Type.of.VF.lossToken, Type.of.VF.lossGroup)

wordcloud(words = Type.of.VF.lossCount$Type.of.VF.lossGroup, freq = Type.of.VF.lossCount$n, min.freq = 1)

#---------------------------------------------------------------------------------------
##Type of Treatments Word Cloud

##isolate out one row at a time for word cloud
TreatGroup <- Onlycharactercolumns$Treatment.s..for.eye.condition
TreatGroupframe=as.data.frame(TreatGroup)

#Tokenize
TreatGroupToken <- tibble(TreatGroupframe)

TreatGroupCount <- count(TreatGroupToken, TreatGroup)

wordcloud(words = TreatGroupCount$TreatGroup, freq = TreatGroupCount$n, min.freq = 1)

#---------------------------------------------------------------------------------------
##Type of Treatments 2 Word Cloud

##isolate out one row at a time for word cloud
Treat2Group <- Onlycharactercolumns$Treatment2
Treat2Groupframe=as.data.frame(Treat2Group)

#Tokenize
Treat2GroupToken <- tibble(Treat2Groupframe)

Treat2GroupCount <- count(Treat2GroupToken, Treat2Group)

wordcloud(words = Treat2GroupCount$Treat2Group, freq = Treat2GroupCount$n, min.freq = 1)
#---------------------------------------------------------------------------------------
##Type of Visual Assistance Word Cloud

##isolate out one row at a time for word cloud
VAGroup <- Onlycharactercolumns$Visual.assistive.device
VAGroupframe=as.data.frame(VAGroup)

#Tokenize
VAGroupToken <- tibble(VAGroupframe)

VAGroupCount <- count(VAGroupToken, VAGroup)

wordcloud(words = VAGroupCount$VAGroup, freq = VAGroupCount$n, min.freq = 1)





##############Exporting the frequency values for word clouds into excel value

library("writexl")
write_xlsx(MaculaCount,"/Users/maryagopi/Desktop/Excel parts/June 22/MaculaCount.xlsx")

library("writexl")
write_xlsx(PBDCount,"/Users/maryagopi/Desktop/Excel parts/June 22/PBDCount.xlsx")

library("writexl")
write_xlsx(DiagnosisCount,"/Users/maryagopi/Desktop/Excel parts/June 22/DiagnosisCount.xlsx")

library("writexl")
write_xlsx(OpticCount,"/Users/maryagopi/Desktop/Excel parts/June 22/OpticCount.xlsx")

library("writexl")
write_xlsx(OpticNCount,"/Users/maryagopi/Desktop/Excel parts/June 22/OpticNCount.xlsx")

library("writexl")
write_xlsx(Peripheral.retinaCount,"/Users/maryagopi/Desktop/Excel parts/June 22/Peripheral.retinaCount.xlsx")

library("writexl")
write_xlsx(VesselsCount,"/Users/maryagopi/Desktop/Excel parts/June 22/VesselsCount.xlsx")

library("writexl")
write_xlsx(Type.of.VF.lossCount,"/Users/maryagopi/Desktop/Excel parts/June 22/Type.of.VF.lossCount.xlsx")

library("writexl")
write_xlsx(TreatGroupCount,"/Users/maryagopi/Desktop/Excel parts/June 22/TreatGroupCount.xlsx")

library("writexl")
write_xlsx(Treat2GroupCount,"/Users/maryagopi/Desktop/Excel parts/June 22/Treat2GroupCount.xlsx")

library("writexl")
write_xlsx(VAGroupCount,"/Users/maryagopi/Desktop/Excel parts/June 22/VAGroupCount.xlsx")

