#-------------------------Unique words in Expr
#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)

#------------Expr Word frequencies
Exprtext = c("Flat", "Not detectable", "Diminished", "Not detectable", "Not detectable", "Not detectable", "Diminished", "Not detectable", "Flat", 
             "Diminished", "Not detectable", "Diminished", "Not Detectable", "Not detectable", "Diminished", "Diminished", "Diminished", "Diminished", 
             "Diminished", "Not detectable", "Not detectable", "Diminished", "Diminished", "Normal", "Not detectable", "Not detectable")

ExprTextframe=as.data.frame(Exprtext)

#Tokenize
ExprTexttoken <- tibble(ExprTextframe)

ExprTextCount <- count(ExprTexttoken, Exprtext)

wordcloud(words = ExprTextCount$Exprtext, freq = ExprTextCount$n, min.freq = 1)

library("writexl")
write_xlsx(ExprTextCount,"/Users/maryagopi/Desktop/ExprTextCount.xlsx")

#------------------
library(ggplot2)
Exprplot <- ggplot(ExprTextCount, aes(x=Exprtext, y=n)) + 
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.3, color="black", size=3)+
  theme_minimal()+ 
  xlab("Expr Diagnosis") + 
  ylab("Frequency")+
  ggtitle("Frequency of Expr Diagnosis (Unique ID)")+
  coord_flip()

Exprplot