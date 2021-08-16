install.packages("eye")
library("eye")

#Opening VA.Numeric data on R
VA.Numeric <- read.csv("Mary_June26_Optho_VA.csv")

#Right Eye--------------------------------------------------------
Right.eye <- VA.Numeric$VA_snellen_R

Right.eye.LogMar <- to_logmar(Right.eye)
Right.eye.LogMarframe=as.data.frame(Right.eye.LogMar)

library("writexl")
write_xlsx(Right.eye.LogMarframe,"/Users/maryagopi/Desktop/Right.eye.LogMarframe.xlsx")

#Left Eye--------------------------------------------------------
Left.eye <- VA.Numeric$VA_snellen_L

Left.eye.LogMar <- to_logmar(Left.eye)
Left.eye.LogMarframe=as.data.frame(Left.eye.LogMar)

library("writexl")
write_xlsx(Left.eye.LogMarframe,"/Users/maryagopi/Desktop/Left.eye.LogMarframe.xlsx")

#Both Eyes--------------------------------------------------------
Both.eye <- VA.Numeric$VA_snellen_Bi

Both.eye.LogMar <- to_logmar(Both.eye)
Both.eye.LogMarframe=as.data.frame(Both.eye.LogMar)

library("writexl")
write_xlsx(Both.eye.LogMarframe,"/Users/maryagopi/Desktop/Both.eye.LogMarframe.xlsx")

#############################################Finding Frequencies and averages
#Opening LogMar.VA.Numeric data on R
Log.Mar.VA.Numeric <- read.csv("LogMary_Mary_June26_Optho_VA._NumOnly.csv")

#Right eye------------------------------------------------------------------- 
library("plyr")
Frequency.Right.eye <- count(Log.Mar.VA.Numeric, 'VA_snellen_R')

library("writexl")
write_xlsx(Frequency.Right.eye,"/Users/maryagopi/Desktop/Frequency.Right.eye.xlsx")


#BAR PLOT
library(ggplot2)
# Basic barplot
p<-ggplot(data=Frequency.Right.eye, aes(x=VA_snellen_R, y=freq)) +
  geom_bar(stat="identity", fill="green")+
  geom_text(aes(label=freq), vjust=-0, size=2.5)+
  theme_minimal()
p

# Horizontal bar plot
p + coord_flip()

#Left eye------------------------------------------------------------------- 
library("plyr")
Frequency.Left.eye <- count(Log.Mar.VA.Numeric, 'VA_snellen_L')

library("writexl")
write_xlsx(Frequency.Left.eye,"/Users/maryagopi/Desktop/Frequency.Left.eye.xlsx")


#BAR PLOT
library(ggplot2)
# Basic barplot
p<-ggplot(data=Frequency.Left.eye, aes(x=VA_snellen_L, y=freq)) +
  geom_bar(stat="identity", fill="green")+
  geom_text(aes(label=freq), vjust=-0, size=2.5)+
  theme_minimal()
p

# Horizontal bar plot
p + coord_flip()
#Bilateral eye------------------------------------------------------------------- 
library("plyr")
Frequency.Both.eye <- count(Log.Mar.VA.Numeric, 'VA_snellen_Bi')

library("writexl")
write_xlsx(Frequency.Both.eye,"/Users/maryagopi/Desktop/Frequency.Both.eye.xlsx")


#BAR PLOT
library(ggplot2)
# Basic barplot
p<-ggplot(data=Frequency.Both.eye, aes(x=VA_snellen_Bi, y=freq)) +
  geom_bar(stat="identity", fill="green")+
  geom_text(aes(label=freq), vjust=-0, size=2.5)+
  theme_minimal()
p

# Horizontal bar plot
p + coord_flip()
#---------------------------------------------------------------------------------------
##CC/SC Pie Chart

#Loading packages
library(dplyr) # for data wrangling
library(tidytext) # for NLP
library(stringr) # to deal with strings

library(wordcloud) # to render wordclouds
library(RColorBrewer)

library(knitr) # for tables
library(tidyr)


##isolate out one row at a time for word cloud
SCCC <- Log.Mar.VA.Numeric$SC.CC.
SCCCframe=as.data.frame(SCCC)

#Tokenize
SCCCToken <- tibble(SCCCframe)
SCCCCount <- count(SCCCToken, SCCC)


library("writexl")
write_xlsx(SCCCCount,"/Users/maryagopi/Desktop/Frequency.SCCC.xlsx")




