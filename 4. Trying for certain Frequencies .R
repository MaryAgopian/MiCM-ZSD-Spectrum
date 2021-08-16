#Plotting low n columns with the IDs to see the frequency of representation (ie: if N is 50, are 40 of those entries from one single ID = non-representative?)

#Opening Ophthalmology data on R
optho <- read.csv("June19_optho.csv")

#---------------------------------------------------------------------------
# Central Foveal Thickness frequency- with the dplyr and tidyr packages; 
library(tidyr)
library(dplyr)

myFreqs <- optho %>% 
  group_by(ID, Central.Foveal.Thickness..um.) %>%
  summarise(Freq = n())
myFreqs
##Lots of NAs

#Exporting this table to excel
library("writexl")
print(myFreqs)
write_xlsx(myFreqs,"/Users/maryagopi/Desktop/Excel parts/June 20/CFT_freq.xlsx")
write_xlsx
##Table was exported
##Observations, very low n, possibly remove this table

#---------------------------------------------------------------------------
# Mean Thickness frequency- with the dplyr and tidyr packages; 
library(tidyr)
library(dplyr)

MTFreqs <- optho %>% 
  group_by(Mean.thickness..um., ID) %>%
  summarise(Freq = n())
MTFreqs

#Exporting this table to excel
library("writexl")
print(MTFreqs)
write_xlsx(MTFreqs,"/Users/maryagopi/Desktop/Excel parts/June 20/MT_freq.xlsx")
write_xlsx
##Table was exported
##Observations, very low n, possibly remove this table


#---------------------------------------------------------------------------
# Refraction_OD_R frequency- with the dplyr and tidyr packages; 
library(tidyr)
library(dplyr)

RODRFreqs <- optho %>% 
  group_by(Refraction_OD_R, ID) %>%
  summarise(Freq = n())
RODRFreqs

#Exporting this table to excel
library("writexl")
print(RODRFreqs)
write_xlsx(RODRFreqs,"/Users/maryagopi/Desktop/Excel parts/June 20/RODR_freq.xlsx")
write_xlsx
##Table was exported

#---------------------------------------------
#TRYING FOR SCATTER PLOT
ScatterRODR <-RODRFreqs[c(1:156),c(1:3)]

library(ggplot2)
# Basic scatter plot
scatterplotRODR <- ggplot(ScatterRODR, aes(x=Refraction_OD_R, y=Freq)) + geom_point()
scatterplotRODR

# Make the scatter plot even more intuitive through adding a heat map aspect
scatterplot.RODR <- ggplot(ScatterRODR, aes(x=Refraction_OD_R, y=Freq))
scatterplot.RODR+geom_bin2d()

