#Opening Ophthalmology data on R
optho <- read.csv("June19_optho.csv")

#Summary of columns in database
head(optho)

#Checking if the number of columns in the count data and the rows match up.
nrow(optho)
## Should be 882

#Double checking everything is in order
length(optho$ID)
c
length(optho$Ophthalmo_AgeAtExam)
## Should also be 882, no mismatch

length(unique(optho$Ophthalmo_AgeAtExam))
#Should be 40, there are 40 unique ages

#Downloading stargazer package for tables
install.packages("stargazer")
library(stargazer)



#Observation = what are the unique PBD groups in the PBD column
sort(unique(optho$PBD.group, decreasing=TRUE))
##[1] "Other PEX - Intermediate and mild"
##[2] "Other PEX - Severe"               
##[3] "PEX1 G843D / Null"                
##[4] "PEX1 G843D Homozygote"            
##[5] "PEX1 severe"      

#Observation = what are the unique ages in the age column
sort(unique(optho$Ophthalmo_AgeAtExam, decreasing=FALSE))
## ages: 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 31 32 33 34 35 38 39 40 41 43

#Observation = unique pairings of diagnosis (not diagnosis per term)
sort(unique(optho$Diagnosis, decreasing=TRUE))
