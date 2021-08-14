#Opening Ophthalmology data on R
optho <- read.csv("June19_optho.csv")

#Checking size (no relation with stats, just checking to make sure everythng loaded properly)
object.size(optho)
print(object.size (optho), units = "Mb")

#Double checking everything is in order
length(optho$ID)
##results = 882

length(optho$Ophthalmo_AgeAtExam)
##results = 882

length(unique(optho$Ophthalmo_AgeAtExam))
##results = 40

#Seeing summary
summary(optho)

#Classification of data per column
str(optho)

# Load packages:
library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(dplyr)
library(SnowballC)
library(udpipe)

#______________________________________________
#Gives the frequency of ages
#______________________________________________
optho_ageatexam <- optho %>% 
  group_by(Ophthalmo_AgeAtExam) %>%
  summarise(Freq = n())
optho_ageatexam



#______________________________________________
#Gives the frequency of IDs
#______________________________________________
IDFreqs <- optho %>% 
  group_by(ID) %>%
  summarise(Freq = n())
IDFreqs


#______________________________________________
#Gives the frequency of OBD.groups
#______________________________________________
PBDFreqs <- optho %>% 
  group_by(PBD.group) %>%
  summarise(Freq = n())
PBDFreqs
#______________________________________________

##Plotting this distribution
IDFreqsplot <- ggplot(IDFreqs, aes(y=Freq, x=ID)) + 
  geom_bar(stat = "identity", width=1, fill="blue")+
  geom_text(aes(label=(ID)), vjust=-0.3, size=3.5)+
  theme_minimal()+  
  coord_flip()

IDFreqsplot

##Changing Title bars
ID.Freqsplot<- IDFreqsplot + labs(title = "Frequency of IDs in sample", x = "IDs", y = "Frequency")
ID.Freqsplot

#______________________________________________

#Create diagnosis values
opthodiagnosis<-optho$Diagnosis

unique(opthodiagnosis)
#Had to seperate out the following diagnoisis manually:
#Esotropia,Nystagmus, Blindness, Retinitis pigmentosa, Optic nerve atrophy, Bilateral hyperopia, Retinal dystrophy, Optic atrophy, Amblyopia,Strabismus, Astigmatism, Maculopathy, Nyctalopia, Cystoid Macular Edema, Cataract, Conjunctivitis, Cortical visual impairement, Glaucoma, Central scotoma, Other
