#-------------------------Unique words in Diagnosis

#Opening OnlyCharacter Unique Diagnosis words on R
# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
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
diagnosistext = c("Esotropia", "Optic nerve atrophy", "Retinitis pigmentosa", "Bilateral hyperopia", "Retinal dystrophy", "Amblyopia", "Strabismus", "Optic atrophy", "Esotropia", "Optic nerve atrophy", "Retinitis pigmentosa", "Strabismus", 
         "Bilateral hyperopia", "Maculopathy", "Retinopathy", "Strabismus", "Nystagmus", "Astigmatism", "Nyctalopia", "Nystagmus", "Retinitis pigmentosa", "Retinopathy", "Nystagmus", "Retinal dystrophy", "Bilateral hyperopia", 
         "Astigmatism", "Retinopathy", "Amblyopia", "Esotropia", "Maculopathy", "Retinitis pigmentosa", "Retinopathy", "Nystagmus", "Bilateral hyperopia", "Astigmatism", "Retinal degeneration", "Cystoid Macular Edema", "Nyctalopia", 
         "Nystagmus", "Retinal degeneration", "Bilateral hyperopia", "Astigmatism", "Retinopathy", "Esotropia", "Retinal detachment", "Strabismus", "Esotropia", "Retinopathy", "Bilateral hyperopia", "Cataract", "Maculopathy", 
         "Retinal degeneration", "Retinitis pigmentosa", "Nystagmus", "Amblyopia", "Optic nerve atrophy", "Retinal dystrophy", "Retinitis pigmentosa", "Bilateral hyperopia", "Astigmatism", "Blindness", "Retinal degeneration", 
         "Esotropia", "Nystagmus", "Nystagmus", "Strabismus", "Conjunctivitis", "Nystagmus", "Nystagmus", "Bilateral hyperopia", "Esotropia", "Cortical visual impairement", "Nystagmus", "Retinitis pigmentosa", "Retinopathy", "Nystagmus", 
         "Optic nerve atrophy", "Retinal dystrophy", "Bilateral hyperopia", "Astigmatism", "Nyctalopia", "Retinal degeneration", "Retinitis pigmentosa", "Retinopathy", "Nystagmus", "Optic nerve atrophy", "Bilateral hyperopia", 
         "Astigmatism", "Amblyopia", "Esotropia", "Retinal dystrophy", "Bilateral hyperopia", "Strabismus", "Astigmatism", "Esotropia", "Retinopathy", "Nystagmus", "Blindness", "Nyctalopia", "Retinopathy", "Cataract", "Maculopathy", 
         "Optic atrophy", "Retinitis pigmentosa", "Retinopathy", "Bilateral hyperopia", "Optic nerve atrophy", "Nyctalopia", "Glaucoma", "Esotropia", "Nystagmus", "Retinal dystrophy", "Bilateral hyperopia", "Nystagmus", "Nystagmus", 
         "Maculopathy", "Nystagmus", "Retinal dystrophy", "Bilateral hyperopia", "Central scotoma", "Amblyopia", "Strabismus", "Retinitis pigmentosa", "Esotropia", "Retinopathy", "Retinal degeneration", "Astigmatism", "Esotropia", 
         "Strabismus", "Bilateral hyperopia", "Nyctalopia", "Esotropia", "Bilateral hyperopia", "Blindness", "Nyctalopia", "Nystagmus", "Retinal dystrophy", "Retinal degeneration", "Retinitis pigmentosa", "Strabismus", "Esotropia", 
         "Nystagmus", "Retinal dystrophy", "Amblyopia", "Bilateral hyperopia", "Astigmatism", "Esotropia", "Bilateral hyperopia", "Astigmatism", "Retinitis pigmentosa", "Astigmatism", "Nyctalopia", "Nystagmus", "Retinopathy", 
         "Blindness", "Optic atrophy", "Strabismus", "Blindness", "Retinitis pigmentosa", "Retinopathy", "Strabismus", "Glaucoma", "Optic atrophy", "Esotropia", "Nystagmus", "Retinal degeneration", "Nystagmus", "Nystagmus", 
         "Retinitis pigmentosa", "Blindness", "Strabismus", "Cortical visual impairement", "Nystagmus", "Nystagmus", "Retinal dystrophy", "Blindness", "Retinopathy", "Esotropia", "Retinopathy", "Nystagmus", "Retinitis pigmentosa", 
         "Retinal dystrophy", "Nystagmus", "Retinal degeneration", "Bilateral hyperopia", "Nyctalopia", "Amblyopia", "Esotropia", "Strabismus", "Cataract", "Blindness", "Nystagmus", "Retinal dystrophy", "Cataract", "Nystagmus", 
         "Esotropia", "Strabismus", "Bilateral hyperopia", "Nystagmus", "Cortical visual impairement", "Nystagmus", "Retinal degeneration", "Esotropia", "Nystagmus", "Strabismus", "Maculopathy", "Esotropia", "Strabismus", "Amblyopia", 
         "Nystagmus", "Bilateral hyperopia", "Retinal dystrophy", "Optic nerve atrophy", "Nystagmus", "Strabismus", "Maculopathy", "Nystagmus", "Retinal dystrophy", "Esotropia", "Cataract", "Conjunctivitis", "Retinitis pigmentosa", 
         "Optic nerve atrophy", "Bilateral hyperopia", "Blindness", "Retinitis pigmentosa", "Retinitis pigmentosa", "Blindness", "Nystagmus", "Retinopathy", "Bilateral hyperopia", "Optic atrophy", "Retinitis pigmentosa", 
         "Retinal degeneration", "Retinal dystrophy", "Retinitis pigmentosa", "Retinal degeneration", "Retinopathy", "Esotropia", "Bilateral hyperopia", "Strabismus", "Strabismus", "Bilateral hyperopia", "Astigmatism", 
         "Optic atrophy",  "Retinopathy", "Nystagmus", "Cataract", "Glaucoma", "Optic nerve atrophy", "Blindness", "Retinal dystrophy", "Bilateral hyperopia", "Astigmatism", "Retinopathy", "Retinitis pigmentosa", "Bilateral hyperopia", 
         "Strabismus", "Astigmatism", "Nystagmus", "Bilateral hyperopia", "Amblyopia", "Retinal degeneration", "Maculopathy", "Nystagmus", "Astigmatism", "Retinal dystrophy", "Retinal dystrophy", "Cystoid Macular Edema", 
         "Bilateral hyperopia", "Astigmatism", "Retinal dystrophy", "Other (please specify)", "Retinitis pigmentosa", "Retinitis pigmentosa", "Strabismus", "Bilateral hyperopia", "Retinal dystrophy", "Bilateral hyperopia", 
         "Astigmatism", "Nystagmus", "Nyctalopia", "Retinitis pigmentosa", "Retinitis pigmentosa", "Amblyopia", "Esotropia", "Nystagmus", "Bilateral hyperopia", "Blindness", "Nystagmus", "Retinopathy", "Bilateral hyperopia", "Nystagmus", 
         "Bilateral hyperopia", "Blindness", "Amblyopia", "Retinopathy", "Bilateral hyperopia", "Cortical visual impairement", "Nystagmus", "Exotropia", "Retinal dystrophy", "Blindness")

DiagnosisTextframe=as.data.frame(diagnosistext)

#Tokenize
DiagnosisTexttoken <- tibble(DiagnosisTextframe)

DiagnosisTextCount <- count(DiagnosisTexttoken, diagnosistext)

wordcloud(words = DiagnosisTextCount$diagnosistext, freq = DiagnosisTextCount$n, min.freq = 1)

library("writexl")
write_xlsx(DiagnosisTextCount,"/Users/maryagopi/Desktop/DiagnosisTextCount.xlsx")

#------------------
library(ggplot2)
Diagnosisplot <- ggplot(DiagnosisTextCount, aes(x=diagnosistext, y=n)) + 
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.3, color="black", size=3)+
  theme_minimal()+ 
  xlab("Diagnosis") + 
  ylab("Frequency")+
  ggtitle("Frequency of Diagnosis (Unique ID)")+
  coord_flip()

Diagnosisplot
