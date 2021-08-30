#Open dataset
OO2 <- read.csv("Fixed optho copy 2.csv", header = TRUE)

#Remove ID and PBD group for clustering
OO3 <- OO2[3:12]

#Double check that all binaries were transformed into integers
str(OO3)

#Transform all binary variables into integers
OO3 <- transform(OO3, Nystagmus..TRUE = as.integer(Nystagmus..TRUE))
OO3 <- transform(OO3, Pupils.2..Reactive = as.integer(Pupils.2..Reactive))

#load all packages
library(tidyverse)
library(magrittr)
library(cluster)
library(cluster.datasets)
library(cowplot)
library(NbClust)
library(clValid)
library(ggfortify)
library(clustree)
library(dendextend)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(GGally)
library(ggiraphExtra)
library(knitr)
library(kableExtra)
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(mclust)
library("kohonen")


#gower
gower_dist <- daisy(OO3, metric = "gower")
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)


#Number of clusters: The silhouette figure helps us identify the best option(s)
sil_width <- c(NA)
for(i in 2:16){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

#For silhouette width. Let’s pick k = 2, since it has the highest silhouette width

#Summary of each cluster
k <- 2
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- OO3 %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

#visualization in a lower dimensional space
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity = 1)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

#This is to see which observations went to which cluster (in the same order as the dataset)
tsne_data$cluster
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#Example of my chi-squared and p-value package on only the Cortical.visual.impairement variable 
#p-value for clustered data
library(htestClust)

OO3 <- transform(OO3, Cortical.visual.impairement = as.integer(Cortical.visual.impairement))
levenetestClust(OO3$Cortical.visual.impairement ~ tsne_data$cluster, id=OO2$ID)

#Density graph
OO3 <- transform(OO3, Cortical.visual.impairement = as.factor(Cortical.visual.impairement))
cdplot(OO3$Cortical.visual.impairement ~ tsne_data$cluster, data=OO3)

# Bar graph
ggplot(OO3, 
       aes(x = OO3$Cortical.visual.impairement, 
           fill = tsne_data$cluster)) + 
  geom_bar(position = "dodge")
#-------------------------------------------------------------------------------
#p-value for clustered data
OO3 <- transform(OO3, Cataract = as.integer(Cataract))
levenetestClust(OO3$Cataract ~ tsne_data$cluster, id=OO2$ID)

#Density graph
OO3 <- transform(OO3, Cataract = as.factor(Cataract))
cdplot(OO3$Cataract ~ tsne_data$cluster, data=OO3)

# Bar graph
ggplot(OO3, 
       aes(x = OO3$Cataract, 
           fill = tsne_data$cluster)) + 
  geom_bar(position = "dodge")
#-------------------------------------------------------------------------------
#p-value for clustered data
OO3 <- transform(OO3, Nyctalopia = as.integer(Nyctalopia))
levenetestClust(OO3$Nyctalopia ~ tsne_data$cluster, id=OO2$ID)

#Density graph
OO3 <- transform(OO3, Nyctalopia = as.factor(Nyctalopia))
cdplot(OO3$Nyctalopia ~ tsne_data$cluster, data=OO3)

# Bar graph
ggplot(OO3, 
       aes(x = OO3$Nyctalopia, 
           fill = tsne_data$cluster)) + 
  geom_bar(position = "dodge")
#-------------------------------------------------------------------------------
#p-value for clustered data
OO3 <- transform(OO3, Maculopathy = as.integer(Maculopathy))
levenetestClust(OO3$Maculopathy ~ tsne_data$cluster, id=OO2$ID)

#Density graph
OO3 <- transform(OO3, Maculopathy = as.factor(Maculopathy))
cdplot(OO3$Maculopathy ~ tsne_data$cluster, data=OO3)

# Bar graph
ggplot(OO3, 
       aes(x = OO3$Maculopathy, 
           fill = tsne_data$cluster)) + 
  geom_bar(position = "dodge")
#-------------------------------------------------------------------------------
#p-value for clustered data
OO3 <- transform(OO3, Bilateral.hyperopia = as.integer(Bilateral.hyperopia))
levenetestClust(OO3$Bilateral.hyperopia ~ tsne_data$cluster, id=OO2$ID)

#Density graph
OO3 <- transform(OO3, Bilateral.hyperopia = as.factor(Bilateral.hyperopia))
cdplot(OO3$Bilateral.hyperopia ~ tsne_data$cluster, data=OO3)

# Bar graph
ggplot(OO3, 
       aes(x = OO3$Bilateral.hyperopia, 
           fill = tsne_data$cluster)) + 
  geom_bar(position = "dodge")
#-------------------------------------------------------------------------------
#p-value for clustered data
OO3 <- transform(OO3, Retinitis.pigmentosa = as.integer(Retinitis.pigmentosa))
levenetestClust(OO3$Retinitis.pigmentosa ~ tsne_data$cluster, id=OO2$ID)

#Density graph
OO3 <- transform(OO3, Retinitis.pigmentosa = as.factor(Retinitis.pigmentosa))
cdplot(OO3$Retinitis.pigmentosa ~ tsne_data$cluster, data=OO3)

# Bar graph
ggplot(OO3, 
       aes(x = OO3$Retinitis.pigmentosa, 
           fill = tsne_data$cluster)) + 
  geom_bar(position = "dodge")
#-------------------------------------------------------------------------------
#p-value for clustered data
OO3 <- transform(OO3, Blindness = as.integer(Blindness))
levenetestClust(OO3$Blindness ~ tsne_data$cluster, id=OO2$ID)

#Density graph
OO3 <- transform(OO3, Blindness = as.factor(Blindness))
cdplot(OO3$Blindness ~ tsne_data$cluster, data=OO3)

# Bar graph
ggplot(OO3, 
       aes(x = OO3$Blindness, 
           fill = tsne_data$cluster)) + 
  geom_bar(position = "dodge")
#-------------------------------------------------------------------------------
#p-value for clustered data
OO3 <- transform(OO3, Optic.Nerve.Atrophy = as.integer(Optic.Nerve.Atrophy))
levenetestClust(OO3$Optic.Nerve.Atrophy ~ tsne_data$cluster, id=OO2$ID)

#Density graph
OO3 <- transform(OO3, Optic.Nerve.Atrophy = as.factor(Optic.Nerve.Atrophy))
cdplot(OO3$Optic.Nerve.Atrophy ~ tsne_data$cluster, data=OO3)

# Bar graph
ggplot(OO3, 
       aes(x = OO3$Optic.Nerve.Atrophy, 
           fill = tsne_data$cluster)) + 
  geom_bar(position = "dodge")
#-------------------------------------------------------------------------------
#p-value for clustered data
OO3 <- transform(OO3, Nystagmus..TRUE = as.integer(Nystagmus..TRUE))
levenetestClust(OO3$Nystagmus..TRUE ~ tsne_data$cluster, id=OO2$ID)

#Density graph
OO3 <- transform(OO3, Nystagmus..TRUE = as.factor(Nystagmus..TRUE))
cdplot(OO3$Nystagmus..TRUE ~ tsne_data$cluster, data=OO3)

# Bar graph
ggplot(OO3, 
       aes(x = OO3$Nystagmus..TRUE, 
           fill = tsne_data$cluster)) + 
  geom_bar(position = "dodge")
#-------------------------------------------------------------------------------
#p-value for clustered data
OO3 <- transform(OO3, Pupils.2..Reactive = as.integer(Pupils.2..Reactive))
levenetestClust(OO3$Pupils.2..Reactive ~ tsne_data$cluster, id=OO2$ID)

#Density graph
OO3 <- transform(OO3, Pupils.2..Reactive = as.factor(Pupils.2..Reactive))
cdplot(OO3$Pupils.2..Reactive ~ tsne_data$cluster, data=OO3)

# Bar graph
ggplot(OO3, 
       aes(x = OO3$Pupils.2..Reactive, 
           fill = tsne_data$cluster)) + 
  geom_bar(position = "dodge")









