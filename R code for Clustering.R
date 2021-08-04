#Open dataset
OO2 <- read.csv("Book1.csv", header = TRUE, stringsAsFactors = FALSE)

#Remove ID and PBD group for clustering
OO3 <- OO2[3:26]

#Transform all binary variables into integers
OO3 <- transform(OO3, Cortical.visual.impairement = as.integer(Cortical.visual.impairement))
OO3 <- transform(OO3, Cataract = as.integer(Cataract))
OO3 <- transform(OO3, Nyctalopia = as.integer(Nyctalopia))
OO3 <- transform(OO3, Maculopathy = as.integer(Maculopathy))
OO3 <- transform(OO3, Bilateral.hyperopia = as.integer(Bilateral.hyperopia))
OO3 <- transform(OO3, Retinitis.pigmentosa = as.integer(Retinitis.pigmentosa))
OO3 <- transform(OO3, Blindness = as.integer(Blindness))
OO3 <- transform(OO3, Optic.Nerve.Atrophy = as.integer(Optic.Nerve.Atrophy))
OO3 <- transform(OO3, Nystagmus..TRUE = as.integer(Nystagmus..TRUE))
OO3 <- transform(OO3, Peripheral.retina..Leopard.spots = as.integer(Peripheral.retina..Leopard.spots))
OO3 <- transform(OO3, Vessels..Attenuated.vessels = as.integer(Vessels..Attenuated.vessels))
OO3 <- transform(OO3, Pupils.1..Equal = as.integer(Pupils.1..Equal))
OO3 <- transform(OO3, Pupils.2..Reactive = as.integer(Pupils.2..Reactive))
OO3 <- transform(OO3, Nystagmus..TRUE = as.integer(Nystagmus..TRUE))
OO3 <- transform(OO3, Macula..Normal.Appearing = as.integer(Macula..Normal.Appearing))
OO3 <- transform(OO3, Macular.Pigmentary.changes = as.integer(Macular.Pigmentary.changes))
OO3 <- transform(OO3, Macular.edema = as.integer(Macular.edema))
OO3 <- transform(OO3, Foveal.hypoplasia = as.integer(Foveal.hypoplasia))
OO3 <- transform(OO3, Macular.thinning = as.integer(Macular.thinning))
OO3 <- transform(OO3, Optic.nerve.hypoplasia = as.integer(Optic.nerve.hypoplasia))
OO3 <- transform(OO3, Optic.nerve.drusen = as.integer(Optic.nerve.drusen))
OO3 <- transform(OO3, Optic.nerve..Normal.appearing = as.integer(Optic.nerve..Normal.appearing))
OO3 <- transform(OO3, Optic.nerve.atrophy = as.integer(Optic.nerve.atrophy))

#Double check that all binaries were transformed into integers
str(OO3)

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
for(i in 2:8){  
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
#---------------------------------------------------------------------------------------------------------------------------------------
#Example of my chi-squared and p-value package on only the Cataract variable 

#Desnity graph
OO3 <- transform(OO3, Cataract = as.factor(Cataract))
cdplot(OO3$Cataract ~ tsne_data$cluster, data=OO3)

# Bar graph
ggplot(OO3, 
       aes(x = OO3$Cataract, 
           fill = tsne_data$cluster)) + 
  geom_bar(position = "dodge")


#p-value for clustered data

install.packages("htestClust")
library(htestClust)

levenetestClust(OO3$Cataract ~ tsne_data$cluster, id=OO2$ID)
#---------------------------------------------------------------------------------------------------------------------------------------
