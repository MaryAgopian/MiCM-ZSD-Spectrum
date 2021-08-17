#packages
library(tidyverse)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
library(viridis)
library(viridisLite)
library (ggplot2)


ALP <- read.csv("ALP.csv", header = TRUE)
#Removed outliers

#Boxplot to remove extreme outliers
boxplot(ALP$ALP..U.L., main = "ALP")
##Remove outliers >400
#68 observations

#Boxplots
ALP$PBD.group <- factor(ALP$PBD.group ,levels=c("Mild", "Mild and Intermediate", "Intermediate", "Severe"))


p <- ggplot(ALP, aes(x=PBD.group, y=ALP..U.L., fill=PBD.group)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="blue", fill="blue") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set4")
p

#This is just to get the stats of the boxplot
res <- boxplot(ALP..U.L. ~ PBD.group, data= ALP)
res$stats

#This is doublechecking that the levels are correctly represented
levels(ALP$PBD.group)

#This is for the mean and standard deviation
library(dplyr)
group_by(ALP, PBD.group) %>%
  summarise(
    count = n(),
    mean = mean((ALP..U.L.), na.rm = TRUE),
    sd = sd((ALP..U.L.), na.rm = TRUE)
  )

# 95% Confidence Interval
library("gplots")
plotmeans(ALP..U.L. ~ PBD.group, data = ALP, frame = FALSE,
          xlab = "PBD.group", ylab = "ALP..U.L.",
          main="Mean Plot with 95% CI") 

#p-values
pairwise.t.test(ALP$ALP..U.L., ALP$PBD.group,
                p.adjust.method = "BH")

