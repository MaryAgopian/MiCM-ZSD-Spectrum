#Upload file
Albumin <- read.csv("Albumin.csv", header = TRUE)
head(Albumin)
str(Albumin)

#packages
library(tidyverse)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
library(viridis)
library(viridisLite)
library (ggplot2)
library(ggpubr)
library(rstatix)

Albumin.na <- na.omit(Albumin)
#76 observations

#Boxplot to remove extreme outliers
boxplot(Albumin.na$Albumin..g.L., main = "Albumin")
##Only one outlier with 20, 70, 80

#Try again without the extreme outlier
Albumin.copy <- read.csv("Albumin copy.csv", header = TRUE)
Albumincopy.na <- na.omit(Albumin.copy)
#Now 73 observations, since I removed that outlier

boxplot(Albumincopy.na$Albumin..g.L., main = "Albumin")
##Remove the seven outliers as well


#Boxplots
Albumincopy.na$PBD.group <- factor(Albumincopy.na$PBD.group ,levels=c("Mild", "Mild and Intermediate", "Intermediate", "Severe"))


boxplot(Albumincopy.na$Albumin..g.L. ~ Albumincopy.na$PBD.group , col=rgb(0.3,0.5,0.4,0.6) , ylab="Albumin" , 
        xlab="Severity")

p <- ggplot(Albumincopy.na, aes(x=PBD.group, y=Albumin..g.L., fill=PBD.group)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="blue", fill="blue") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set4")
p

##?Add in normal range???????????????????????????


#mean and standard deviation
levels(Albumincopy.na$PBD.group)

library(dplyr)
group_by(Albumincopy.na, PBD.group) %>%
  summarise(
    count = n(),
    mean = mean((Albumin..g.L.), na.rm = TRUE),
    sd = sd((Albumin..g.L.), na.rm = TRUE)
  )


#95% Confidence Interval
library("gplots")
plotmeans(Albumin..g.L. ~ PBD.group, data = Albumincopy.na, frame = FALSE,
          xlab = "PBD.group", ylab = "Albumin..g.L.",
          main="Mean Plot with 95% CI") 

#p-values
pairwise.t.test(Albumincopy.na$Albumin..g.L., Albumincopy.na$PBD.group,
                p.adjust.method = "BH")