#packages
library(tidyverse)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
library(viridis)
library(viridisLite)
library (ggplot2)


Total.Bili. <- read.csv("Total.Bili.csv", header = TRUE)
head(Total.Bili.)
str(Total.Bili.)

Total.Bili.na <- na.omit(Total.Bili.)
#81 (removed 3000 outlier) observations with 3 severity degress

#Boxplot to remove extreme outliers
boxplot(Total.Bili.na$Total.Bilirubin..mg.dL., main = "Total.Bili.")
##Remove outliers >5


#Boxplots
Total.Bili.na$PBD.group <- factor(Total.Bili.na$PBD.group ,levels=c("Mild", "Mild and Intermediate", "Intermediate", "Severe"))


boxplot(Total.Bili.na$Total.Bilirubin..mg.dL. ~ Total.Bili.na$PBD.group , col=rgb(0.3,0.5,0.4,0.6) , ylab="Bilirubin" , 
        xlab="Severity")

p <- ggplot(Total.Bili.na, aes(x=PBD.group, y=Total.Bilirubin..mg.dL., fill=PBD.group)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="blue", fill="blue") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set4")
p

res <- boxplot(Total.Bilirubin..mg.dL. ~ PBD.group, data= Total.Bili.na)
res$stats


levels(Total.Bili.na$PBD.group)

library(dplyr)
group_by(Total.Bili.na, PBD.group) %>%
  summarise(
    count = n(),
    mean = mean((Total.Bilirubin..mg.dL.), na.rm = TRUE),
    sd = sd((Total.Bilirubin..mg.dL.), na.rm = TRUE)
  )

# 95% Confidence Interval
library("gplots")
plotmeans(Total.Bilirubin..mg.dL. ~ PBD.group, data = Total.Bili.na, frame = FALSE,
          xlab = "PBD.group", ylab = "Total.Bilirubin..mg.dL.",
          main="Mean Plot with 95% CI") 

#p-values
pairwise.t.test(Total.Bili.na$Total.Bilirubin..mg.dL., Total.Bili.na$PBD.group,
                p.adjust.method = "BH")
