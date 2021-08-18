#packages
library(tidyverse)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
library(viridis)
library(viridisLite)
library (ggplot2)


ALT <- read.csv("ALT.csv", header = TRUE)

#Boxplot to remove extreme outliers
boxplot(ALT$ALT..U.L., main = "ALT..U.L.")
##Remove outliers >150

#Boxplots
ALT$PBD.group <- factor(ALT$PBD.group ,levels=c("Mild", "Mild and Intermediate", "Intermediate", "Severe"))


p <- ggplot(ALT, aes(x=PBD.group, y=ALT..U.L., fill=PBD.group)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="blue", fill="blue") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set4")
p

res <- boxplot(ALT..U.L. ~ PBD.group, data= ALT)
res$stats

levels(ALT$PBD.group)

library(dplyr)
group_by(ALT, PBD.group) %>%
  summarise(
    count = n(),
    mean = mean((ALT..U.L.), na.rm = TRUE),
    sd = sd((ALT..U.L.), na.rm = TRUE)
  )

# 95% Confidence Interval
library("gplots")
plotmeans(ALT..U.L. ~ PBD.group, data = ALT, frame = FALSE,
          xlab = "PBD.group", ylab = "ALT..U.L.",
          main="Mean Plot with 95% CI") 

#p-values
pairwise.t.test(ALT$ALT..U.L., ALT$PBD.group,
                p.adjust.method = "BH")







