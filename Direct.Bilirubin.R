#packages
library(tidyverse)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
library(viridis)
library(viridisLite)
library (ggplot2)


Direct.Bili. <- read.csv("Direct.Bili.csv", header = TRUE)
#Removed 3000 outlier and everything >5

#Boxplot to remove extreme outliers
boxplot(Direct.Bili.$Direct.Bilirubin..mg.dL., main = "Direct.Bili.")
##Remove outliers >5

#Boxplots
Direct.Bili.$PBD.group <- factor(Direct.Bili.$PBD.group ,levels=c("Mild", "Mild and Intermediate", "Intermediate", "Severe"))


p <- ggplot(Direct.Bili., aes(x=PBD.group, y=Direct.Bilirubin..mg.dL., fill=PBD.group)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="blue", fill="blue") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set4")
p

res <- boxplot(Direct.Bilirubin..mg.dL. ~ PBD.group, data= Direct.Bili.)
res$stats

levels(Direct.Bili.$PBD.group)

library(dplyr)
group_by(Direct.Bili., PBD.group) %>%
  summarise(
    count = n(),
    mean = mean((Direct.Bilirubin..mg.dL.), na.rm = TRUE),
    sd = sd((Direct.Bilirubin..mg.dL.), na.rm = TRUE)
  )

# 95% Confidence Interval
library("gplots")
plotmeans(Direct.Bilirubin..mg.dL. ~ PBD.group, data = Direct.Bili., frame = FALSE,
          xlab = "PBD.group", ylab = "Direct.Bilirubin..mg.dL.",
          main="Mean Plot with 95% CI") 

#p-values
pairwise.t.test(Direct.Bili.$Direct.Bilirubin..mg.dL., Direct.Bili.$PBD.group,
                p.adjust.method = "BH")