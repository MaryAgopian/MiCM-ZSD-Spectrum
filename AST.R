#packages
library(tidyverse)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
library(viridis)
library(viridisLite)
library (ggplot2)


AST <- read.csv("AST.csv", header = TRUE)
#Removed outliers

#Boxplot to remove extreme outliers
boxplot(AST$AST..U.L., main = "AST")
##Remove outliers >300
#77 observations

#Boxplots
AST$PBD.group <- factor(AST$PBD.group ,levels=c("Mild", "Mild and Intermediate", "Intermediate", "Severe"))


p <- ggplot(AST, aes(x=PBD.group, y=AST..U.L., fill=PBD.group)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="blue", fill="blue") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set4")
p

res <- boxplot(AST..U.L. ~ PBD.group, data= AST)
res$stats

levels(AST$PBD.group)

library(dplyr)
group_by(AST, PBD.group) %>%
  summarise(
    count = n(),
    mean = mean((AST..U.L.), na.rm = TRUE),
    sd = sd((AST..U.L.), na.rm = TRUE)
  )

# 95% Confidence Interval
library("gplots")
plotmeans(AST..U.L. ~ PBD.group, data = AST, frame = FALSE,
          xlab = "PBD.group", ylab = "AST..U.L.",
          main="Mean Plot with 95% CI") 

#p-values
pairwise.t.test(AST$AST..U.L., AST$PBD.group,
                p.adjust.method = "BH")


