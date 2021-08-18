#Upload file
LabsLiver <- read.csv("LL Clustering and Regression.csv", header = TRUE)
head(LabsLiver)
str(LabsLiver)

#packages
library(tidyverse)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
library(viridis)
library(viridisLite)
library (ggplot2)

#Upload specific file for each boxplot
Total.Protein <- read.csv("Total protein.csv", header = TRUE)

Total.Protein.na <- na.omit(Total.Protein)
#72 observations with 3 severity degress

boxplot(Total.Protein.copy.na$Total.protein..g.L., main = "Total.protein..g.L.")
##Remove the other two outliers as well, y= 120 and y= 8
##Now 69 observations, removed other 2 outliers

#create boxplot that displays protein level distribution for each severity level in the dataset
WordsTotalProtein <- read.csv("Total protein copy 2.csv", header = TRUE)
head(WordsTotalProtein)
str(WordsTotalProtein)

Total.Protein.Words.na <- na.omit(WordsTotalProtein)

#Boxplots
Total.Protein.Words.na$PBD.group <- factor(Total.Protein.Words.na$PBD.group ,levels=c("Mild", "Mild and Intermediate", "Intermediate", "Severe"))


boxplot(Total.Protein.Words.na$Total.protein..g.L. ~ Total.Protein.Words.na$PBD.group , col=rgb(0.3,0.5,0.4,0.6) , ylab="Total.protein..g.L." , 
        xlab="Severity")

p <- ggplot(Total.Protein.Words.na, aes(x=PBD.group, y=Total.protein..g.L., fill=PBD.group)) +
        geom_boxplot(alpha=0.7) +
        stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="blue", fill="blue") +
        geom_jitter(color="black", size=0.4, alpha=0.9) +
        theme(legend.position="none") +
        scale_fill_brewer(palette="Set4")
p

res <- boxplot(Total.protein..g.L. ~ PBD.group, data=Total.Protein.Words.na)
res$stats

levels(Total.Protein.Words.na$PBD.group)

library(dplyr)
group_by(Total.Protein.Words.na, PBD.group) %>%
        summarise(
                count = n(),
                mean = mean((Total.protein..g.L.), na.rm = TRUE),
                sd = sd((Total.protein..g.L.), na.rm = TRUE)
        )


# 95% Confidence Interval
library("gplots")
plotmeans(Total.protein..g.L. ~ PBD.group, data = Total.Protein.Words.na, frame = FALSE,
          xlab = "PBD.group", ylab = "Total.protein..g.L.",
          main="Mean Plot with 95% CI") 

#p-values
pairwise.t.test(Total.Protein.Words.na$Total.protein..g.L., Total.Protein.Words.na$PBD.group,
                p.adjust.method = "BH")

