#Upload data
data <- read.csv("Cleaned_Total Query 2_Ophthalmology_Mary.csv")

#Double-check everything is in order
length(data$ID)
## You should get 982
length(data$Ophthalmo_AgeAtExam)
## Also 982, this shows that there are no missing columns or mis-matched columns
length(unique(data$Ophthalmo_AgeAtExam))
## There are 41 different ages in our sample

#Make a histogram of Ophthalmo_AgeAtExam
hist(data$Ophthalmo_AgeAtExam, 
     main="Histogram for Patients' Age at Exam", 
     xlab="Age at Exams", 
     border="blue", 
     col="green")

#Make a boxplot of Ophthalmo_AgeAtExam
boxplot(data$Ophthalmo_AgeAtExam, 
        main="Boxplot for Patients' Age at Exam", 
        border="blue", 
        col="grey")

#Double-checking summary output
summary(data$Ophthalmo_AgeAtExam)

## This is the results
#Min.   1st Qu.  Median    Mean  3rd Qu.    Max. 
#0.000   1.000   4.000   6.981  10.000    48.000 

#Downloading stargazer package for tables
install.packages("stargazer")
library(stargazer)

#this package allows for the creation of neatly organised summary statistics tables
stargazer(data[c("Ophthalmo_AgeAtExam","Refraction_OD_R","Refraction_OS_L","Refraction_Cyl_R","Refraction_degrees_R","Refraction_degrees_L","Central.Foveal.Thickness..um.","Mean.thickness..um.","Total.volume..mm.3.","Central.Foveal.Thickness..um..1","Mean.thickness..um..1","Total.volume..mm.3..1","Latency.of.p100.wave","Horizontal.span","Left.to.midline","Right.to.midline","Vertical.span","Above.midline","Below.midline","HorizonSpan_OS","LeftMidline_OS","RightMidline_OS","VerticalSpan_OS","AboveMidline_OS","BelowMidline_OS")], type = "text", title="Descriptive statistics for Ophthalmology", out="descriptivestatsOphtha.txt", digits = 1)

#Exporting to LaTeX, then just copy the generated code into LaTeX for a nicely formatted table
stargazer(data[c("Ophthalmo_AgeAtExam","Refraction_OD_R","Refraction_OS_L","Refraction_Cyl_R","Refraction_degrees_R","Refraction_degrees_L","Central.Foveal.Thickness..um.","Mean.thickness..um.","Total.volume..mm.3.","Central.Foveal.Thickness..um..1","Mean.thickness..um..1","Total.volume..mm.3..1","Latency.of.p100.wave","Horizontal.span","Left.to.midline","Right.to.midline","Vertical.span","Above.midline","Below.midline","HorizonSpan_OS","LeftMidline_OS","RightMidline_OS","VerticalSpan_OS","AboveMidline_OS","BelowMidline_OS")], type = "latex", title="Descriptive statistics for Ophthalmology", out="descriptivestatsOphtha.tex", digits = 1)
