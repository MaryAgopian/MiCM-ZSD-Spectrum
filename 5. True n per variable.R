# Goal is to look at the "true ns" of each column and determine wether or not there is enough data to keep.
optho <- read.csv("June19_optho.csv")



#--------------------------------------------------------------------------------------------------------------------------------
##Goal: Calculate true n for each column in this file

#------------Part1- true n for OpthoAgeAtExam------------
#How many OpthoAgeAtExam do we have in the database?
##1) Create dataframe with just those two variables.
OpthoAgeandID <- optho[c(1:882),c(1,6)]
##2) Calculate unique IDs
#How many IDs do we have in the database?
OpthoAgeIDcount <- OpthoAgeandID$ID
OpthoAgeIDcountuniq <- unique(OpthoAgeIDcount)
length(OpthoAgeIDcountuniq)
#we have 119 unique data in the database, this means we need to have (smaller than or equal to) 119 in each column


#------------Part2- true n for Refraction OD R------------
##1) Create dataframe with just those two variables.
RODRandID <- optho[c(1:882),c(1,39)]
##2) Remove any missing values (NAs)
RODR <- na.omit(RODRandID)
##3)How many IDs do we have in the database?
RODRIDcount <- RODR$ID
RODRIDcount
RODRIDcountuniq <- unique(RODRIDcount)
RODRIDcountuniq
length(RODRIDcountuniq)
##We have n=53 ((53/119)*100= approx 44%)

#------------Part3- true n for Refraction OD L------------
##1) Create dataframe with just those two variables.
RODRLandID <- optho[c(1:882),c(1,40)]
##2) Remove any missing values (NAs)
RODRL <- na.omit(RODRLandID)
##3)How many IDs do we have in the database?
RODRIDLcount <- RODRL$ID
RODRIDLcountuniq <- unique(RODRIDLcount)
length(RODRIDLcountuniq)
##We have n=53 ((53/119)*100= approx 44%), matches RODR (right eye)

#------------Part4- true n for Refraction Cyl (right)------------
##1) Create dataframe with just those two variables.
RCYL <- optho[c(1:882),c(1,41)]
##2) Remove any missing values (NAs)
RCYL.omit <- na.omit(RCYL)
##3)How many IDs do we have in the database?
RCYLcount <- RCYL.omit$ID
RCYLcountuniq <- unique(RCYLcount)
length(RCYLcountuniq)
##We have n=40 ((40/119)*100= approx 34%)

#------------Part5- true n for Refraction CYL (left)------------
##1) Didn't automatically add NA for blanks
optho$Refraction_Cyl_L <- as.numeric(as.character(optho$Refraction_Cyl_L))
##2) Create dataframe with just those two variables.
RCYLL <- optho[c(1:882),c(1,42)]
##3) Remove any missing values (NAs)
RCYLL.omit <- na.omit(RCYLL)
##4)How many IDs do we have in the database?
RCYLLcount <- RCYLL.omit$ID
RCYLLcountuniq <- unique(RCYLLcount)
length(RCYLLcountuniq)
##We have n=37 ((37/119)*100= approx 31%; doesn't match up right eye)

#------------Part6- true n for CFT------------
##1) Create dataframe with just those two variables.
CFT <- optho[c(1:882),c(1,47)]
##2) Remove any missing values (NAs)
CFT.omit <- na.omit(CFT)
##3)How many IDs do we have in the database?
CFTcount <- CFT.omit$ID
CFTcountuniq <- unique(CFTcount)
length(CFTcountuniq)
##We have n=8 ((8/119)*100= approx 7%)

#------------Part7- true n for Mean Thickness------------
##1) Create dataframe with just those two variables.
MT <- optho[c(1:882),c(1,48)]
##2) Remove any missing values (NAs)
MT.omit <- na.omit(MT)
##3)How many IDs do we have in the database?
MTcount <- MT.omit$ID
MTcountuniq <- unique(MTcount)
length(MTcountuniq)
##We have n=6 ((6/119)*100= approx 5%)

#------------Part8- true n for Total Volumne------------
##1) Create dataframe with just those two variables.
TV <- optho[c(1:882),c(1,49)]
##2) Remove any missing values (NAs)
TV.omit <- na.omit(TV)
##3)How many IDs do we have in the database?
TVcount <- TV.omit$ID
TVcountuniq <- unique(TVcount)
length(TVcountuniq)
##We have n=6 ((6/119)*100= approx 5%)

#------------Part9- true n for Total Volumne------------
##1) Create dataframe with just those two variables.
TV <- optho[c(1:882),c(1,49)]
##2) Remove any missing values (NAs)
TV.omit <- na.omit(TV)
##3)How many IDs do we have in the database?
TVcount <- TV.omit$ID
TVcountuniq <- unique(TVcount)
length(TVcountuniq)
##We have n=6 ((6/119)*100= approx 5%)

#------------Part9- true n for Latency------------
##1) Create dataframe with just those two variables.
Lat <- optho[c(1:882),c(1,58)]
##2) Remove any missing values (NAs)
Lat.omit <- na.omit(Lat)
##3)How many IDs do we have in the database?
Latcount <- Lat.omit$ID
Latcountuniq <- unique(Latcount)
length(Latcountuniq)
##We have n=3 ((3/119)*100= approx 3%)

#------------Part10- true n for Horizontal Span------------
##1) Create dataframe with just those two variables.
HS <- optho[c(1:882),c(1,67)]
##2) Remove any missing values (NAs)
HS.omit <- na.omit(HS)
##3)How many IDs do we have in the database?
HScount <- HS.omit$ID
HScountuniq <- unique(HScount)
length(HScountuniq)
##We have n=10 ((10/119)*100= approx 8%)

#------------Part11- true n for Left to Midline------------
##1) Create dataframe with just those two variables.
L2M <- optho[c(1:882),c(1,68)]
##2) Remove any missing values (NAs)
L2M.omit <- na.omit(L2M)
##3)How many IDs do we have in the database?
L2Mcount <- L2M.omit$ID
L2Mcountuniq <- unique(L2Mcount)
length(L2Mcountuniq)
##We have n=7 ((7/119)*100= approx 6%)

#------------Part12- true n for Right to Midline------------
##1) Create dataframe with just those two variables.
R2M <- optho[c(1:882),c(1,69)]
##2) Remove any missing values (NAs)
R2M.omit <- na.omit(R2M)
##3)How many IDs do we have in the database?
R2Mcount <- R2M.omit$ID
R2Mcountuniq <- unique(R2Mcount)
length(R2Mcountuniq)
##We have n=7 ((7/119)*100= approx 6%; same as Left to Midline)

#------------Part13- true n for Vertical Span------------
##1) Create dataframe with just those two variables.
VS <- optho[c(1:882),c(1,70)]
##2) Remove any missing values (NAs)
VS.omit <- na.omit(VS)
##3)How many IDs do we have in the database?
VScount <- VS.omit$ID
VScountuniq <- unique(VScount)
length(VScountuniq)
##We have n=10 ((10/119)*100= approx 8%)

#------------Part13- true n for Above Midline------------
##1) Create dataframe with just those two variables.
AM <- optho[c(1:882),c(1,71)]
##2) Remove any missing values (NAs)
AM.omit <- na.omit(AM)
##3)How many IDs do we have in the database?
AMcount <- AM.omit$ID
AMcountuniq <- unique(AMcount)
length(AMcountuniq)
##We have n=7 ((7/119)*100= approx 6%)

#------------Part14- true n for Below Midline------------
##1) Create dataframe with just those two variables.
BM <- optho[c(1:882),c(1,72)]
##2) Remove any missing values (NAs)
BM.omit <- na.omit(BM)
##3)How many IDs do we have in the database?
BMcount <- BM.omit$ID
BMcountuniq <- unique(BMcount)
length(BMcountuniq)
##We have n=7 ((7/119)*100= approx 6%; same as Above Midline)

#------------Part15- true n for Horizontal Span_OS------------
##1) Create dataframe with just those two variables.
HSOS <- optho[c(1:882),c(1,73)]
##2) Remove any missing values (NAs)
HSOS.omit <- na.omit(HSOS)
##3)How many IDs do we have in the database?
HSOScount <- HSOS.omit$ID
HSOScountuniq <- unique(HSOScount)
length(HSOScountuniq)
##We have n=10 ((10/119)*100= approx 8%)

#------------Part16- true n for Left Midline_OS------------
##1) Create dataframe with just those two variables.
LMOS <- optho[c(1:882),c(1,74)]
##2) Remove any missing values (NAs)
LMOS.omit <- na.omit(LMOS)
##3)How many IDs do we have in the database?
LMOScount <- LMOS.omit$ID
LMOScountuniq <- unique(LMOScount)
length(LMOScountuniq)
##We have n=7 ((7/119)*100= approx 6%)

#------------Part17- true n for Right Midline_OS------------
##1) Create dataframe with just those two variables.
RMOS <- optho[c(1:882),c(1,75)]
##2) Remove any missing values (NAs)
RMOS.omit <- na.omit(RMOS)
##3)How many IDs do we have in the database?
RMOScount <- RMOS.omit$ID
RMOScountuniq <- unique(RMOScount)
length(RMOScountuniq)
##We have n=7 ((7/119)*100= approx 6%, Same as Left Midline_OS)

#------------Part18- true n for Vertical Span_OS------------
##1) Create dataframe with just those two variables.
VSOS <- optho[c(1:882),c(1,76)]
##2) Remove any missing values (NAs)
VSOS.omit <- na.omit(VSOS)
##3)How many IDs do we have in the database?
VSOScount <- VSOS.omit$ID
VSOScountuniq <- unique(VSOScount)
length(VSOScountuniq)
##We have n=10 ((10/119)*100= approx 8%)

#------------Part19- true n for Above Midline_OS------------
##1) Create dataframe with just those two variables.
AMOS <- optho[c(1:882),c(1,77)]
##2) Remove any missing values (NAs)
AMOS.omit <- na.omit(AMOS)
##3)How many IDs do we have in the database?
AMOScount <- AMOS.omit$ID
AMOScountuniq <- unique(AMOScount)
length(AMOScountuniq)
##We have n=7 ((7/119)*100= approx 6%)

#------------Part20- true n for Below Midline_OS------------
##1) Create dataframe with just those two variables.
BMOS <- optho[c(1:882),c(1,78)]
##2) Remove any missing values (NAs)
BMOS.omit <- na.omit(BMOS)
##3)How many IDs do we have in the database?
BMOScount <- BMOS.omit$ID
BMOScountuniq <- unique(BMOScount)
length(BMOScountuniq)
##We have n=7 ((7/119)*100= approx 6%, same as Above Midline_OS)

