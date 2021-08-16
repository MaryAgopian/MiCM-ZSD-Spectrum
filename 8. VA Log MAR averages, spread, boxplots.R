#Calculating averages for LogMAR VAs Right eye
VA.right <- read.csv("Right.eye.LogMarframe copy.csv")

VA.rightt <- VA.right$Right.eye.LogMar

#Doubkechecking everything is a number
str(VA.rightt)

#Now stats

#########AVERAGES--------------------------------------------------------
mean(VA.rightt)
median(VA.rightt)

#Mode is a bit complex
# 1. Create the function.
getmode <- function(VA.rightt) {
  uniqv <- unique(VA.rightt)
  uniqv[which.max(tabulate(match(VA.rightt, uniqv)))]
}

#2. Now applying the function
result.mode <- getmode(VA.rightt)
print(result.mode)


#########SPREAD-------------------------------------------------------------
range(VA.rightt)
var(VA.rightt)
sd(VA.rightt)

###########BOX PLOT---------------------------------------------
boxplot(VA.right$Right.eye.LogMar, main="VA Right eye", ylab="LogMAR values")


summary(VA.right$Right.eye.LogMar)
################################---------------
################################---------------
#Calculating averages for LogMAR VAs Left eye
VA.left <- read.csv("Left.eye.LogMarframe copy.csv")

VA.leftt <- VA.left$Left.eye.LogMar

#Doubkechecking everything is a number
str(VA.leftt)

#Now stats

#########AVERAGES--------------------------------------------------------
mean(VA.leftt)
median(VA.leftt)

#Mode is a bit complex
# 1. Create the function.
getmode <- function(VA.leftt) {
  uniqv <- unique(VA.leftt)
  uniqv[which.max(tabulate(match(VA.leftt, uniqv)))]
}

#2. Now applying the function
result.mode <- getmode(VA.leftt)
print(result.mode)


#########SPREAD-------------------------------------------------------------
range(VA.leftt)
var(VA.leftt)
sd(VA.leftt)

###########BOX PLOT---------------------------------------------
boxplot(VA.leftt, main="VA Left eye", ylab="LogMAR values")

summary(VA.leftt)

################################---------------
################################---------------
#Calculating averages for LogMAR VAs bilaterral
VA.both <- read.csv("Both.eye.LogMarframe copy.csv")

VA.botth <- VA.both$Both.eye.LogMar

#Doubkechecking everything is a number
str(VA.botth)

#Now stats

#########AVERAGES--------------------------------------------------------
mean(VA.botth)
median(VA.botth)

#Mode is a bit complex
# 1. Create the function.
getmode <- function(VA.botth) {
  uniqv <- unique(VA.botth)
  uniqv[which.max(tabulate(match(VA.botth, uniqv)))]
}

#2. Now applying the function
result.mode <- getmode(VA.botth)
print(result.mode)


#########SPREAD-------------------------------------------------------------
range(VA.botth)
var(VA.botth)
sd(VA.botth)

###########BOX PLOT---------------------------------------------
boxplot(VA.botth, main="VA Bilateral", ylab="LogMAR values")

summary(VA.botth)