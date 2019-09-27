######################
## R Lab #1
######################
## Basics, Assigning Variables
1 + 1
x <- 1 + 1
x
print(x)
(y <- 3 + 4)

## Be careful with the assignment operator!
a <- 1
b = 2 
system.time(c <- 3)
system.time(d = 4)

## Logical Operators
1 == 1
1 != 2
1 == 1 & 1 == 2
1 == 1 | 1 == 2
xor(1 == 1, 1 != 2)

## Creating and indexing vectors
myvec <- c(1, 2, 3, 4)
myvec
myvec <- c(myvec, 5)
myvec[1]
myvec[1:3]
myvec[c(1, 3)] # return first and third element of vector 
myvec[myvec == 1] # return all of the elements in the vector that have a value of "1" 

## Standard functions for summarizing data
mean(myvec)
sd(myvec)
range(myvec)
min(myvec)
max(myvec)

## Creating and indexing in matrices
mymat <- matrix(c(1:10, 21:30), ncol = 2)
mymat
mymat[3, 2]
mymat[3, ] # leaving an index blank means "all"
mymat[, 2]
mean(mymat[, 2]) # get mean of the second column
mymat[mymat[, 1] == 5, ] # all the rows which have a "5" in the first column

## Basic Data Frame Manipulations
fake.data <- data.frame(subject = c(rep(1, 3), rep(2, 3), rep(3, 3)),
                        RT = c(300, 400, 500, 320, 440, 450, 250, 300, 400),
                        Condition = rep(c("Condition 1", "Condition 2", "Condition 3"), 3))
fake.data[, 3]
fake.data$Condition # does same thing as last line
fake.data[, "Condition"]
fake.data[fake.data$Condition == "Condition 1", ] # eww, ugly!
subset(fake.data, Condition == "Condition 1") # much easier!
mean(subset(fake.data, Condition == "Condition 1")$RT)
mean(subset(fake.data, Condition == "Condition 2")$RT)
mean(subset(fake.data, Condition == "Condition 3")$RT)

## Loading Data
getwd()
setwd("~/Dropbox/Courses/CSP519_SP17/wbushong_labs/Lab1")
boys <- read.csv("theboys.csv")
boys

## Loading an SPSS data file
library(foreign) # library that allows you to read in .sav files
# if you don't have the library, you can install by uncommenting the following line, then calling the library() command again
# install.packages("foreign")

example <- read.spss("ExampleData1.sav", to.data.frame = TRUE)
dim(example)
head(example, 3)
example[1, ]                   # first row (all columns)
example[2:3, ]                 # second through third row
example[, 1]                   # first column (all rows)
example[, -(2:10)]             # exclude columns two through ten
example[1, c("Gender", "Age")] # returns values for gender and age for first case
example[1, c(16, 15)]          # yields identical result

## Summarise data in interesting ways 
summary(example)
example$ID <- as.factor(example$ID)
str(example)
mean(example$Age)
median(example$Age)
var(example$Age)
sd(example$Age)
subset(example, Age > 18)
mean(subset(example, Gender == "M")$Age)
mean(subset(example, Gender == "F")$Age)
rowMeans(example[, c("SOS1", "SOS2", "SOS3", "SOS4")])

## Exclude rows with NAs
example[!complete.cases(example), ] # "!" means "not"
example_complete <- example[complete.cases(example), ]
dim(example_complete)
dim(example)

## Merge with other data
psych_majors <- read.spss("PsychMajors.sav", to.data.frame = TRUE)
dim(psych_majors)
head(psych_majors)

other_majors <- read.spss("OtherMajors.sav", to.data.frame = TRUE)
dim(other_majors)

all_majors <- rbind(psych_majors, other_majors)
dim(all_majors)

gender <- read.spss("Gender.sav", to.data.frame = TRUE)
head(gender, 3)

all_majors <- merge(all_majors, gender)
head(all_majors)

## Add new columns
all_majors$Mean_SR1_SR2 <- rowMeans(all_majors[, c("SR1", "SR2")])
head(all_majors)
all_majors$GPA_Category <- ifelse(all_majors$GPA > 3, "High GPA", "Low GPA")

## Write data to .csv file
write.csv(all_majors, "AllMajors.csv", row.names = FALSE)