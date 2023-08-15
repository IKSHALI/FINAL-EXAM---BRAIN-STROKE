# install pre-requisite packages 

install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("stats")

# load required libraries

library(dplyr)
library(ggplot2)
library(gridExtra)
library(stats)
library(tidyverse)

# loading data onto mydata object

mydata = read.csv("brain_stroke.csv")

# display first 20 entries

head(mydata,20)

# finding sized/dimension of dataset

dim(mydata)

# viewing attributes and their data types

str(mydata)

## showing summary of data attributes nd their plots

# age

summary(mydata$age)
x <- mydata$age
h<-hist(x, breaks=15, col="purple", xlab="age in years",
        main="histogram with normal curve")
xfit<-seq(min(x),max(x),length=30)
yfit<-dnorm(xfit,mean = mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="green",lwd=2)

help("hist")

# glucose level

summary(mydata$avg_glucose_level)

class(mydata$avg_glucose_level)
gl_sorted<- sort(mydata$avg_glucose_level)
plot(mydata$avg_glucose_level,ylab = "average glucose level")
boxplot(mydata$avg_glucose_level,main="average_glucose_level")

# gender

summary(mydata$gender)

plot(mydata$gender)

# hypertension

summary(mydata$hypertension)

hist(mydata$hypertension)

# heart disease

summary(mydata$heart_disease)

hist(mydata$heart_disease)

# maritial status

summary(mydata$ever_married)

plot(mydata$ever_married,main = "Ever married")

# work type

summary(mydata$work_type)

plot(mydata$work_type,main = "work Type")

# bmi

summary(mydata$bmi)

plot(mydata$bmi)

# smoking status

summary(mydata$smoking_status)

# stroke 

summary(mydata$stroke)

plot(mydata$stroke,horiz=TRUE,main="people with and without strokes")


# viewing headers

names((mydata))

# checking errors in data

colSums(is.na(mydata))

colSums(mydata == "N/A")

# checking for duplicate values

n_occur <- data.frame(table(mydata$id))
n_occur[n_occur$Freq > 1]

sum(mydata$gender == "other")

# removing rows with N/A values of bmi and "other" value of gender

# and converting attributes into suitable data type

mydata <- mydata %>%
  mutate(
    gender = factor(gender),
    hypertension = factor(hypertension),
    heart_disease = factor(heart_disease),
    ever_married = factor(ever_married),
    work_type = factor(work_type),
    Residence_type = factor(Residence_type),
    bmi = as.numeric(bmi),
    smoking_status = factor(smoking_status),
    stroke = factor(stroke),
  )
 
str(mydata)
dim(mydata)

# removing column mydata$id

mydata<-mydata[ ,2:12]

summary(mydata)

# visualizing stroke occurence for each attribute

mydata %>%
  ggplot(aes(x=gender, fill = stroke)) + geom_bar()

mydata %>%
  ggplot(aes(x=hypertension, fill = stroke)) + geom_bar()

mydata %>%
  ggplot(aes(x=heart_disease, fill = stroke)) + geom_bar()

mydata %>%
  ggplot(aes(x=ever_married, fill = stroke)) + geom_bar()

mydata %>%
  ggplot(aes(x=work_type, fill = stroke)) + geom_bar()

mydata %>%
  ggplot(aes(x=Residence_type, fill = stroke)) + geom_bar()

mydata %>%
  ggplot(aes(x=smoking_status, fill = stroke)) + geom_bar()

# now visualizing people with stroke with certain attributes

dat_prop <- mydata %>%
  group_by(gender) %>%
  summarise(prop = sum(stroke == "1")/length(gender))

#dat_prop 

dat_prop %>%
  ggplot(aes(x = gender, y = prop)) + geom_col(fill = "#5C43AF")

dat_prop <- mydata %>%
  group_by(hypertension) %>%
  summarise(prop = sum(stroke == "1")/length(hypertension))

dat_prop %>%
  ggplot(aes(x = hypertension, y = prop)) + geom_col(fill = "#023FC4")

dat_prop <- mydata %>%
  group_by(heart_disease) %>%
  summarise(prop = sum(stroke == "1")/length(heart_disease))

dat_prop %>%
  ggplot(aes(x = heart_disease, y = prop)) + geom_col(fill = "#00BFC4")

dat_prop <- mydata %>%
  group_by(ever_married) %>%
  summarise(prop = sum(stroke == "1")/length(ever_married))

dat_prop %>%
  ggplot(aes(x = ever_married, y = prop)) + geom_col(fill = "#7FD0F9")

dat_prop <- mydata %>%
  group_by(work_type) %>%
  summarise(prop = sum(stroke == "1")/length(work_type))

dat_prop %>%
  ggplot(aes(x = work_type, y = prop)) + geom_col(fill = "#00bf89")

dat_prop <- mydata %>%
  group_by(Residence_type) %>%
  summarise(prop = sum(stroke == "1")/length(Residence_type))

dat_prop %>%
  ggplot(aes(x = Residence_type, y = prop)) + geom_col(fill = "#39da24")

dat_prop <- mydata %>%
  group_by(smoking_status) %>%
  summarise(prop = sum(stroke == "1")/length(smoking_status))

dat_prop %>%
  ggplot(aes(x = smoking_status, y = prop)) + geom_col(fill = "#ef5fc4")

# plotting boxplot for all numeric attributes

boxplot(mydata$bmi)
summary(mydata$bmi)

##  Descriptive Regression Analysis

# regression analysis

# plot

model = lm(age ~ stroke,data = mydata)

summary(model)

plot(mydata$age,mydata$stroke,
     xlab = "age",
     ylab = "stroke")

model = lm(avg_glucose_level ~ stroke,data = mydata)

summary(model)

plot(mydata$avg_glucose_level,mydata$stroke,
     xlab = "avg_glucose_level",
     ylab = "stroke")

model = lm(bmi ~ stroke,data = mydata)

summary(model)

plot(mydata$bmi,mydata$stroke,
     xlab = "bmi",
     ylab = "stroke")

abline(model)

# Nonlinear Model

# plot

non_linear_model = lm(age~stroke + I(stroke),
                      data = mydata)

summary(non_linear_model)

plot(mydata$stroke,mydata$age,
     xlab = "stroke",
     ylab = "age")

non_linear_model = lm(avg_glucose_level~stroke + I(stroke),
                      data = mydata)

summary(non_linear_model)

plot(mydata$stroke,mydata$avg_glucose_level,
     xlab = "stroke",
     ylab = "avg_glucose_level")

non_linear_model = lm(bmi~stroke + I(stroke),
                      data = mydata)

summary(non_linear_model)

plot(mydata$stroke,mydata$bmi,
     xlab = "stroke",
     ylab = "bmi")

# fit plot

lines(mydata$stroke,fitted(non_linear_model))

# Multiple Linear Regression

mydata

data = mydata

mydata = data[,c("age","avg_glucose_level","bmi","stroke")]

class(mydata)

mydata = as.data.frame(mydata)

# Multiple Regression Analysis

multiple_fit = lm(age ~ avg_glucose_level + bmi + stroke,
                  data = mydata)

summary(multiple_fit)
