# setwd("~/Desktop/STAT215A/Lab4/image_data")
# setwd("/Users/soeren/Documents/215Homework/Lab4/stats215a")
# setwd("~/stats215a")

library(ggplot2)
library(rgl)
# library(YaleToolkit)
library(dplyr)
library(reshape2)

source("helper_functions.R")
load("image_data/rawD.RData")
set.seed(1)
TrD <- rawD[sample(1:nrow(rawD), 10000), ]
TeD <- rawD[sample(1:nrow(rawD), 10000), ]

Logit3probabilities <- function(Traind = TrD, Testd = TeD,
                             scale ="noscale"){
  # This is the logistic regression model based on the the three variables
  # NDAI, CORR and DF. It returns the probability for a datapoint in Testd to be
  # one, trained on the dataset.
  # Args:
  # Traind must have the columns NDAI, CORR, DF
  # Testd should be of the same form as Traind, but of course can have different
  # amount of rows
  # scale: variable which allows us to choose, whether we want to scale image by
  # image or the entire dataset at once.
  # Outp:
  # The predicted values for Testd
  if(scale == "byimage") {
    Traind <- scalebyimage(Traind)
    Testd <- scalebyimage(Testd)
  }
  if(scale == "complete") {
    Traind <- scalecomplete(Traind)
    Testd <- scalecomplete(Testd)
  }
  #remove 0 and make  -1 -> 0, because otherwise glm does not work!
  Traind <- filter(Traind, label !=0) %>% mutate(label = (label + 1)/2)
  
  Model <- glm(label ~ NDAI + CORR + DF, data = Traind, family = "binomial")
  s <- predict(Model, Testd)
  exp(s)/(exp(s)+1) #inverse of the logit function to make values probabilites
}



LogitAllprobabilities <- function(Traind = TrD, Testd = TeD,
                                scale ="noscale"){
  # This is the logistic regression model based on the the three variables
  # NDAI, CORR and DF. It returns the probability for a datapoint in Testd to be
  # one, trained on the dataset.
  # Args:
  # Traind must have the columns NDAI, logSD, CORR, DF, CF, BF, AF, AN
  # Testd should be of the same form as Traind, but of course can have different
  # amount of rows
  # scale: variable which allows us to choose, whether we want to scale image by
  # image or the entire dataset at once.
  # Outp:
  # The predicted values for Testd
  if(scale == "byimage") {
    Traind <- scalebyimage(Traind)
    Testd <- scalebyimage(Testd)
  }
  if(scale == "complete") {
    Traind <- scalecomplete(Traind)
    Testd <- scalecomplete(Testd)
  }
  #remove 0 and make  -1 -> 0, because otherwise glm does not work!
  Traind <- filter(Traind, label !=0) %>% mutate(label = (label + 1)/2)
  
  Model <- glm(label ~ NDAI + logSD + CORR + DF + CF + BF + AF + AN, data = Traind, family = "binomial")
  s <- predict(Model, Testd)
  exp(s)/(exp(s)+1) #inverse of the logit function to make values probabilites
}






### Test function

if(round(Logit3probabilities(Traind = TrD, Testd = TeD, scale ="byimage")[1],5)
   != 0.08871) stop("Logit3probabilities is not working correct!")

if(round(LogitAllprobabilities(Traind = TrD, Testd = TeD, scale ="byimage")[1],5)
   != 0.08595) stop("LogitAllprobabilities is not working correct!")


Logit3prediction <- function(Traind = rawD[1:1e5,], Testd = rawD[(1e5+1):2e5,],
                                scale ="noscale", threshold =.5){
  # this funciton predicts coulds(1) and no clouds (-1)
  # Args:
  # Traind: must have the columns NDAI, CORR, DF
  # Testd: should be of the same form as Traind, but of course can have different
  # amount of rows
  # scale: variable which allows us to choose, whether we want to scale image by
  # image or the entire dataset at once.
  # threshold: The likelihood at which we will say cloud. >threshod => Cloud
  # Outp:
  # The predicted values for Testd
  
  Probabilities <- Logit3probabilities(Traind, Testd, scale ="byimage")
  # the Probabilites contains the likelihood for beeing 1. Thus in the other case
  # it should be 0
  return(as.numeric(Probabilities > threshold)*2 -1)
}

LogitAllprediction <- function(Traind = rawD[1:1e5,], Testd = rawD[(1e5+1):2e5,],
                             scale ="noscale", threshold =.5){
  # this funciton predicts coulds(1) and no clouds (-1)
  # Args:
  # Traind: must have the columns NDAI, CORR, DF
  # Testd: should be of the same form as Traind, but of course can have different
  # amount of rows
  # scale: variable which allows us to choose, whether we want to scale image by
  # image or the entire dataset at once.
  # threshold: The likelihood at which we will say cloud. >threshod => Cloud
  # Outp:
  # The predicted values for Testd
  
  Probabilities <- LogitAllprobabilities(Traind, Testd, scale ="byimage")
  # the Probabilites contains the likelihood for beeing 1. Thus in the other case
  # it should be 0
  return(as.numeric(Probabilities > threshold)*2 -1)
}
  
if(table(Logit3prediction(Traind = TrD, Testd = TeD, 
                       scale ="byimage", threshold =.5))[1] != 4654){
  stop("Logit3prediction is not working correct!")
}
if(table(LogitAllprediction(Traind = TrD, Testd = TeD, 
                          scale ="byimage", threshold =.5))[1] != 4774){
  stop("LogitAllprediction is not working correct!")
}


rm(TeD, TrD)

