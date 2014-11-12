library(ggplot2)
library(rgl)
library(YaleToolkit)
library(dplyr)
library(reshape2)

source("logisticRegression.R")
load("image_data/rawD.RData")


############
# cross validation function
############
CrossValidation <- function(FUN = Logit3prediction, k = 5, Fulldata = rawD){
  # This is a general cross validation function to test different prediction
  # methods. The dataset Fulldata is divided into k equal sized subsets (randomly)
  # then it is trained on 4 and it is tested how well it predicts on the 5th.
  # This will be done 5 times (for each of the 5 subsets)
  # Args:
  # FUN function which must accept Trainingsdata, Testdata in this order(first 
  # Trainingsdata second testdata)[returns 0,1]
  # k the number of subsets the entire data set will be devided
  # Fulldata Dataset which should be used
  # Ouput:
  # Crossvalidation output on the right set. k outputs: For each crossvalidation
  # loop one output.
  N <- nrow(Fulldata)
  sub <- list()
  size <- trunc(N/k)
  R <- 1:N
  for(i in 1:k){
    sub[[i]] <- sample(R, size)
    R <- R[! R%in% sub[[i]]]
  }
  # sub contains now the subsamples
  
  res <- numeric()
  for(i in 1:k){   # exclude sub i, fit the model, predict i, save result in res
    Traind <- Fulldata[-sub[[i]], ]
    Testd  <- Fulldata[ sub[[i]], ]
    Testd <- Testd[Testd$label != 0, ]
    prediction <- FUN(Traind, Testd)
    res[i] <- sum(prediction == Testd$label)/ nrow(Testd)
  }
  return(res)
}
set.seed(10)
if(round(CrossValidation(FUN = Logit3prediction, k = 5, 
               Fulldata = rawD[sample(1:nrow(rawD), 10000), ])[1],5) != 0.88825){
 stop("CrossValidation is not working correct! Or your computer is using another
      randomization algorithm!")
}

######
# Special Cross-Validation # Leave one picture out and fit on the others and then predict
# this is more robust to the problem that each picture is different!
# This is maybe the real and more important test
######
CrossValiPictures <- function(FUN = Logit3prediction, Fulldata = rawD){
  # This is cross validation function to test different prediction
  # methods. The predictor is trained on two pictures and should predict the
  # outcome of the third. This is useful, as we believe that each picture is 
  # different and we should do something clever to "norm" it!
  # Args:
  # FUN function which must accept Trainingsdata, Testdata in this order(first 
  # Ouput:
  # An average number how well the function is performing based on Crossvalidation
  res <- numeric()
  for(i in 1:3){   # exclude sub i, fit the model, predict i, save result in res
    Traind <- Fulldata[Fulldata$Image != paste0("I",i), ]
    Testd  <- Fulldata[Fulldata$Image == paste0("I",i), ]
    Testd <- filter(Testd, label !=0)
    prediction <- FUN(Traind, Testd)
    res[i] <- sum(prediction == Testd$label)/ nrow(Testd)
  }
  return(data.frame(predictedImage = 1:3, Results = res))
}
set.seed(2)
if(round(CrossValiPictures(FUN = Logit3prediction, 
 Fulldata = rawD[sample(1:nrow(rawD), 10000), ])[2,2], 5) != 0.9261){
 stop("CrossValiPictures is not working correct or you computer is using another
      random numbers generator!")
}
