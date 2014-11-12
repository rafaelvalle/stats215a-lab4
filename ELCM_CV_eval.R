source("logisticRegression.R")
source("MethodEvaluation.R")
set.seed(1)

library(segmented)
library(boot)
library(mixtools)
library(ggplot2)
library(graphics)
library(pastecs)
library(kimisc)

######################
# ELCM_fixed
######################
predict_fixed_threshold <- function(Traind = TrD, Testd = TeD, scale ="no_scaling",
                                    variable = "logSD", threshold.ini = -3,
                                    step = 0.1, imax = 90){
  # Find the threshold with the minimum error for the training data.
  # Error is the number of mislabelled pixels over the number of pixels for 
  # the testing data.
  # Input
  #   Traind - training set
  #   Testd - testing set
  #   scale - scaling method used, "no_scaling", "byimage" or "complete"
  #   variable - variable used. Ex: logSD
  #   threshold.ini - the initial value of the threshold
  #   step - the value added to the threshold at each step
  #   imax - the number of iteration
  # Output
  #   value of the threshold
  if(scale == "byimage") {
    Traind <- scalebyimage(Traind)
    Testd <- scalebyimage(Testd)
  }
  if(scale == "complete") {
    Traind <- scalecomplete(Traind)
    Testd <- scalecomplete(Testd)
  }
  #remove 0
  Traind <- filter(Traind, label !=0)
  # initialization
  error <- numeric()
  thr <- numeric()
  threshold <- threshold.ini
  # grid search to find the threshold with the lowest error
  # cloud free is labelled 0
  # cloud is labelled 1
  for (i in 1:imax){
    Traind[Traind[ ,variable] < threshold, "estimate"] <- -1
    Traind[Traind[ ,variable] >= threshold, "estimate"] <- 1
    TP <- nrow(Traind[Traind$label == 1 & Traind$estimate == 1, ])
    TN <- nrow(Traind[Traind$label == -1 & Traind$estimate == -1, ])
    error <- c(error, nrow(Traind) - 
                 sum(as.numeric(Traind[, "label"] == Traind[, "estimate"])))
    thr <- c(thr, threshold)
    threshold <- threshold + step
  }
  return(which.min(error)*step + threshold.ini)
}


my.threshold <- function(learning.set, variable, threshold.ini, step, imax){
  # Find the value of the variable with the minimum error
  # Error is the number of mislabelled pixels
  # Return also performance evaluation (sensitivity, specificity)
  # Input
  #   learning.set - to guess the threshold for the variable
  #   variable - the variable of the threshold. Ex: "logSD"
  #   threshold.ini - the initial value of the threshold
  #   step - the value added to the threshold at each step
  #   imax - the number of iteration
  #
  # Output
  #   value of the threshold
  #   sensitivity
  #   specificity
  #   thresholds all the steps
  
  my.ls <- as.data.frame(learning.set)
  my.ls <- filter(my.ls, label !=0)
  # initialize the error at zero
  error <- numeric()
  accuracy <- numeric()
  sensitivity <- numeric()
  specificity <- numeric()
  thr <- numeric()
  threshold <- threshold.ini
  
  # loop over threshold
  # cloud free is labelled -1
  # cloud is labelled 1
  for (i in 1:imax){
    my.ls[my.ls[ ,variable] < threshold, "estimate"] <- -1
    my.ls[my.ls[ ,variable] >= threshold, "estimate"] <- 1
    TP <- nrow(my.ls[my.ls$label == 1 & my.ls$estimate == 1, ])
    TN <- nrow(my.ls[my.ls$label == -1 & my.ls$estimate == -1, ])
    error <- c(error, nrow(my.ls) - 
                 sum(as.numeric(my.ls[, "label"] == my.ls[, "estimate"])))
    accuracy <- c(accuracy, 
                  sum(as.numeric(my.ls[, "label"] == my.ls[, "estimate"]))/
                    nrow(my.ls))
    sensitivity <- c(sensitivity,
                     TP / nrow(my.ls[my.ls$label == 1, ]))
    specificity <- c(specificity,
                     TN / nrow(my.ls[my.ls$label == -1, ]))
    thr <- c(thr, threshold)
    threshold <- threshold + step
  }
  return(list(which.min(error)*step + threshold.ini, 
              sensitivity=sensitivity, specificity=specificity,
              thresholds=thr))
}


ELCM_fixed <- function(Trainingsdata, Testdata){
  # function to compute a vector of the estimated labels using function
  # predict_fixed_threshold
  # Input
  #    Trainingsdata - training set
  #    Testdata - testing set
  # Output 
  #    A vector of the estimated labels for the testing set
  threshold1 <- predict_fixed_threshold(Traind = Trainingsdata, Testd = Testdata,
                                       scale = "no_scaling",
                                       variable = "logSD", threshold.ini = -3,
                                       step = 0.1, imax = 90)
  threshold2 <- predict_fixed_threshold(Traind = Trainingsdata, Testd = Testdata,
                                       scale = "no_scaling",
                                       variable = "NDAI", threshold.ini = -3,
                                       step = 0.1, imax = 90)
  Testdata[Testdata[ ,"logSD"] < threshold1 |
             Testdata[ ,"NDAI"] < threshold2, "estimate"] <- -1
  Testdata[Testdata[ ,"logSD"] >= threshold1 &
             Testdata[ ,"NDAI"] >= threshold2, "estimate"] <- 1
  return(Testdata$estimate)
}



######################
# ELCM_EM
######################
predict_EM_threshold <- function(Traind = TrD, Testd = TeD, scale ="no_scaling",
                                 variable = "logSD"){
  # Estimate the threshold using the EM algorithm initialized by kmean for
  # a gaussian mixture.
  # The EM algorithm allows us to retreive the distributions of the 
  # variables restricted to labels 1 and -1.
  # The threshold is the minimum of the distribution between the two means.
  # Input
  #    Traind - training set
  #    Testd - testing set
  #    scale - scaling option, either "no_scaling","byimage" or "complete"
  #    variable - variable to estimate the threshold
  # Output
  #    threshold
  #    
  if(scale == "byimage") {
    Traind <- scalebyimage(Traind)
    Testd <- scalebyimage(Testd)
  }
  if(scale == "complete") {
    Traind <- scalecomplete(Traind)
    Testd <- scalecomplete(Testd)
  }
  TrD <- filter(Traind, label !=0)
  # Kmeans initialization
  my.KMean <- kmeans(TrD[, variable], centers=2)
  # give a cluster to each pixel
  cluster <- my.KMean$cluster
  # initial pi
  Pi.ini <- length(cluster[cluster==1])/length(cluster)
  # initial means
  mu <- as.vector(my.KMean$centers)
  # initial variances
  sigma <- my.KMean$withinss / 
    c(length(cluster[cluster==1]), length(cluster[cluster==2]))
  
  # mixt 
  invisible(capture.output(my.mixt <- normalmixEM2comp(as.matrix(TrD[, variable]), lambda=Pi.ini, mu=mu,
                              sigsqrd=sigma, verb=FALSE)))
  # find minimum
  # try to find a turning point
  #density <- my.mixt$lambda[1]*
  #  dnorm(seq(-3,6, by=0.01), mean=my.mixt$mu[1], sd = my.mixt$sigma[1]) +
  # my.mixt$lambda[2]*
  #  dnorm(seq(-3,6, by=0.01), mean=my.mixt$mu[2], sd = my.mixt$sigma[2])
  #ts.density <- ts(density)
  #tp.density <- turnpoints(ts.density)
  #threshold <- tp.density$tppos[2]*0.01 - 3
  # if no turning point exists, the threshold is the mean of the means of the
  # two normalmixEM.
  threshold <- round((my.mixt$mu[1] + my.mixt$mu[2])/2,2)
  # restraint the threshold to an interval
  # if the threshold found is not in this interval, then
  # the threshold takes to value of the closest boundary
  threshold.logSD.max <- 1.6
  threshold.logSD.min <- 0.6
  threshold.NDAI.max <- 0.9
  threshold.NDAI.min <- 0.2
  if (variable == "NDAI" & threshold > threshold.NDAI.max){
    threshold <- threshold.NDAI.max
  }
  if (variable == "NDAI" & threshold < threshold.NDAI.min){
    threshold <- threshold.NDAI.min
  }
  if (variable == "logSD" & threshold > threshold.logSD.max){
    threshold <- threshold.logSD.max
  }
  if (variable == "logSD" & threshold < threshold.logSD.min){
    threshold <- threshold.logSD.min
  }
  return(threshold)
}

ELCM_EM <- function(Trainingsdata, Testdata){
  # function to compute a vector of the estimated labels using function
  # predict_EM_threshold
  # Input
  #    Trainingsdata - training set
  #    Testdata - testing set
  # Output 
  #    A vector of the estimated labels for the testing set
  threshold1 <- predict_EM_threshold(Traind = Trainingsdata, Testd = Testdata,
                                    scale = "no_scaling", variable = "logSD")
  threshold2 <- predict_EM_threshold(Traind = Trainingsdata, Testd = Testdata,
                                     scale = "no_scaling", variable = "NDAI")
  
  Testdata[Testdata[ ,"logSD"] < threshold1 | 
             Testdata[ ,"NDAI"] <= threshold2, "estimate"] <- -1
  Testdata[Testdata[ ,"logSD"] >= threshold1 &
             Testdata[ ,"NDAI"] >= threshold2, "estimate"] <- 1
  return(Testdata$estimate)
}




