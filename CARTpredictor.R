# Classification Tree with rpart
library(rpart)
library(rpart.plot)

CARTprob <- function(Traind = rawD, Testd = rawD[300: 1000,]){
  Traind1 <- filter(Traind, label != 0) %>% mutate(label = (label +1)/2) %>%
    select(-Image)
  Model <- rpart(label ~ . - y - x - label, method="class", data = Traind1)

  Testd1 <- select(Testd, - Image)
  return(predict(Model, Testd1)[,2])
}


CARTpred <- function(Traind = rawD, Testd = rawD[300: 1000,], k = .5){
  as.numeric(CARTprob(Traind, Testd) > k)*2 - 1
}

CART1prob <- function(Traind = rawD, Testd = rawD[300: 1000,]){
  Traind1 <- filter(Traind, label != 0) %>% mutate(label = (label +1)/2)%>%
    select(-Image)
  Model <- rpart(label ~ NDAI , method="class", data = Traind1)
  
  Testd <- select(Testd, - Image)
  return(predict(Model, Testd)[,2])
}


CART1pred <- function(Traind = rawD, Testd = rawD[300: 1000,], k = .5){
  as.numeric(CART1prob(Traind, Testd) > k)*2 - 1
}



CART2prob <- function(Traind = rawD, Testd = rawD[300: 1000,]){
  Traind1 <- filter(Traind, label != 0) %>% mutate(label = (label +1)/2)%>%
    select(-Image)
  Model <- rpart(label ~ NDAI + AN , method="class", data = Traind1)
  
  Testd <- select(Testd, - Image)
  return(predict(Model, Testd)[,2])
}


CART2pred <- function(Traind = rawD, Testd = rawD[300: 1000,], k = .5){
  as.numeric(CART1prob(Traind, Testd) > k)*2 - 1
}

