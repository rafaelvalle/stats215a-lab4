#####
# This R script reads in the image.txt files names the columns, creates a logSD
# column, rbinds the dataframe to one big dataframe and saves the data as a 
# data frame tble
# The file is the saved in image_data as rawD.RData. All our functions require 
# this clean and universal format.
######

print("Running script to save image data to a faster format")
library(dplyr)
source("helper_functions.R")

image1 <- read.table('image_data/image1.txt', header=F)
image2 <- read.table('image_data/image2.txt', header=F)
image3 <- read.table('image_data/image3.txt', header=F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

image1$Image ="I1"
image2$Image ="I2"
image3$Image ="I3"

rawD <- tbl_df(rbind(image1, image2, image3))

rawD <- mutate(rawD, logSD = log(SD)) %>%
  select(Image, y:SD, logSD, CORR:AN)


# I add the PCA but I am removing the 0, because I want to have the most  
# describing direction!
# I follow two approaches: The first sclaes by image and the other one by the 
# entiere populaiton
#scaled by image
rawD_by_image <- scalebyimage(rawD,
                           cols = c("NDAI", "SD", "logSD", "CORR", 
                                    "DF", "CF", "BF", "AF", "AN"))
rawD_by_all <- scalecomplete(rawD,
                           cols = c("NDAI", "SD", "logSD", "CORR", 
                                    "DF", "CF", "BF", "AF", "AN"))

PCA1 <- prcomp(rawD_by_image[rawD$label != "0" ,9:13], center=FALSE, scale.=FALSE)
#scaled by complete dataset
PCA2 <- prcomp(rawD_by_all[rawD$label != "0" ,9:13], center=FALSE, scale.=FALSE)

rotate <- function(X, rotation){ 
  # rotate rotates the matrix with respect to the rotation matrix
  as.matrix(X) %*% as.matrix(rotation)
}

PCs1 <- data.frame(rotate(rawD_by_image[ ,9:13], PCA1$rotation))
PCs2 <- data.frame(rotate(rawD_by_all[ ,9:13], PCA2$rotation))

colnames(PCs1) <- paste0(colnames(PCs1),"imagscld")
colnames(PCs2) <- paste0(colnames(PCs2),"compscld")
### (!) This is not a good idea. it is better to scale the data picture by picture
### also for the PCA!
rawD <- tbl_df(cbind(rawD, PCs1, PCs2))
save(rawD, file= 'image_data/rawD.RData')


