###### 
# This file contains specific functions for the cloud prediction project.
# and a test at the end of the document, which tests, whether the functions
# are working. When you souce this document, these tests will give an error,
# if something is not working with these functions
######
#### The Functions:####
# 1.) scalebyimage: scales the Rawdata iamge by image
# 2.) scalecomplete: scales the Rawdata 
# 3.) delete0: A function which deltes all the 0 outputs
# 4.) PlotCorr: A special function to plot correlation in a more intuitive way
#### Data cleaning funcitons #####

scalebyimage <- function(mdata = rawD, cols = c("NDAI", "SD", "logSD", "CORR",
  "DF", "CF", "BF", "AF", "AN", "PC1imagscld", "PC2imagscld", "PC3imagscld",
  "PC4imagscld", "PC5imagscld", "PC1compscld", "PC2compscld", "PC3compscld",
  "PC4compscld", "PC5compscld")){
  # Columns cols are scaled image by image
  # Args:
  # mdata: the data frame or data table which should be scaled by Image, should
  #         contain a column called Image
  # cols: list of characters containing the names of columns which should be 
  #       scaled
  # Output:
  # The scaled dataframe or tabl_df
  Images <- unique(mdata$Image)
  #copy
  for(im in Images){
    mdata[mdata$Image==im, cols] <- scale(mdata[mdata$Image==im, cols]) 
  }
  return(mdata)
}

scalecomplete <- function(mdata = rawD, cols = c("NDAI", "SD", "logSD", "CORR",
  "DF", "CF", "BF", "AF", "AN")){
  # Columns cols are scaled wihtout looking at the image structure
  # Args:
  # mdata: the data frame or data table which should be scaled
  # cols: list of characters containing the names of columns which should be 
  #       scaled
  # Output:
  # The scaled dataframe or tabl_df
  Images <- unique(mdata$Image)
  #copy
  mdata[, cols] <- scale(mdata[, cols]) 
  return(mdata)
}


#Modified plotcorrelation matrix found at http://hlplab.wordpress.com/2012/03/20/correlation-plot-matrices-using-the-ellipse-library/
PlotCorr = function (corr, outline = FALSE, col = "grey", 
                         upper.panel = c("ellipse", "number", "none"), lower.panel = c("ellipse", "number", "none"), 
                         diag = c("none", "ellipse", "number"), 
                         digits = 2, bty = "n", axes = FALSE, 
                         xlab = "", ylab = "", asp = 1, 
                         cex.lab = par("cex.lab"), cex = 0.75 * par("cex"), 
                         mar = 0.1 + c(2, 2, 4, 2), ...)
{
  # this is a modified version of the plotcorr function from the ellipse package
  # this prints numbers and ellipses on the same plot but upper.panel and lower.panel changes what is displayed
  # diag now specifies what to put in the diagonal (numbers, ellipses, nothing)
  # digits specifies the number of digits after the . to round to
  # unlike the original, this function will always print x_i by x_i correlation rather than being able to drop it
  # modified by Esteban Buz
  if (!require('ellipse', quietly = TRUE, character = TRUE)) {
    stop("Need the ellipse library")
  }
  savepar <- par(pty = "s", mar = mar)
  on.exit(par(savepar))
  if (is.null(corr))
    return(invisible())
  if ((!is.matrix(corr)) || (round(min(corr, na.rm = TRUE), 6) < -1) || (round(max(corr, na.rm = TRUE), 6) > 1))
    stop("Need a correlation matrix")
  plot.new()
  par(new = TRUE)
  rowdim <- dim(corr)[1]
  coldim <- dim(corr)[2]
  rowlabs <- dimnames(corr)[[1]]
  collabs <- dimnames(corr)[[2]]
  if (is.null(rowlabs))
    rowlabs <- 1:rowdim
  if (is.null(collabs))
    collabs <- 1:coldim
  rowlabs <- as.character(rowlabs)
  collabs <- as.character(collabs)
  col <- rep(col, length = length(corr))
  dim(col) <- dim(corr)
  upper.panel <- match.arg(upper.panel)
  lower.panel <- match.arg(lower.panel)
  diag <- match.arg(diag)
  cols <- 1:coldim
  rows <- 1:rowdim
  maxdim <- max(length(rows), length(cols))
  plt <- par("plt")
  xlabwidth <- max(strwidth(rowlabs[rows], units = "figure", cex = cex.lab))/(plt[2] - plt[1])
  xlabwidth <- xlabwidth * maxdim/(1 - xlabwidth)
  ylabwidth <- max(strwidth(collabs[cols], units = "figure", cex = cex.lab))/(plt[4] - plt[3])
  ylabwidth <- ylabwidth * maxdim/(1 - ylabwidth)
  plot(c(-xlabwidth - 0.5, maxdim + 0.5), c(0.5, maxdim + 1 + ylabwidth), type = "n", bty = bty, axes = axes, xlab = "", ylab = "", asp = asp, cex.lab = cex.lab, ...)
  text(rep(0, length(rows)), length(rows):1, labels = rowlabs[rows], adj = 1, cex = cex.lab)
  text(cols, rep(length(rows) + 1, length(cols)), labels = collabs[cols], srt = 90, adj = 0, cex = cex.lab)
  mtext(xlab, 1, 0)
  mtext(ylab, 2, 0)
  mat <- diag(c(1, 1))
  
  PlotCorrInternal = function() {
    if (i == j){ #diag behavior
      if (diag == 'none'){
        return()
      } else if (diag == 'number'){
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else if (diag == 'ellipse') {
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      }
    } else if (i >= j){ #lower half of plot
      if (lower.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (lower.panel == 'number') { #check if ellipses should go here
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else {
        return()
      }
    } else { #upper half of plot
      if (upper.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (upper.panel == 'number') { #check if ellipses should go here
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else {
        return()
      }
    }
  }
  for (i in 1:dim(corr)[1]) {
    for (j in 1:dim(corr)[2]) {
      PlotCorrInternal()
    }
  }
  invisible()
}

my.threshold <- function(learning.set, variable, threshold.ini, step, imax){
  # Find the value of the variable with the minimum error
  # Error is the number of mislabelled pixels
  # Input
  #   learning.set - to guess the threshold for the variable
  #   variable - the variable of the threshold. Ex: "logSD"
  #   threshold.ini - the initial value of the threshold
  #   step - the value added to the threshold at each step
  #   imax - the number of iteration
  #
  # Output
  #   value of the threshold
  
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

