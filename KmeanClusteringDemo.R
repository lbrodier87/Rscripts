#INSTRUCTIONS: 
#    - Start by loading all functions in the 'FUNCTION' section below
#    - Execute the code in the 'MAIN SCRIPT' section line by line
#      This section contains a loop to demonstrate clustering step by step.
#      After starting the loop, follow instructions in R console.
#    - The code under the section 'additional tests' is facultative and demonstrate other methods

rm(list = ls()) #if you want to reset your workspace, execute this line

#######################
###### FUNCTIONS ######
#######################
#assign groups to a df and return the df
assigngroup <- function(df, centroid){
  df$d1 <- apply(X = df[,1:2], MARGIN = 1, FUN = function(x){d <- calcDist(x["x"], x["y"], centroid$x[1], centroid$y[1])})
  df$d2 <- apply(X = df[,1:2], MARGIN = 1, FUN = function(x){d <- calcDist(x["x"], x["y"], centroid$x[2], centroid$y[2])})
  df$d3 <- apply(X = df[,1:2], MARGIN = 1, FUN = function(x){d <- calcDist(x["x"], x["y"], centroid$x[3], centroid$y[3])})
  df$group <- apply(X = df[, c("d1", "d2", "d3")], MARGIN = 1, FUN = function(x){as.character(paste0("g",which.min(x)))})
  drawMyPlot(df, centroid)
  df
}
#recalc centroids position
recalcCentroid <- function(df, centroid){
  centroid$x[1] <- mean(df[df$group=="g1",1])
  centroid$y[1] <- mean(df[df$group=="g1",2])
  centroid$x[2] <- mean(df[df$group=="g2",1])
  centroid$y[2] <- mean(df[df$group=="g2",2])
  centroid$x[3] <- mean(df[df$group=="g3",1])
  centroid$y[3] <- mean(df[df$group=="g3",2])
  drawMyPlot(df, centroid)
  centroid
}
#calculate dist between two points...
calcDist <- function(x1, y1, x2, y2){
  d <- sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
  d
}
#function to draw plot
drawMyPlot <- function(df, centroid){
  print(ggplot() + 
          geom_point(data = df, mapping = aes(x=x, y=y, color=group), alpha=0.5, size = 2) + 
          geom_point(data = centroid, mapping = aes(x=centroid$x, y=centroid$y, color = centroid$no), shape = 3, size = 5) +
          theme_bw(), newpage = F)
}
#function to initialize or reset the datas
resetData <- function(){
  #Random data generator
  x <- c(rnorm(n = 20, mean = 4, sd = 2), rnorm(n = 20, mean = 10, sd = 3), rnorm(n = 20, mean = 15, sd = 2))
  y <- c(rnorm(n=20, mean = 4, sd = 1.5), rnorm(n=20, mean=12, sd = 2), rnorm(n=20, mean =3, sd = 2))
  cx <- sample(1:20, 3, replace = T)
  cy <- sample(1:12, 3, replace = T)
  
  #re-init dataframe and random centroids + graph
  df <<- data.frame(x=x, y=y, group=factor(rep("g4", 30),levels = c("g1", "g2", "g3")))
  centroid <<- data.frame(x=cx, y=cy, no=factor(c("g1", "g2", "g3"), c("no_group", "g1", "g2", "g3")))
  i <<- 1
 drawMyPlot(df, centroid)
}
resetCentroids <- function(){
  cx <- sample(1:20, 3, replace = T)
  cy <- sample(1:12, 3, replace = T)
  centroid <<- data.frame(x=cx, y=cy, no=factor(c("g1", "g2", "g3"), c("no_group", "g1", "g2", "g3")))
  df <<- data.frame(x=df$x, y=df$y, group=factor(rep("g4", 30),levels = c("g1", "g2", "g3")))
  i <<- 1
  drawMyPlot(df, centroid)
}

####################
### MAIN SCRIPT ####
####################
install.packages("ggplot2") #execute this line only if ggplot2 is not installed
library(ggplot2) #load the ggplot2 library in R
x11() #to plot in a separate window (facultative but faster)

#initialize the global variables
df <- NULL
centroid <- NULL;
i = NULL;
resetData()

#run this infinite loop to analyze step by step, follow instructons in the R console
while(T){
  message(paste0("--> Iteration ", i, " (r: reset all c: reset centroids q: quit)"))
  r <- readline(prompt=paste0("    press enter to assign groups: "))
  if(r=="r"){
    resetData()
    message(paste0("--> Data have been reset..."))
  }else if(r=="c"){
    resetCentroids()
    message(paste0("--> Centroids have been reset..."))
  }else if(r=="q"){
    break
  }else{
    df <- assigngroup(df, centroid)
    r <- readline(prompt=paste0("    press enter to recalc centroids: "))
    if(r=="r"){
      resetData()
      message(paste0("--> Data have been reset..."))
    }else if(r=="c"){
      resetCentroids()
      message(paste0("--> Centroids have been reset..."))
    }else if(r=="q"){
      break
    }else{
      centroid <- recalcCentroid(df, centroid)
      i <- i+1
    }
  }
}



########################
## (additional tests) ##
########################
#this section is only to test other clustering methods, run if you are interested...
resetData() #reset the data

#for hierarchical clustering + plot
distxy <- dist(df[,1:2])
hClustering <- hclust(distxy)
plot(hClustering)

#use the default kmeans function in R and plot
km <- kmeans(df[,1:2], centers = 3)
plot(df$x, df$y, col=km$cluster, xlab = "x", ylab = "y")
points(km$centers, col=1:3, pch =3, cex = 2, lwd = 3)
