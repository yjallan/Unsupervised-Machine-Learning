#' wine dataset analysis
library(caret)
library(plyr)
library(dplyr)
library(reshape2)
library(GGally)
library(ggbiplot)
library(gtable)
library(randomForest)
library(gridExtra)

source("R/random_projection_gauss.R")
wq <- read.table(file.path(getwd(), "data", "winequality", "winequality-red.csv"), 
                     sep=";",
                     header=TRUE)
# discard obs less than 5 and greater than 7, since there isn't enough information
wq <- na.omit(wq)
wq <- wq[wq$quality %in% c(5,6,7),]
wq$quality <- factor(wq$quality, ordered=TRUE)
wq_nl <- dplyr::select(wq, -quality)
wq_pp <- preProcess(wq_nl) %>% predict(wq_nl)

algo <- "Forgy"

wss<-c()
times<-c()
for (i in 2:15) {
  print(i)
  wss[i] <- 0
  times[i] <- 0
  for (j in 1:10) {
    ptm <- proc.time()
    wss[i] <- wss[i] + sum(kmeans(wq_nl, i,iter.max = 200, nstart = 10, algorithm = algo)$withinss)
    #print(wss[i])
    predictTime<-(proc.time() - ptm)[1]
    times[i] <- predictTime + times[i]
  }
  wss[i] = wss[i] / 10
  times[i] = times[i] / 10
}

plot(wss, main="Forgy Method Cluster Evaluation", xlab="Number of Clusters", ylab="Within groups sum of squared")
plot(times, main="Wine Forgy Method Cluster Timing", xlab="Number of Clusters", ylab="Time")

algo <- "Lloyd"

wss<-c()
for (i in 2:15) {
  wss[i] <- 0
  for (j in 1:10) {
    wss[i] <- wss[i] + sum(kmeans(wq_nl, i,iter.max = 200, nstart = 10, algorithm = algo)$withinss)
    #print(wss[i])
  }
  wss[i] = wss[i] / 10
}


plot(wss, main="Lloyd Method Cluster Evaluation", xlab="Number of Clusters", ylab="Within groups sum of squared")


#According to the Elbow method, kink is seen at 5

algo <- "Forgy"

ptm <- proc.time()
wineCluster <- kmeans(wq_nl, 5, nstart = 20, algorith = algo)
predictTime<-(proc.time() - ptm)[1]
print(predictTime)

#The plot:

library(fpc)
#plotcluster(reds, wineCluster$cluster)
plotcluster(wq_nl, wineCluster$cluster)

library(MASS)
parcoord(wq_nl, wineCluster$cluster)

library(flexclust)
crosstab <- table(wq_nl$quality, wineCluster$cluster)
#crosstab <- table( wineCluster$cluster,wine$quality)
agreement <- randIndex(crosstab)
print(agreement)

########################################

library(mclust)

wineBIC<-mclustBIC(wq_nl)
plot(wineBIC)
print(wineBIC)
summary(wineBIC)

#Plot the times
times<-c()
for (i in 2:11) {
  #for (i in 2:2) {
  print(i)
  ptm <- proc.time()
  print("before run")
  emWineCluster<-Mclust(wq_nl, i)
  times[i] <-(proc.time() - ptm)[1]
  print(times[i])
}

plot(times, main="Wine EM Cluster Timing", xlab="Number of Clusters", ylab="Time")

ptm <- proc.time()
#emWineCluster<-Mclust(wine, 7) #old

#emWineCluster<-Mclust(wine, x=wineBIC) #these both are same
emWineCluster<-Mclust(wq_nl, 9)

summary(emWineCluster)
summary(emWineCluster,parameters = TRUE)

predictTime<-(proc.time() - ptm)[1]
print(predictTime)

emWineCluster$classification
crosstab <- table(wq$quality, emWineCluster$classification)
crosstab

agreement <- randIndex(crosstab)
print(agreement)

plot(emWineCluster,what = "classification")
#plot(emWineCluster,what = "uncertainty")
#plot(emWineCluster,what = "density")

plotcluster(wq_nl, emWineCluster$classification)
parcoord(wq_nl, emWineCluster$classification)

