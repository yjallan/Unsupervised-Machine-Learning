#' pima dataset analysis
library(mlbench)
library(caret)
library(plyr)
library(dplyr)
library(reshape2)
library(GGally)
library(ggbiplot)
library(gtable)
library(randomForest)
library(gridExtra)
library(randIndex)

source("R/random_projection_gauss.R")

data(PimaIndiansDiabetes)
PID <- na.omit(PimaIndiansDiabetes)
PID_nl <- dplyr::select(PID, -diabetes)
PID_pp <- preProcess(PID_nl) %>% predict(PID_nl) # preprocessed dataframe, centered and scaled


algo <- "Forgy"

wss<-c()
times<-c()
for (i in 2:15) {
  print(i)
  wss[i] <- 0
  times[i] <- 0
  for (j in 1:10) {
    ptm <- proc.time()
    wss[i] <- wss[i] + sum(kmeans(PID_nl, i,iter.max = 200, nstart = 10, algorithm = algo)$withinss)
    #print(wss[i])
    predictTime<-(proc.time() - ptm)[1]
    times[i] <- predictTime + times[i]
  }
  wss[i] = wss[i] / 10
  times[i] = times[i] / 10
}

plot(wss, main="Forgy Method Cluster Evaluation", xlab="Number of Clusters", ylab="Within groups sum of squared")
plot(times, main="pima Forgy Method Cluster Timing", xlab="Number of Clusters", ylab="Time")

algo <- "Lloyd"

wss<-c()
for (i in 2:15) {
  wss[i] <- 0
  for (j in 1:10) {
    wss[i] <- wss[i] + sum(kmeans(PID_nl, i,iter.max = 200, nstart = 10, algorithm = algo)$withinss)
    #print(wss[i])
  }
  wss[i] = wss[i] / 10
}


plot(wss, main="Lloyd Method Cluster Evaluation", xlab="Number of Clusters", ylab="Within groups sum of squared")


#According to the Elbow method, kink is seen at 5

algo <- "Forgy"

ptm <- proc.time()
pimaCluster <- kmeans(PID_nl, 5, iter.max = 200,nstart = 20, algorith = algo)
predictTime<-(proc.time() - ptm)[1]
print(predictTime)

#The plot:

library(fpc)
#plotcluster(reds, pimaCluster$cluster)
plotcluster(PID_nl, pimaCluster$cluster)

library(MASS)
parcoord(PID_nl, pimaCluster$cluster)

library(flexclust)
crosstab <- table(PID$diabetes, pimaCluster$cluster)
#crosstab <- table( pimaCluster$cluster,pima$diabetes)
agreement <- randIndex(crosstab)
print(agreement)

########################################

library(mclust)

pimaBIC<-mclustBIC(PID_nl)
plot(pimaBIC)
print(pimaBIC)
summary(pimaBIC)

pimaBIC<- pimaBIC[,] %>% as.data.frame %>% add_rownames  %>% melt("rowname") %>% na.omit

ggplot(pimaBIC, aes(x=rowname, y=value, colour=variable, group=variable)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle("PIMA Expectation Maximization")

#Plot the times
times<-c()
for (i in 2:11) {
  #for (i in 2:2) {
  print(i)
  ptm <- proc.time()
  print("before run")
  empimaCluster<-Mclust(PID_nl, i)
  times[i] <-(proc.time() - ptm)[1]
  print(times[i])
}

plot(times, main="Pima EM Cluster Timing", xlab="Number of Clusters", ylab="Time")

ptm <- proc.time()
#empimaCluster<-Mclust(pima, 7) #old

#empimaCluster<-Mclust(pima, x=pimaBIC) #these both are same
empimaCluster<-Mclust(PID_nl, 5)

summary(empimaCluster)
summary(empimaCluster,parameters = TRUE)

predictTime<-(proc.time() - ptm)[1]
print(predictTime)

empimaCluster$classification
crosstab <- table(PID$diabetes, empimaCluster$classification)
crosstab

agreement <- randIndex(crosstab)
print(agreement)

plot(empimaCluster,what = "classification")
#plot(empimaCluster,what = "uncertainty")
#plot(empimaCluster,what = "density")

plotcluster(PID_nl, empimaCluster$classification)
parcoord(PID_nl, empimaCluster$classification)

