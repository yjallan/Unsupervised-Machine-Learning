# 03 wine-nnet

library(caret)
library(plyr)
library(dplyr)
library(reshape2)
library(GGally)
library(ggbiplot)
library(gtable)
library(mclust)
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

#' PCA
wq_pca <- prcomp(wq_nl, retx=TRUE, scale.=TRUE, center=TRUE)
wq_data <- data.frame(PC=1:length(wq_pca$sdev), Variance=wq_pca$sdev^2)
wq_propvar <- (wq_pca$sdev^2/sum(wq_pca$sdev^2)) %>% cumsum
wq_propvar <- data.frame(x=1:length(wq_propvar), cumvar=wq_propvar)
wqpca <- cbind(as.data.frame(wq_pca$x), quality=wq$quality)

#' ICA
wq_ica = fastICA::fastICA(wq_nl, n.comp=11, verbose=TRUE, row.norm=TRUE)
wqica=cbind(as.data.frame(wq_ica$S), quality=wq$quality)
wqn <- sapply(wqica[,!(names(wqica) %in% c("quality"))], function(x) (e1071::kurtosis(x))^2)
wqn <- melt(wqn)
wqn$name <- row.names(wqn)
wqn <- wqn[wqn$value < 1,]
# remove all...in wqn
wqica <- wqica[,!(names(wqica) %in% wqn$name)]

#' random projections
wq_rca <- Map(function(x) {
  gaussian_random_projection(wq_nl, 8)
}, 1:100)

# get the ones which immitate the result best.
# wqrcadiff <- Map(function(x) {
#   sum((PID_nl - (x$RP %*% MASS::ginv(x$R)))^2)
# }, PID_rca) %>% melt

# get the ones which immitate the result best.
wqrcadiff <- Map(function(x) {
  sum((wq_nl - (x$RP %*% MASS::ginv(x$R)))^2)
}, wq_rca) %>% melt

bestrca <- wqrcadiff %>% arrange(value) %>% head(1)
names(bestrca) <- c("value", "k")
wqrca <- cbind(as.data.frame(wq_rca[[bestrca$k]]$RP), quality=wq$quality)

# apply randomforest to get the mean gini, variable importance.
wq_rf <- randomForest(quality ~., wq)
wqrf <- as.data.frame(varImp(wq_rf))
wqrf$names <- row.names(wqrf)
wqrf <- wqrf %>% arrange(desc(Overall))
wqrf <- wqrf[,c("names", "Overall")]
wqrf.name <- wqrf$names[1:(length(wqrf$names)-2)]
wqrfdf <- wq[,c(wqrf.name, "quality")]

###CHANGE HERE TO SLECT COMPONENETS FOR EACH DIMENSIONALITY REDUCTION ALGO
#wq_final <- wqpca
#wq_final <- wqica
#wq_final <- wqrca
wq_final <- wqrfdf

wq_final <- na.omit(wq_final)

# run caret::train on it with nnet
folds <- createFolds(wq_final$quality, k = 6, list = TRUE, returnTrain = FALSE)
train_ind <- c(folds$Fold1, folds$Fold2,folds$Fold3, folds$Fold4)
valid_ind <- c(folds$Fold5)
test_ind <- c(folds$Fold6)

pnetGrid <- expand.grid(size=c(1,3,5,7,9,11,13,15,17,19), 
                        decay=c(0.1))
# wq_model_train <- caret::train(quality~., wq_final[train_ind,], method="nnet", 
#                                tuneGrid=pnetGrid,
#                                trControl=trainControl(savePredictions=TRUE))

#### THIS IS WITH THE CLUSTERING AS FEATURES

train_models <- apply(pnetGrid, 1, function(x) {
  #return(nnet(quality~., wq_final[train_ind,], size=x[1], decay=x[2]))
  return(caret::train(quality~., data=wq_final[train_ind,], method="nnet",
                      tuneGrid = data.frame(size=x[1], decay=x[2]),
                      trControl=caret::trainControl(method="cv")))
})

train_perf <- Map(function(x) {
  pred <- predict(x, wq_final[train_ind,])
  return(caret::confusionMatrix(pred, wq_final[train_ind, c("quality")])$overall['Accuracy'])
}, train_models) %>% unlist

valid_perf <- Map(function(x) {
  pred <- predict(x, wq_final[valid_ind,])
  return(caret::confusionMatrix(pred, wq_final[valid_ind, c("quality")])$overall['Accuracy'])
}, train_models) %>% unlist

test_perf <- Map(function(x) {
  pred <- predict(x, wq_final[test_ind,])
  return(caret::confusionMatrix(pred, wq_final[test_ind, c("quality")])$overall['Accuracy'])
}, train_models) %>% unlist

#performance...
# somehow plot that and you're done, compare with no additional clusters.
pnnet_perf <- cbind(as.data.frame(pnetGrid), 
                    train_perf=train_perf, 
                    valid_perf=valid_perf, 
                    test_perf=test_perf)


#' select the best value based on validation for the one with additional features and plot
#' that slice keeping decay constant.

#decay = 0.10...
pnnet_cons <- select(pnnet_perf[pnnet_perf$decay ==0.1,], -decay)
pnnet_melt <- reshape2::melt(pnnet_cons, "size")

xlim_points <- c(floor(min(pnnet_melt$value)*100)/100, ceiling(max(pnnet_melt$value)*100)/100)

g1 <- ggplot(pnnet_melt[pnnet_melt$variable %in% c("train_perf", "valid_perf", "test_perf"),], 
             aes(x=size, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  #ggtitle("Neural Net - PCA Components as Features") +
  #ggtitle("Neural Net - ICA Components as Features") +
  #ggtitle("Neural Net - RP Components as Features") +
  ggtitle("Neural Net - RF as Features Selection") +
  ylab("Accuracy") + 
  xlab("Number of hidden layers - decay constant at 0.1") +
  scale_y_continuous(limits = xlim_points) +
  scale_colour_discrete(name="",
                        breaks=c("train_perf", "valid_perf", "test_perf"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

# calculate the avverage training time for `train_models` and `train_models_v`
t_times <- Map(function(x) {x$times$everything[['elapsed']]}, train_models) %>% unlist %>% sum















