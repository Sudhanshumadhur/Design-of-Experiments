

library(randomForest)
library(DoE.base)
library(FrF2)
library(rsm)

df <- read.csv("adult.csv", header = F)

nodesizeCenter <- floor(0.005*nrow(df))

nodesizeDistance <- nodesizeCenter - 1

design <- bbd(3, n0 = 3)
print(design)

ntree <- 250
results5 <- c()

for (i in 1:nrow(design)){
  print(as.data.frame(design[i,c('x1','x2','x3')]))
  
  
  if (design$x1[i] == -1)   {nodesize <- 3}
  if (design$x1[i] ==  0)   {nodesize <- 6}
  if (design$x1[i] ==  1)   {nodesize <- 9}
  
  if (design$x2[i] == -1)   {classwt <- c(8,1)}
  if (design$x2[i] ==  0)   {classwt <- c(10,1)}
  if (design$x2[i] ==  1)   {classwt <- c(12,1)}
  
  if (design$x3[i] == -1)    {cutoff <- c(0.75, 0.25)}
  if (design$x3[i] ==  0)    {cutoff <- c(0.80, 0.20)}
  if (design$x3[i] ==  1)    {cutoff <- c(0.85, 0.15)}
  
  #cross-validation
  createIndex <- rep(1:10, length.out = nrow(df))
  randomIndex <- sample(createIndex)
  
  innerResults <- c()
  
  TP   <-  0
  FN   <-  0
  FP   <-  0
  TN   <-  0
  
  for (j in 1:10){
    
    print(paste("fold",j,"of run", i))
    train <- df[-which(randomIndex == j),]
    test <-  df[ which(randomIndex == j),]
    

    RFmodel <- randomForest(x = train[,1:ncol(train)-1], y = train[,ncol(train)],
                            xtest = test[,1:ncol(test)-1], ytest = test[,ncol(test)],
                            nodesize = nodesize,
                            classwt = classwt,
                            cutoff = cutoff,
                            keep.forest = T)
    
    tree <-  as.data.frame(getTree(RFmodel), 3, labelVar=TRUE)
    
    print(max(tree$`left daughter`))
    print(max(tree$`right daughter`))
    
    confu <- as.data.frame(RFmodel$test$confusion)
    
    TP   <-  TP + confu[' >50K',' >50K'] 
    FN   <-  FN + confu[' >50K',' <=50K']
    FP   <-  FP + confu[' <=50K',' >50K']
    TN   <-  TN + confu[' <=50K',' <=50K']
    
  }
  TPR <- TP/(TP+FN)
  TNR <- TN/(TN+FP)
  BACC <- (TPR + TNR)/2
  print(BACC)
  results5 <- c(results5, BACC)
}

design$response <- results5

write.csv(design, "RSM7_BBD1.csv")
save(design, file = "RSM7_BBD1.RData")
design <- read.csv("RSM7_BBD1.csv")
load('RSM7_BBD1.RData')

modelFO <- rsm(response ~ FO(x1, x2, x3), data = design)
print(summary(modelFO))

modelTWI <- update(modelFO, .~. + TWI(x1, x2, x3))
print(summary(modelTWI))

modelPQ <- update(modelFO, .~. +PQ(x1, x2, x3))
print(summary(modelPQ))

modelSO <- rsm(response ~ SO(x1, x2, x3), data = design)
print(summary(modelSO))

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}

PRESS(modelSO)


modelReduced <- rsm(response ~ FO(x1,x2,x3) + TWI(x1,x2,x3) + PQ(x2), data = design)
print(summary(modelReduced))
PRESS(modelReduced)


par(mfrow = c(1,3))
contour(modelReduced, ~ FO(x1,x2,x3) + TWI(x1,x2,x3) + PQ(x2), image = T, 
        at = summary(modelReduced)$canonical$xs)

par(mfrow = c(1,3))
contour(modelReduced, ~ FO(x1,x2,x3) + TWI(x1,x2,x3) + PQ(x2), image = T, 
        at = list(x1=0, x2=0, x3=0))

steepest(modelReduced, dist = seq(0, 2, by = 0.5)  )

canonical.path(modelReduced, dist = seq(-2, 2, by = 0.5))

par(mfrow=c(1,2))
varfcn(design, ~ SO(x1,x2,x3), dist = seq(0,3, by = 0.1) )
varfcn(design, ~ SO(x1,x2,x3), dist = seq(0,3, by = 0.1), contour = T )

