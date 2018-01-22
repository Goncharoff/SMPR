dst1 <- (function(a,b) dist(rbind(a, b))) # euclidian 

sortByDist <- function(set, x, n, m, dist){
  distances <- data.frame(set[,m+1])
  for(i in 1:n)
    distances[i, 2] <- dist(set[i, 1:m], x)
  return(distances[order(distances[, 2]), ])
}

kNNClassifier <- function(trainSet, u, metric = dst1, k){
  rowsNum <- dim(trainSet)[1]; varsNum <- dim(trainSet)[2]-1
  labels = levels(trainSet[rowsNum, varsNum+1])
  orderedDist <- sortByDist(trainSet, u, rowsNum, varsNum, metric)
  kNNeighbors <- orderedDist[1:k,1]
  lst <- orderedDist[k,2]
  countTable <- table(kNNeighbors)
  maxLabelIdx = which.max(countTable)
  return(c(labels[maxLabelIdx], abs(max(countTable)-max(countTable[-maxLabelIdx]))))
}

STOLP <- function(set, threshold, classifier, argsList){
  plot(iris[,3:4], bg=colors2[iris$Species], col=colors2[iris$Species])
  rowsNum <- dim(set)[1]
  varsNum <- dim(set)[2]-1
  toDelete = numeric()
  for(i in 1:rowsNum){
    currentArgs = c(list(set, set[i, 1:varsNum]),argsList)
    res = do.call(classifier,currentArgs)
    if(res[1] != set[i, varsNum+1]){
      toDelete <- c(toDelete, i)
    }
  }
  points(set[toDelete,], pch=21, bg='grey', col='grey') # debug
  set = set[-toDelete, ]; rowsNum = rowsNum - length(toDelete)
  labels = levels(set[rowsNum,varsNum+1])
  maxRes = rep(0, length(labels)); names(maxRes)<-labels
  maxLabel = rep(0, length(labels)); names(maxLabel)<-labels
  for(i in 1:rowsNum){
    currentArgs = c(list(set, set[i, 1:varsNum]),argsList)
    res = do.call(classifier,currentArgs)
    if(res[2] > maxRes[res[1]]){
      maxRes[res[1]] = res[2]
      maxLabel[res[1]] = i
    }
  }
  regular = set[maxLabel, ]
  points(regular, pch=21, bg=colors2[regular$Species], col=colors2[regular$Species])
  repeat{
    errCount = 0L; toAdd = 0L; maxAbsMargin = -1
    for(i in 1:rowsNum){
      currentArgs = c(list(regular, set[i, 1:varsNum]),argsList)
      res = do.call(classifier,currentArgs)
      if(res[1] != set[i, varsNum+1]){
        errCount = errCount + 1
        if(as.double(res[2]) > maxAbsMargin)
          toAdd <- i
        maxAbsMargin <- as.double(res[2])
      }
    }
    if(errCount <= threshold)
      return(regular)
    newRegular = set[toAdd,]
    regular = rbind(regular,newRegular)
    points(newRegular, pch=21, bg=colors2[newRegular$Species], col=colors2[newRegular$Species])
  }
}


colors1 <- c("setosa"="#FF000044", "versicolor"="#00FF0044", "virginica"="#0000FF44")
colors2 <- c("setosa" = "#FF000088", "versicolor" = "#00FF0088", "virginica" = "#0000FF88")

etalons = STOLP(iris[,3:5], 1, kNNClassifier, list(k=6))
LOO <- function(classifier, classifierArgs, dataSet, paramName, paramRange){
  rows <- dim(dataSet)[1]
  cols <- dim(dataSet)[2]-1
  risks <- vector('numeric', length(paramRange))
  minErr <- rows
  bestParam = 0; j=1; paramIdx = 1
  for(param in paramRange){
    err = 0
    for(i in 1:rows){
      currentArgs = c(list(u=dataSet[i, 1:cols]),classifierArgs)
      currentArgs[paramName] = param
      class <- do.call(classifier, currentArgs)[1]
      if(class != dataSet[i,cols+1])
        err <- err+1
    }
    if(err < minErr){
      minErr = err
      bestParam = param
      paramIdx = j
    }
    risks[j] = err/rows
    j = j+1
    message(paste("current ", paramName, " = ", sep=''), param, " out of ", paramRange[length(paramRange)])
  }
  message(paste("best ", paramName, ": ", sep=''), bestParam)
  message("errors made: ", minErr)
  message("empirical risk: ", minErr/rows)
  plot(paramRange, risks, xlab=paramName, ylab='LOO', col='red', type='l')
  points(bestParam, risks[paramIdx], bg='red', col='red', pch=21)
  text(bestParam, risks[paramIdx], paste(paramName,"=",bestParam), pos=3)
  return(bestParam)
}
#LOO(kNNClassifier, list(trainSet=iris[,3:5]), iris[,3:5], "k", 6:6)
LOO(kNNClassifier, list(trainSet=iris[,3:5]), iris[,3:5], "k", 6:6)
