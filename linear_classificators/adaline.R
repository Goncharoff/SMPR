trainingSampleNormalization <-function(xl){
    n <- dim(xl)[2] -1
    for(i in 1:n) 
    {
      xl[, i] <-(xl[, i] - mean(xl[, i])) / sd(xl[, i]) 
    }
    return (xl)
  }
trainingSamplePrepare <-function(xl){
    l <- dim(xl)[1]
    n <- dim(xl)[2]-1
    xl <- cbind(xl[, 1:n], seq(from = -1, to =-1, length.out = 1), xl[, n + 1])
  }

lossQuad <-function(x){
    return ((x-1) ^ 2)
  }

sg.ADALINE <-function(xl, eta = 1, lambda = 1 / 6){
    l <-dim(xl)[1]
    n <-dim(xl)[2] - 1
    w <-c(1 / 2, 1 / 2, 1 / 2)
    result <- list()
    iterCount <- 0
    ## initialize Q
    Q <- 0
    for (i in 1:l)
    {
      ## calculate the scalar product <w,x>
      wx <-sum(w * xl[i, 1:n])
      ## calculate a margin
      margin <-wx * xl[i, n + 1]
      Q <-Q + lossQuad(margin)
    }
    repeat {
      ## calculate the margins for all objects of the  training sample
      margins <-array(dim = l)
      for (i in 1:l)
      {
        xi <-xl[i, 1:n]
        yi <-xl[i, n + 1]
        
        margins[i] <-crossprod(w, xi) * yi
      }
      ## select the error objects
      errorIndexes <-which(margins <= 0)
      if (length(errorIndexes) > 0)
      {
        # select the random index from the error
        i <-sample(errorIndexes, 1)
        iterCount <-iterCount + 1
        xi <-xl[i, 1:n]
        yi <-xl[i, n + 1]
        ## calculate the scalar product <w,xi>
        wx <-sum(w * xi)
        ## make a gradient step
        margin <-wx * yi
        ## calculate an error
        ex <-lossQuad(margin)
        eta < -1 / sqrt(sum(xi * xi))
        w <-w-eta * (wx-yi) * xi
        result <-c(result, list(w))
        ## Calculate a new Q
        Qprev <-Q
        Q <-(1-lambda) * Q + lambda * ex
      }
      else
      {
        break
      }
    }
    return (result)
}



## Отбираем только интересующие нас классы и переводим их в матрицу dm
dd <- iris[iris$Species != "versicolor", -(3:4)]
dm <- data.matrix(dd[, 1:2])

dm <- cbind(dm, 1)

dc <- rep(1, nrow(dm))
## и исправляем те, что в классе -1
dc[dd$Species == "virginica"] <- -1

## меняем объекты класса sepal на -1
for(i in 1:50){
  dm[i,3] = -1
}
## Нормализация данных
xlNorm <-trainingSampleNormalization(dm)
xlNorm <-trainingSamplePrepare(xlNorm)

## ADALINE

w <-sg.ADALINE(xlNorm) 
a = seq(1,length(w))*0
b = seq(1, length(w))*0
for(i in 1:length(w)){
  a[i] = w[[i]][3] / w[[i]][2] 
  b[i] = -w[[i]][1] / w[[i]][2]
}

steps_a <- c(a[2], a[5], a[10], a[length(a)])
steps_b <- c(b[2], b[5], b[10], b[length(b)])
steps <- c(2,5,10,length(a))
par(mfrow=c(2,2))
for (i in 1:4){
  plot(xlNorm[, 1], xlNorm[, 2], pch = ifelse(xlNorm[,4] > 0, 2, 1), asp = 1, xlab = "Nornolized Sepal.Length",
       ylab = "Normolized Sepal.Width", main = paste("step", steps[i]), sub=paste(a[i], b[i], collapse=' '))
  abline(steps_a[i], steps_b[i], col="blue")
}


