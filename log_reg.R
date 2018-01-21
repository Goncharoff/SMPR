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

## Логарифмическая функция потерь
lossLog<-function(x)
  {
    return (log2(1 + exp(-x)))
  }
## Сигмоидная функция
sigmoidFunction <-function(z)
  {
    return (1 / (1 + exp(-z)))
  }
## Стохастический градиент для логистической регрессии
sg.LogRegression <-function(xl)
  {
    l <-dim(xl)[1]
    n <-dim(xl)[2]-1  
    result_log <- list()
    w <-c(1 / 2, 1 / 2, 1 / 2)
    iterCount <-0
    lambda <- 1/l
    ## initialize Q
    Q <- 0
    for (i in 1:l)
    {
      ## calculate the scalar product <w,x>
        wx <-sum(w * xl[i, 1:n])   
        ## calculate a margin 
        margin <- wx * xl[i, n + 1] 
        Q <- Q + lossLog(margin)   
    }
    repeat
    {
      # select the random index from the error objects errorIndexes
      i <- sample(1:l, 1)
      iterCount <- iterCount + 1 
       #i <-sample(1:l, 1)
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      ## calculate the scalar product <w,xi>
      wx  <- sum(w * xi)
      ## make a gradient step
      margin <- wx * yi
      ex <-lossLog(margin) 
      eta <-0.3 #1 / iterCount 
      w <-w + eta * xi * yi * sigmoidFunction(-wx * yi)
      result_log <-c(result_log, list(w))
      ## Calculate a new Q
      Qprev <-Q
      Q <-(1-lambda) * Q + lambda * ex
      if (abs(Qprev-Q) / abs(max(Qprev, Q)) < 1e-5) 
        break
    }
    return (result_log)
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

## Логистическая регрессия
w_log <-sg.LogRegression(xlNorm)

a_log = seq(1,length(w_log))*0
b_log = seq(1, length(w_log))*0

for(i in 1:length(w_log)){
  a_log[i] = w_log[[i]][3] / w_log[[i]][2] 
  b_log[i] = -w_log[[i]][1] / w_log[[i]][2]
}
steps_a_log <- c(a_log[2], a_log[5], a_log[10], a_log[length(a_log)])
steps_b_log <- c(b_log[2], b_log[5], b_log[10], b_log[length(b_log)])
steps_log <- c(2,5,10,length(a_log))
par(mfrow=c(2,2))
for (i in 1:4){
  plot(xlNorm[, 1], xlNorm[, 2], pch = ifelse(xlNorm[,4] > 0, 2, 1), asp = 1, xlab = "Nornolized Sepal.Length",
       ylab = "Normolized Sepal.Width", main = paste("step", steps_log[i]), sub=paste(a_log[i], b_log[i], collapse=' '))
  abline(steps_a_log[i], steps_b_log[i], col="blue")
}

#abline(steps_a[4], steps_b[4], col="blue")
#abline(-w[3] / w[2], -w[1] / w[2])
