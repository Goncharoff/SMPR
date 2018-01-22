## Перцептрон Розенблатта
trainPerceptron <- function(dm, dc) {
  ## в result будем хранить список весов в зависимости от итерации, 
  ## чтобы потом можно было посмотреть, как развивались события
  result <- list()
  oldW <- c()
  w <- rep(0, ncol(dm))
  while (!identical(w, oldW)) {
    oldW <- w
    for (i in 1:nrow(dm)) {
      ## %*% здесь означает скалярное произведение
      pred <- sign(dm[i,] %*% w)
      w = w + (dc[i] - pred) * dm[i, ]
    }
    ## Добавляем w в конец списка
    result <- c(result, list(w))
  }
  result
}

## Визуализация результата обучения pcResult в разные моменты времени steps
plotPerceptronSteps <- function(pcResult, dm, dc, steps) {
  for (s in steps) {
    w <- pcResult[[s]]
    plot(dm[,1:2], main=paste("step", s), 
         pch= ifelse(dc > 0, 1, 2), sub=paste(w, collapse=' '))
    abline(-w[3] / w[2], -w[1] / w[2])
  }
}

data(iris)

## Отбираем только интересующие нас классы и переводим их в матрицу dm
dd <- iris[iris$Species != "versicolor", -(3:4)]
dm <- data.matrix(dd[, 1:2])

## А это чтобы разделяющая плоскость имела вид не
## y = ax, а y = ax + b, где подбираются и a, и b
dm <- cbind(dm, 1)

## dc — правая часть классификатора (метки классов)
## Делаем вектор из единиц
dc <- rep(1, nrow(dm))
## и исправляем те, что в классе -1
dc[dd$Species == "virginica"] <- -1


## Результат обучения:
pc <- trainPerceptron(dm, dc)
plotPerceptronSteps(pc, dm, dc, length(pc))

## Посмотрим разделение в разные мометны времени
par(mfrow=c(2,2))
plotPerceptronSteps(pc, dm, dc, c(10, 75, 230, length(pc)))
