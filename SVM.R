## Отбираем только интересующие нас классы и переводим их в матрицу dm
dd <- iris[iris$Species != "versicolor", -(3:4)]
dm <- data.matrix(dd[, 1:2])

## А это чтобы разделяющая плоскость имела вид не
## y = ax, а y = ax + b, где подбираются и a, и b
##dm <- cbind(dm, 1)

## dc — правая часть классификатора (метки классов)
## Делаем вектор из единиц
dc <- rep(1, nrow(dm))
## и исправляем те, что в классе -1
dc[dd$Species == "virginica"] <- -1


library(kernlab)
## Another example with the famous iris data
svp = ksvm(dm,dc,type="C-svc", kernel = "splinedot")
plot(svp, data=dm)
svp
