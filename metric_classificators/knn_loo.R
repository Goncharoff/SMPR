v <- iris[, 3:4]

p <- c(3, 1)


avg <- function(x)
  
{
  sum(x) / length(x)
  
}


colors <-
  c("setosa" = "red",
    "versicolor" = "green",
    "virginica" = "blue")
ax <- avg(iris[iris$Species == "setosa", 3])

ay <- avg(iris[iris$Species == "setosa", 4])

bx <- avg(iris[iris$Species == "versicolor", 3])

by <- avg(iris[iris$Species == "versicolor", 4])

cx <- avg(iris[iris$Species == "virginica", 3])

cy <- avg(iris[iris$Species == "virginica", 4])


plot(iris[, 3:4],
     pch = 21,
     bg = colors[iris$Species],
     col = colors[iris$Species])


dist <- function(u, v)
  
{
  sqrt(sum((u - v) ^ 2))
  
}

a <- dist(c(ax, ay), p)

b <- dist(c(bx, by), p)

c <- dist(c(cx, cy), p)

min(c(a, b, c))

euclideanDistance <- function(u, v) {
  sqrt(sum((u - v) ^ 2))
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  
  distances <- matrix(NA, l, 2) 
  for (i in 1:l) {
    distances[i,] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  
  orderedXl <- xl[order(distances[, 2]),] 
  return (orderedXl)
  
}

kNN <- function(xl, z, k) {
  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance) 
  n <- dim(orderedXl)[2] - 1 
  classes <-orderedXl[1:k, n + 1] 
  counts <- table(classes)
  class <- names(which.max(counts)) 
  return (class)
}

plot(
  iris[, 3:4],
  pch = 21,
  bg = colors[iris$Species],
  col = colors[iris$Species],
  asp = 1
)

  z <-c(2.7, 1) 
xl <-iris[, 3:5] 
class <-kNN(xl, z, k = 6) 
points(z[1], z[2], pch = 22, bg = colors[class], col = colors[class], asp = 1, lwd = 5)

LOO <- function(classificator){ 
    vec <- c(seq(1, 150)) 
    for (k in 3:7) { 
      cnt <- 0 
      
      for (i in 1:150) { 
        x_el <- c(iris[i, 3], iris[i, 4]) 
        x_sample <- iris[-i, 3:5] 
        class <- classificator(x_sample, x_el, k) 
        if (iris[i, 5] != class) { 
          cnt <- cnt + 1 
        } 
      } 
      vec[k] <- cnt / 150 
      print(vec[k]) 
    } 
    
  }
  
  print(arr)
  plot(c(seq(1,5)),arr,type="l",ylab="Error Rate",
       xlab="K",main="Error Rate for Iris", ylim = c(0.4, 0.6))
  tochka = which(arr == min(arr))
  arr2 <- (seq(1,50))
  plot(arr2, arr, type = "l", xlab = "k", ylab="LOO value", main = "LOO kNN")
  points(arr2[tochka], arr[tochka], pch = 19, col = "red")
  
  
  
  
  
  