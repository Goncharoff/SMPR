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

points(ax, ay, pch = 20, col = "black")

points(bx, by, pch = 20, col = "black")

points(cx, cy, pch = 20, col = "black")

points(p, pch = 20, col = "yellow", lwd = 9)

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
  return (orderedXl<-cbind( orderedXl, evcld = sort(distances[,2],decreasing =FALSE)))
  
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

 
class <-kNN(xl, z, k = 6) 
points(z[1], z[2], pch = 22, bg = colors[class], col = colors[class], asp = 1, lwd = 5)




##########################   OKNA #####################################

#пр€моугольное
u_func <- function(rast, h) {
  if(abs(rast/h) <= 1){
      return (0.5)
    } else {
      return(0)
  }
}
#епачникова
func_epanechnikov <-function(rast, h){
  if(abs(rast/h) <= 1){
    return(3/4 * (1 - (rast/h)^2))
  } else {
    return(0)
  }
}
#квадратичное
func_kvadrat <-function(rast, h){
  if(abs(rast/h) <= 1){
    return(15/16 * (1 - (rast/h)^2)^2)
  } else {
    return(0)
  }
}
#треугольное
func_treyg <-function(rast, h){
  if(abs(rast/h) <= 1){
    return(1-abs(rast/h))
  } else {
    return(0)
  }
}
#гаусовское

funk_gaus <- function(rast, h){
  if(abs(rast/h) <= 1){
    return ( (2*pi)^(-1/2) * exp(-1/2 * (rast/h)^2 ) )
  } else {
    return(0)
  }
}

#LOO дл€ окна, принимает классификатор и функцию €дра
  LOO <- function(classificator, fanc){ 
    vec <- c(seq(1, 45)) 
    tmp <- 1
    for (h in seq(0.5,5,by=0.1)) { 
      cnt <- 0 
      for (i in 1:150) { 
        x_el <- c(iris[i, 3], iris[i, 4]) 
        x_sample <- iris[-i, 3:5] 
        class <- classificator(x_sample, x_el, h, fanc) 
        #print(class)
        # print(iris[i, 5])
        #print(x_el)
        #print(x_sample)
        if (iris[i, 5] != class) { 
          cnt <- cnt + 1 
        } 
      }
      #print(cnt)
      vec[tmp] <- cnt / 150
      print(tmp) 
      tmp = tmp + 1
    } 
    return (vec)
  }
  
#ordinary parzen classificator   
parzen_window <- function(xl, z, h, fanc) {
    orderedXl <- sortObjectsByDist(xl, z, euclideanDistance) 
    n <- dim(orderedXl)[2]-1
    classes <-orderedXl[1:150, n]
    m = c("setosa" = 0, "versicolor" = 0, "virginica" = 0) 
    for (i in seq(1:149)){
      #print(m)
      m[[classes[i]]] <- m[[classes[i]]] + fanc(orderedXl[i,4], h) 
    } 
    class <- names(which.max(m)) 
    return (class) 
}

#LOO results for ordinary parzen window
LOO_pramoygolnik  = LOO(parzen_window, u_func)
LOO_epachnikov = LOO(parzen_window, func_epanechnikov)
LOO_kavdrat = LOO(parzen_window, func_kvadrat)
LOO_gaus = LOO(parzen_window, funk_gaus)
LOO_treug = LOO(parzen_window, func_treyg)

#part of drawing plots for ordinary parzen window classficators

h_vect = seq(0.5,5,by=0.1)
xl <- c(seq(0.5, 5, 0.1))

tochka_epachnikova =  which( LOO_epachnikov == min(LOO_pramoygolnik) )
tochka_kvarticheskoe =  which( LOO_kavdrat == min(LOO_kavdrat) )
tochka_treygolnoe =  which( LOO_treug == min(LOO_treug) )
tochka_gauss =  which( LOO_gaus == min(LOO_gaus) )
tochka_prjamoygolnoe =  which(LOO_pramoygolnik == min(LOO_pramoygolnik) )

par(mfrow=c(3,2))

plot(h_vect,LOO_pramoygolnik, type = "l", xaxt="n", xlab = "h value", ylab = "Error value", main = "ѕр€моугольное €дро")
axis(1, at = seq(0.5, 5, by = 0.1), las=1)
points(h_vect[tochka_prjamoygolnoe], LOO_pramoygolnik[tochka_prjamoygolnoe], col="red", pch = 19)

plot(h_vect,LOO_epachnikov, type = "l", xaxt="n", xlab = "h value", ylab = "Error value", main = "ядро ≈панечникова")
axis(1, at = seq(0.5, 5, by = 0.1), las=1)
points(h_vect[tochka_epachnikova], LOO_epachnikov[tochka_epachnikova], col="red", pch = 19)

plot(h_vect,LOO_kavdrat, type = "l", xaxt="n", xlab = "h value", ylab = "Error value", main = "ядро квадратичое")
axis(1, at = seq(0.5, 5, by = 0.1), las=1)
points(h_vect[tochka_kvarticheskoe], LOO_kavdrat[tochka_kvarticheskoe], col="red", pch = 19)


plot(h_vect,LOO_gaus, type = "l", xaxt="n", xlab = "h value", ylab = "Error value", main = "ядро √аусса")
axis(1, at = seq(0.5, 5, by = 0.1), las=1)
points(h_vect[tochka_gauss], LOO_gaus[tochka_gauss], col="red", pch = 19)

plot(h_vect,LOO_treug, type = "l", xaxt="n", xlab = "h value", ylab = "Error value", main = "ядро треугольное")
axis(1, at = seq(0.5, 5, by = 0.1), las=1)
points(h_vect[tochka_treygolnoe], LOO_treug[tochka_treygolnoe], col="red", pch = 19)
