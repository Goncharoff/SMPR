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




##########################   OKNA #####################################

#прямоугольное
u_func <- function(rast, h) {
  if(h == 0 | abs(rast/h) <= 1){
    return (0.5)
  } else {
    return(0)
  }
}
#епачникова
func_epanechnikov <-function(rast, h){
  if(h == 0 | abs(rast/h) <= 1){
    return(3/4 * (1 - (rast/h)^2))
  } else {
    return(0)
  }
}
#квадратичное
func_kvadrat <-function(rast, h){
  if(h == 0 | abs(rast/h) <= 1){
    return(15/16 * (1 - (rast/h)^2)^2)
  } else {
    return(0)
  }
}
#треугольное
func_treyg <-function(rast, h){
  if(h == 0 | abs(rast/h) <= 1){
    return(1-abs(rast/h))
  } else {
    return(0)
  }
}
#гаусовское

funk_gaus <- function(rast, h){
  if(h == 0 | abs(rast/h) <= 1){
    return ( (2*pi)^(-1/2) * exp(-1/2 * (rast/h)^2 ) )
  } else {
    return(0)
  }
}

#LOO for perem shag

LOO_perem_shag <- function(classificator_alg, jadro){ 
  vec <- c(seq(1, 150) * 0)
  tmp <- 1
    for (i in 1:147) { 
      cnt <- 0
      x_el <- c(iris[i, 3], iris[i, 4]) 
      x_sample <- iris[-i, 3:5]
      for (k in seq(i, 148, by=1) ){
        class <- classificator_alg(x_sample, x_el, jadro, k) 
        if (iris[i, 5] != class) { 
          cnt <- cnt + 1 
        }
      }
      vec[tmp] <- cnt / 150
      tmp = tmp + 1
      print(vec)
      print(tmp)
    }
  return (vec)
}

#parzen window with different step
parzen_window_perem_shag <- function(xel, z2, jadro, k) {
  orderedXl <- sortObjectsByDist(xel, z2, euclideanDistance)
  #print(k)
  #print(orderedXl[i,4])
  #print(orderedXl[k+1,4])
  n <- dim(orderedXl)[2]-1
  classes <-orderedXl[1:150, n]
  s = c("setosa" = 0, "versicolor" = 0, "virginica" = 0) 
  for (i in seq(1:148)){
    #print(i)
    s[[classes[i]]] <- s[[classes[i]]] + jadro(orderedXl[i,4], orderedXl[k+1,4]) 
  } 
  class <- names(which.max(s)) 
  return (class) 
}


LOO_pramoygolnik_shag  = LOO_perem_shag(parzen_window_perem_shag, u_func)
LOO_epachnikov_shag = LOO_perem_shag(parzen_window_perem_shag, func_epanechnikov)
LOO_kavdrat_shag = LOO_perem_shag(parzen_window_perem_shag, func_kvadrat)
LOO_gaus_shag = LOO_perem_shag(parzen_window_perem_shag, funk_gaus)
LOO_treug_shag = LOO_perem_shag(parzen_window_perem_shag, func_treyg)

tochka_epachnikova =  which( LOO_epachnikov_shag == min(LOO_epachnikov_shag[LOO_epachnikov_shag > 0]) )
tochka_kvarticheskoe =  which( LOO_kavdrat_shag == min(LOO_kavdrat_shag[LOO_kavdrat_shag > 0]) )
tochka_treygolnoe =  which( LOO_treug_shag == min(LOO_treug_shag[LOO_treug_shag > 0]) )
tochka_gauss =  which( LOO_gaus_shag == min(LOO_gaus_shag[LOO_gaus_shag > 0]) )
tochka_prjamoygolnoe =  which(LOO_pramoygolnik_shag == min(LOO_pramoygolnik_shag[LOO_pramoygolnik_shag > 0]) )
