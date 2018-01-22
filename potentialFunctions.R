require("plotrix")

source("utils.R")

potentialFuncs = function(distances, classes, potentials, h) {
  w = potentials * ker(distances / h)
  names(w) = classes
  
  classes = unique(classes)
  w = sapply(classes, function(name, arr) sum(arr[names(arr) == name]), w)
  
  #if (max(w) == 0) return("fail") 
  
  return(classes[which.max(w)])
}

potentials = function(points, classes, h, treshold) {
  n = dim(points)[1]
  potentials = integer(n)
  
  mistakes = treshold + 1 
  while (mistakes > treshold) {
    flag = TRUE
    while (flag) {
      i = sample(1:n, 1)
      u = points[i,] 
      distances = Util.getDist(points, u, Util.euclidDist)
      
      if (potentialFuncs(distances, classes, potentials, h) != classes[i]) {
        potentials[i] = potentials[i] + 1
        flag = FALSE 
      }
    }
    
    mistakes = 0
    for (i in 1:n) {
      u = points[i,]
      distances = Util.getDist(points, u, Util.euclidDist)
      
      if (potentialFuncs(distances, classes, potentials, h) != classes[i]) {
        mistakes = mistakes + 1
      }
    }
    print(mistakes)
    print(potentials)
  }    
  
  return(potentials)
}

h <- rep(1, 150)
ker <- Util.triangleKer

potentials <- potentials(Util.petals, Util.classes, h = h, 5)

names(Util.colors) <- unique(Util.classes)
plot(Util.petals, bg <- Util.colors[Util.classes], pch = 21, asp = 1, xlim = Util.xlim, ylim = Util.ylim)
title(main = "Potential functions densities circle")

densities = potentials / max(potentials)
n <- length(potentials)
for (i in 1:n) {
  x = Util.petals[i, 1]
  y = Util.petals[i, 2]
  r = h[i]
  d = densities[i] / 5 
  color = adjustcolor(Util.colors[Util.classes[i]], d)
  
  if (d > 0) draw.circle(x, y, r, 50, border = color, col = color)
}