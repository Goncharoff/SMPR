Util.petals = iris[, 3:4]
Util.classes = iris[, 5]
Util.colors = c("red", "green3", "blue")

Util.xlim = c(min(Util.petals[, 1]), max(Util.petals[, 1]))
Util.ylim = c(min(Util.petals[, 2]), max(Util.petals[, 2]))


Util.euclidDist = function(p1, p2) sqrt(sum((p1 - p2) ^ 2))
Util.getDist = function(points, g, dist_func) apply(points, 1, dist_func, g)


Util.rectKer = function(r) 0.5 * (abs(r) <= 1)

Util.triangleKer = function(r) (1 - abs(r)) * (abs(r) <= 1)

Util.quartKer = function(r) (15 / 16) * (1 - r^2)^2 * (abs(r) <= 1)

Util.epanKer = function(r) (3 / 4) * (1 - r^2) * (abs(r) <= 1)

Util.gaussKer = function(r) dnorm(r) #((2 * pi)^(-1/2)) * exp(-1/2 * r^2) * (abs(r) <= 1) 

Util.kernels = list("Rectangle" = Util.rectKer, "Triangle" = Util.triangleKer, 
                    "Quartic" = Util.quartKer, "Epanechnikov" = Util.epanKer, "Gauss" = Util.gaussKer)

Util.LOO = function(points, classes ,classFunc,hLims = 0) {
  n = dim(points)[1]
  if(identical(classFunc,kNN.kNN) || identical(classFunc,WkNN.WkNN)) {
    loo = double(n-1) #n-1 because one element in sample always missing
    
    for (i in 1:n) {
      
      distances = Util.getDist(points[-i,], points[i,], Util.euclidDist)
      names(distances) = classes[-i]
      sort_dist = sort(distances)
      
      for (l in 1:n - 1) {
        bestClass = classFunc(sort_dist, l)
        loo[l] = loo[l] + ifelse(bestClass == classes[i], 0, 1) #same as ternary operator
      }
    }
  } else if((identical(classFunc, PW.PW))) {
    loo = double(length(hLims))
    
    for (i in 1:n) {
      distances = Util.getDist(points[-i,], points[i,], Util.euclidDist)
      names(distances) = classes[-i]
      
      for (j in 1:length(hLims)) {
        h = hLims[j]
        bestClass = PW.PW(distances, classes[-i], h)
        loo[j] = loo[j] + ifelse(bestClass == classes[i], 0, 1)
      }
    }
  }
  loo = loo / n
  return(loo)
}

Util.drawMap = function(points, classes, classFunc, hOpt = 0, kOpt = 0, potentials = 0, h = 0 ) {
  
  for (x in seq(Util.xlim[1], Util.xlim[2], 0.1)) {
    for (y in seq(Util.ylim[1], Util.ylim[2], 0.1)) {
      u = c(round(x, 1), round(y, 1))
      if (any(apply(Util.petals, 1, function(v) all(v == u)))) next
      
      distances = Util.getDist(points, u, Util.euclidDist)
      names(distances) = classes
      if(identical(classFunc,PF.PF)) {
        
        bestClass = classFunc(distances, classes, potentials, h)
        
      }
      
      points(u[1], u[2], col = Util.colors[bestClass], pch = 21)
    }
  }
}