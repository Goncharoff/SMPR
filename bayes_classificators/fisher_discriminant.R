data(iris)
x <- cbind(iris$Petal.Length,iris$Petal.Width)
Y <- ifelse(iris$Species == "virginica", +1, -1)

#вектор средних значений Petal.Length Petal.Width признаков
#получяем вектор
u <- apply(x,2,mean)
 
up <- apply(subset(x,Y==+1), 2, mean)
un <- apply(subset(x,Y==-1), 2, mean)
#кол-во объектов с меткой 1 = 50
np <- sum(Y==+1)

#кол-во объектов с меткой -1 = 100
nn <- sum(Y==-1)

#вектор -пи до пи с шаго 0.05
t <- seq(- pi , pi, 0.05)

#
uv <- cbind(cos(t), sin(t))

#100 * ((2.6, 0.7) - (3.7, 1.1))  * 
SB <- nn * (un - u) %*% t(up - u)

scatter <- function(v){
  ((v - un) %*% t(v - un)) + ((v - up) %*% t(v - up))
}
 
SW <-matrix(apply(apply(x,1,scatter), 1, sum ), nrow=2 )
 
action <- function(uv, m) {
      abs( uv %*% m %*% matrix(uv) )
    }

ratios <- apply(uv, 1, action, SB) / apply(uv, 1, action, SW)

mr <- which.max(ratios)
muv <- uv[mr,]
mv <- 40*ratios[mr]*muv
xp <- as.vector(x %*% muv)
rxp <- round(range(xp),0)+c(-1,1)
xpn <- subset(xp,Y==-1)
xpp <- subset(xp,Y==+1)
b = (mean(xpp) * sd(xpn) + mean(xpn) * sd(xpp)) /(sd(xpp) + sd(xpn))
plot(x,col=Y+3,asp=1, xlab = "Sepal.Length",
     ylab = "Sepal.Width", main = paste("LDF"))
par(lwd=2)
abline(b/muv[2],-muv[1]/muv[2])
distance.from.plane = function(x,w,b) {
     b - sum(x*w)
 }
classify.fisher = function(x,w,b) {
   distances = apply(x, 1, distance.from.plane, w, b)
  return(ifelse(distances < 0, -1, +1))
 }

sum(abs(Y - classify.fisher(x,muv,b) ))
