require(MASS)
##The function CVLoess##
CVLoess <- function(x, y, h, k){
  numb <- rep(c(1:k), n/k)
  xx<-c()
  yy<-c()
  ##Reorder the data to get the k folds.##
  for(ss in 1:k){
    xx <- c(xx, x[which(numb== ss)])
    yy <- c(yy, y[which(numb== ss)])
  }
  ##Choose the best h##
  RSS <- c()
  for(b in 1: length(h)){
    Rss <- c()
    for(i in 1: k){
      kk<-n/k
      num <- rep(c(1:k), n/k)
      xtrain <- xx[-(((i - 1)*kk + 1): (i*kk))] 
      ytrain <- yy[-(((i - 1)*kk + 1): (i*kk))]
      ytrain1 <- ytrain[order(xtrain)]
      xtrain1 <- sort(xtrain)
      xvalid <- xx[((i - 1)*kk + 1): (i*kk)] 
      yvalid <- yy[((i - 1)*kk + 1): (i*kk)]
      fit = loess(ytrain1 ~ xtrain1, span = h[b], degree=1, control=loess.control(surface="direct"))
      yvalidhat = predict(fit, data.frame(xtrain1 = xvalid), type="response")
      Rss[i] <- sum((yvalid - yvalidhat)^2)/kk
    }
    RSS[b] <- mean(Rss)
  }
  return(h[which.min(RSS)])
}

##Use the function to get the best h.##
f = function(x) (1 + 10*x - 5*x^2)*sin(10*x)
n = 1e3
k <- 5
nn <- n*(1-1/k)
x = runif(n)
x<-sort(x)
y = f(x) + rnorm(n)
h <- seq(0.01, 1, 0.01)
hbest <- CVLoess(x, y, h, k)
cat("The best h that we choose is", hbest)


##Test##
##Generate 200 new data to test our method.##
Rss <- c()
f = function(x) (1 + 10*x - 5*x^2)*sin(10*x)
nvalid = 200
xvalidation = runif(nvalid)
xvalidation = sort(xvalidation)
yvalidation = f(xvalidation) + rnorm(nvalid)
fit1 = loess(yvalidation ~ xvalidation, span = hbest, degree=1, control=loess.control(surface="direct"))
yvalidhat1 = predict(fit1, xvalidation, type="response")
plot(xvalidation, yvalidation)
lines(xvalidation, yvalidhat1, col = 'green', lwd=2)
a <- seq(0.01, 1, 0.01)
lines(a, f(a), col = 'red', lwd=2)
##In this problem, we best h is about 0.11. From the graph, we can see that 
##the model is fitted well by the bandwidth h. 
