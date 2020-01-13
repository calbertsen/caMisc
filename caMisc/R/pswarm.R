

emswarm <- function(par,fn, gr, N, sigma,
                    lower=-5, upper=5,
                    dt=0.01,
                    minAttraction = c(0,0),
                    maxAttraction = c(1,20),
                    cap = Inf,
                    global = TRUE,
                    plot = FALSE,
                    control){

    if(missing(control))
        control <- list()
    if(is.null(control$iter.max))
        control$iter.max <- 1000
    if(is.null(control$gradient.max))
        control$gradient.max <- 1e-5
    if(length(lower) != length(par))
        lower <- rep(lower,length=length(par))
    if(length(upper) != length(par))
        upper <- rep(upper,length=length(par))
    
    Xspace <- cbind(lower,upper)
    X <- apply(Xspace,1,function(xx)runif(N,xx[1],xx[2]))
    fy <- apply(X,1,fn)
    y <- X[which.min(fy),]
    gy <- gr(y)

    ii <<- 0
    while(!max(abs(gy)) < control$gradient.max &&
          ii < control$iter.max &&
          (max(apply(X,2,sd)) > sigma*sqrt(dt)*1.1 || !global)){
              
        ii <<- ii + 1
        Xnew <- t(apply(X,1, function(x){
            alpha <- runif(2,minAttraction,maxAttraction)
            x - alpha[1] * dt * gr(x) - as.numeric(global) * alpha[2] * dt * (x - y) + sigma * sqrt(dt) * rnorm(length(x))
        }))

        fynew <- apply(Xnew,1,fn)
        ynew <- Xnew[which.min(fynew),]
        gynew <- gr(ynew)

        if(plot){
            plot(Xnew,pch=16, asp = 1,xlim=c(lower[1],upper[1]),ylim=c(lower[2],upper[2]),type="n",
                 main = sprintf("%04d; mgc: %.7f",ii,max(abs(gynew))))
                                        #image(xx,yy,z, add=TRUE)
            points(Xnew,pch=16)         
            points(y[1],y[2],pch=17,col=rgb(0.6,0,0.6,0.4))
            points(ynew[1],ynew[2],pch=17,col=rgb(0.6,0,0.6))
        }
        ydiff <- NA
        X <- Xnew
        y <- ynew
        fy <- fynew
        gy <- gynew  
    }
    return(list(par = y,
                objective = min(fy),
                iter = ii,
                gradient = gy))
}

## plot(x <- seq(-5,5,0.1),-log(0.8 * dnorm(x,-2,1) + 0.7 * dnorm(x,2,1)))
## fn = function(x)sum(x^2 - 10 * cos(2*pi*x) + 10)##function(x) -sum(log(0.8 * dnorm(x,-2,1) + 0.6 * dnorm(x,2,1)))
## globalMin <- c(0,0)
## gr = function(x) numDeriv::grad(fn,x)


## xx <- seq(lower[1],upper[1],len=200)
## yy <- seq(lower[2],upper[2],len=250)
## z <- matrix(apply(expand.grid(xx,yy),1,fn),length(xx),length(yy), byrow=FALSE)
            

## Xspace <- cbind(lower,upper)
## X <- apply(Xspace,1,function(xx)runif(N,xx[1],xx[2]))
## fy <- apply(X,1,fn)
## y <- X[which.min(fy),]
## gy <- gr(y)

## plot(X,pch=16, asp = 1,xlim=c(lower[1],upper[1]),ylim=c(lower[2],upper[2]),type="n",main = sprintf("%04d",0))
## image(xx,yy,z, add=TRUE)
## points(X,pch=16)
## points(globalMin[1],globalMin[2],col=rgb(0.6,0,0),pch=16)
## points(y[1],y[2],pch=17,col=rgb(0.6,0,0.6,0.4))


## opt0 <- emswarm(c(0,0),fn,gr,N=100,sigma=0.01, plot = TRUE)
## opt0

## opt <- nlminb(y,fn,gr)


## library(pso)

## psoptim(rep(NA,2),fn,gr,lower=-5,upper=5)


## library(globalOptTests)

## funName <- "Rastrigin"
## fn <- function(x)goTest(x,funName)
## gr <- function(x)numDeriv::grad(fn,x)

## opt0 <- emswarm(rep(0,getProblemDimen(funName)),fn,gr,N=100,sigma=0.1, plot = TRUE,dt=0.1,maxAttraction=c(1,20), control=list(iter.max=2000))
## points(matrix(rep(getGlobalOpt(funName),getProblemDimen(funName)),1),col="orange",pch=16)


## fn(rep(getGlobalOpt(funName),getProblemDimen(funName)))
## fn(opt0$par)


## xx <- seq(lower[1],upper[1],len=200)
## yy <- seq(lower[2],upper[2],len=250)
## z <- matrix(apply(expand.grid(xx,yy),1,fn),length(xx),length(yy), byrow=FALSE)

## image(xx,yy,z)

## z
