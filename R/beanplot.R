
beanplot <- function (x, ...) 
    UseMethod("beanplot")

beanplot.list <- function(x,add=FALSE,onlybg=FALSE,commonscale=FALSE,col="white",border="black",ticks.col="black",mean.col="black",xlab="",ylab="",main="",...){
    n <- length(x)
    ds <- lapply(x,density,...)
    maxy <- unlist(lapply(ds,function(xx)max(xx$y)))
    labels <- names(x)
    if(is.null(labels) & n > 1){
        labels <- 1:n
    }else if(is.null(labels) & n == 1){
        labels <- NA
        }
    if(commonscale)
        maxy <- rep(max(maxy),length(maxy))
    if(!add){
        plot(0,0,xlim=c(0,n),ylim=range(unlist(lapply(ds,function(xx)xx$x))),axes=FALSE,ylab=ylab,xlab=xlab,main=main,type="n")
        axis(2)
        axis(1,at=1:n-0.5,labels=labels)
        box()
    }
    if(!onlybg){
        invisible(sapply(1:n,function(i) polygon(c(i-0.5 - ds[[i]]$y/maxy[i]/2*0.9,i-0.5 + rev(ds[[i]]$y)/maxy[i]/2*0.9),
                                                 c(ds[[i]]$x,rev(ds[[i]]$x)),
                                                 col = col,border=border)))
        invisible(sapply(1:n,function(i){
            yv <- approx(ds[[i]]$x,ds[[i]]$y,mean(x[[i]]))$y/maxy[i]/2*0.9
            segments(i-0.5-yv,mean(x[[i]]),i-0.5+yv,mean(x[[i]]),lwd=3,col=mean.col)
        }))
        invisible(sapply(1:n,function(i)sapply(x[[i]],function(y){
            yv <- approx(ds[[i]]$x,ds[[i]]$y,mean(x[[i]]))$y/maxy[i]/2*0.9
            if(yv>0.1) yv <- 0.1
            segments(i-0.5-yv*0.5,y,i-0.5+yv*0.5,y,col=ticks.col)
            })))
    }
}

beanplot.matrix <- function(x,...){
    beanplot(split(x,rep(1:ncol(x),each=nrow(x))),...)
}

beanplot.numeric <- function(x,...){
    beanplot(list(x),...)
}

beanplot.formula <- function(formula,data=NULL,subset,na.action = NULL,...){
    ## Heavily inspired by graphics:::boxplot.formula
    if (missing(formula) || (length(formula) != 3L)) 
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.action
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    beanplot(split(mf[[response]], mf[-response]), ...)
}
