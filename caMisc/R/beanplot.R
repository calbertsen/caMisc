##' Beanplot of something
##'
##' @param x Some object
##' @param ... Other arguments
##' @return Creates a plot
##' @author Christoffer Moesgaard Albertsen
##' @export
beanplot <- function (x, ...) 
    UseMethod("beanplot")


##' @importFrom graphics plot axis box segments polygon
##' @importFrom stats quantile approx density
##' @export
beanplot.list <- function(x,add=FALSE,onlybg=FALSE,commonscale=FALSE,col="white",border="black",ticks.col="black",mean.col="black",xlab="",ylab="",main="",ylim=NULL,quantiles.col="grey",quantiles=c(),...){
    n <- length(x)
    ds <- lapply(x,stats::density,...)
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
        if(is.null(ylim))
            ylim <- range(unlist(lapply(ds,function(xx)xx$x)))
        graphics::plot(0,0,xlim=c(0,n),ylim=ylim,axes=FALSE,ylab=ylab,xlab=xlab,main=main,type="n")
        graphics::axis(2)
        graphics::axis(1,at=1:n-0.5,labels=labels)
        graphics::box()
    }
    if(!onlybg){
        invisible(sapply(1:n,function(i) graphics::polygon(c(i-0.5 - ds[[i]]$y/maxy[i]/2*0.9,i-0.5 + rev(ds[[i]]$y)/maxy[i]/2*0.9),
                                                 c(ds[[i]]$x,rev(ds[[i]]$x)),
                                                 col = col,border=border)))
        invisible(sapply(1:n,function(i){
            yv <- stats::approx(ds[[i]]$x,ds[[i]]$y,mean(x[[i]]))$y/maxy[i]/2*0.9
            graphics::segments(i-0.5-yv,mean(x[[i]]),i-0.5+yv,mean(x[[i]]),lwd=3,col=mean.col)
            if(length(quantiles) > 0)
                for(q in 1:length(quantiles)){
                    vv <- stats::quantile(x[[i]],probs=quantiles[q])
                    yv <- stats::approx(ds[[i]]$x,ds[[i]]$y,vv)$y/maxy[i]/2*0.9
                    graphics::segments(i-0.5-yv,vv,i-0.5+yv,vv,lwd=3,col=quantiles.col)
                }
        }))
        invisible(sapply(1:n,function(i)sapply(x[[i]],function(y){
            yv <- stats::approx(ds[[i]]$x,ds[[i]]$y,mean(x[[i]]))$y/maxy[i]/2*0.9
            if(yv>0.1) yv <- 0.1
            graphics::segments(i-0.5-yv*0.5,y,i-0.5+yv*0.5,y,col=ticks.col)
            })))
    }
}

##' @export
beanplot.matrix <- function(x,...){
    y <- split(x,rep(1:ncol(x),each=nrow(x)))
    if(!is.null(colnames(x))){
        names(y) <- colnames(x)
    }
    beanplot(y,...)
}

##' @export
beanplot.numeric <- function(x,...){
    beanplot(list(x),...)
}

##' @importFrom stats model.frame
##' @export
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
