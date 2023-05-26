##' Add alpha value to a color name
##'
##' @param name name of the color
##' @param alpha alpha value (between 0 and 1)
##' @return a new color name
##' @author Christoffer Moesgaard Albertsen
##' @export
addTrans <- Vectorize(function(name,alpha = 1){
    if(is.na(name) | is.null(name))
        return(name)
    arg <- as.list(grDevices::col2rgb(name)/255)
    names(arg) <- c("red","green","blue")
    arg$alpha <- alpha
    do.call(grDevices::rgb,arg)
})

##' Add tint to a color name
##'
##' @param name name of the color
##' @param tint tint value (between 0 and 1). For 1, the same 
##' @return a new color name
##' @author Christoffer Moesgaard Albertsen
##' @export
addTint <- Vectorize(function(name,tint = 0){
    if(is.na(name) | is.null(name))
        return(name)
    arg <- as.list(grDevices::col2rgb(name,TRUE)/255)
    names(arg) <- c("red","green","blue","alpha")    
    arg[1:3] <- lapply(arg[1:3], function(x){x + (1-x) * tint})    
    do.call(grDevices::rgb,arg)
})


##' Add shade to a color name
##'
##' @param name name of the color
##' @param shade shade value (between 0 and 1). For zero, the same color is returned.
##' @return a new color name
##' @author Christoffer Moesgaard Albertsen
##' @export
addShade <- Vectorize(function(name,shade = 0){
    if(is.na(name) | is.null(name))
        return(name)
    arg <- as.list(grDevices::col2rgb(name)/255)
    names(arg) <- c("red","green","blue")
    arg <- lapply(arg, function(x){x * (1-shade)})    
    do.call(grDevices::rgb,arg)
})


##' @export
toHSL <- function(red,green,blue,alpha = 0){
    R <- red
    G <- green
    B <- blue
    if(R < 0 || G < 0 || B < 0 || alpha < 0 || R > 1 || G > 1 || B > 1 || alpha > 1)
        stop("All values must be between 0 and 1.")
    maxv <- max(R,G,B)
    minv <- min(R,G,B)
    L <- 0.5 * (minv + maxv)
    S <- ifelse(isTRUE(all.equal(maxv,minv,check.attributes = FALSE)),
                0, (maxv - minv) / (1 - abs(2*L - 1)))
    if(isTRUE(all.equal(maxv,minv,check.attributes = FALSE))){
        H <- 0
    }else if(isTRUE(all.equal(maxv,R,check.attributes = FALSE))){
        H <- (G-B) / (maxv - minv)
    }else if(isTRUE(all.equal(maxv,G,check.attributes = FALSE))){
        H <- 2 + (B-R) / (maxv - minv)
    }else{
        H <- 4 + (R-G) / (maxv - minv)
    }
    H <- H*60 + 360 * as.numeric(H<0)
    return(c(H=H,S=S,L=L,alpha=alpha))
}

##' @export
toRGB <- function(H,S,L,alpha = 0){
    if(H < 0 || S < 0 || L < 0 || alpha < 0 || H >= 360 || S > 1 || L > 1 || alpha > 1)
        stop("H must be between 0 and 360. Remaining values must be between 0 and 1.")
    C <- (1 - abs(2*L-1)) * S
    X <- C * (1 - abs((H / 60)%%2 - 1))
    m <- L - C/2
    if(H >= 0 && H < 60){
        RGB <- c(red=C,green=X,blue=0) + m
    }else if(H >= 60 && H < 120){
        RGB <- c(red=X,green=C,blue=0) + m
    }else if(H >= 120 && H < 180){
        RGB <- c(red=0,green=C,blue=X) + m
    }else if(H >= 180 && H < 240){
        RGB <- c(red=0,green=X,blue=C) + m
    }else if(H >= 240 && H < 300){
        RGB <- c(red=X,green=0,blue=C) + m
    }else{
        RGB <- c(red=C,green=0,blue=X) + m
    }
    return(c(RGB,alpha=alpha))
}

##' @export
changeSaturation <- function(name, saturation){
    if(is.na(name) | is.null(name))
        return(name)
    arg <- as.list(grDevices::col2rgb(name,TRUE)/255)
    names(arg) <- c("red","green","blue","alpha")
    hsl <- as.list(do.call("toHSL",arg))
    hsl$S <- saturation
    rgbv <- as.list(do.call("toRGB",hsl))
    do.call(grDevices::rgb,rgbv)    
}
##' Turn pixel into grey scale
##'
##' @param p Pixel matrix/array
##' @return A grey scale pixel matrix/array
##' @author Christoffer Moesgaard Albertsen
toGreyscale <- function(p){    
    if(length(dim(p)) == 3){ ## Color image
        if(dim(p)[3] == 3){ ## RGB, no alpha
            pOut <- 0.2989 * p[,,1] + 0.5870 * p[,,2] + 0.1140 * p[,,3]
        }else if(dim(p)[3] == 4){ ## RGB and alpha
            pOut <- 0.2989 * p[,,1] + 0.5870 * p[,,2] + 0.1140 * p[,,3]
            pOut <- simplify2array(list(pOut,pOut,pOut,p[,,4]))
        }else if(dim(p)[3] == 1){ ## Already grey scale
            pOut <- p
        }else{ ## Something is wrong
            stop("Not sure what to do for an image with ",dim(p)[3]," RGB channels")
        }
    }else if(length(dim(p)) == 2){ ## Already grey scale
        pOut <- p
    }else{ ## Somehting is wrong
        stop("Not sure what to do for an image with ",length(dim(p))," dimension(s)")
    }
    return(pOut)
}
