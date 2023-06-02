##' DTU color palette
##'
##' @param x name of colors to return
##' @return color codes
##' @author Christoffer Moesgaard Albertsen
##' @export
dtucols <- function(x){
    ## DTU color palette
    ## TODO: add argument for 80%, 70%, 60%, 50% tint
    names <- c("corporate", "purple", "orange", "blue", "grey", "yellow", "black", "navy", "green", "red", "pink", "brightgreen")
    if(missing(x))
        x <- names
    cols <- c("#990000", # Red
              "#79238E", #Purple
              "#FC7634", #Orange
              "#2F3EEA", #Blue
              "#DADADA", #Grey
              "#F6D04D", #Yellow
              "#000000", #Black
              "#030F4F", #Navy
              "#008835", #Green
              "#E83F48", #Red2
              "#F7BBB1", #Pink
              "#1FD082" #Bright green          
              )
    names(cols) <- names
    if(is.numeric(x)){
        out <- cols[x]
    }else if(is.character(x)){
        out <- cols[match(x, names)]
    }else{
        stop("Wrong input")
    }
    out    
}
##' Add text with background to plot
##'
##' @param x x-coordinate
##' @param y y-coordinate
##' @param labels label to add
##' @param ... arguments passed to text
##' @param cex size of text
##' @param font font of text
##' @param bg background color
##' @param bgex background extend factor
##' @param border border color of background
##' @return 
##' @author Christoffer Moesgaard Albertsen
##' @importFrom graphics rect text strwidth strheight
##' @export
bgtext <- function(x, y, labels, ...,
                   cex = 1, font = NULL,
                   bg = "white", bgex = 1, border = NA){
    w <- graphics::strwidth(labels, cex = cex, font = font)
    h <- graphics::strheight(labels, cex = cex, font = font)
    graphics::rect(x - 0.5 * bgex * w,
         y - 0.5 * bgex * h,
         x + 0.5 * bgex * w,
         y + 0.5 * bgex * h,
         col = bg, border = border)
    graphics::text(x,y,labels, cex=cex, font=font, ...)
}

##' Add map legend for choropleth map
##'
##' @param x Variable values plotted
##' @param cols Colors
##' @param txt Title / description
##' @return 
##' @author Christoffer Moesgaard Albertsen
##' @importFrom graphics par rect text
##' @export
makeMapLegend <- function(x, cols, txt){
    if(missing(txt))
        txt <- deparse1(substitute(x))
    nb <- length(cols)+1
    dx <- diff(rx <- range(x, na.rm = TRUE))
    breaks <- seq.int(rx[1L], rx[2L], 
                      length.out = nb)
    usr <- graphics::par("usr")
    xmin <- usr[1] + 0.02 * diff(usr[c(1,2)])
    xmax <- usr[1] + 0.04 * diff(usr[c(1,2)])
    ymin <- mean(usr[c(3,4)]) - 0.5 * 0.75 * diff(usr[c(3,4)])
    ymax <- mean(usr[c(3,4)]) + 0.5 * 0.75 * diff(usr[c(3,4)])
    yv <- seq(ymin,ymax,len=nb)
    for(i in 1:(nb-1)){
        graphics::rect(xmin,yv[i],xmax,yv[i+1], col = cols[i], border=NA)
    }
    nv <- approx(breaks,yv,pretty(breaks,7))
    graphics::text(xmax, nv$y, nv$x, pos = 4)
    graphics::text(mean(c(xmin,xmax)), ymax, txt, pos = 3)
}

##' Collapse vector to string
##'
##' @param x vector of values
##' @param collap rule for collapsing
##' @return a string
##' @author Christoffer Moesgaard Albertsen
##' @export
collapse <- function(x,
                     collap=c(rep(", ",length(x)-2),ifelse(length(x)>2,", and "," and "))){
    if(length(x) == 1)
        return(x)
    if(length(collap) < length(x)-1)
        collap <- rep(collap,len=length(x)-1)
    str <- x[1]
    for(i in 2:length(x))
        str <- paste(str,x[i],sep=collap[i-1])
    str
}

##' Print number as (LaTeX) fraction
##'
##' @param x number
##' @param dollar 
##' @return LaTeX code for the fraction
##' @author Christoffer Moesgaard Albertsen
##' @importFrom MASS as.fractions
##' @export
tofrac <- Vectorize(function(x, dollar = TRUE){
    if(dollar){
    paste0("$\\frac{",
           paste0(strsplit(as.character(MASS::as.fractions(x)),"/")[[1]],collapse = "}{"),
           "}$")
    }else{
    paste0("\\frac{",
           paste0(strsplit(as.character(MASS::as.fractions(x)),"/")[[1]],collapse = "}{"),
           "}")
    }
})

##' Convert number to display text
##'
##' @param x number
##' @param capitalize Capitalize first letter?
##' @return LaTeX code for the fraction
##' @author Text representation of number
##' @importFrom MASS as.fractions
##' @export
displayNum <- Vectorize(function(x,capitalize = FALSE, big.mark = ",", small.mark = ".", digits = 0){
    y <- switch(as.character(x),
                "0"="Zero",
                "1"="One",
                "2"="Two",
                "3"="Three",
                "4"="Four",
                "5"="Five",
                "6"="Six",
                "7"="Seven",
                "8"="Eight",
                "9"="Nine",
                formatC(as.numeric(x),big.mark=big.mark,small.mark=small.mark,digits=digits,format="f"))
    if(!capitalize & x < 10)
        return(tolower(y))
    return(y)                
},"x")

##' Format date with locale
##'
##' @param x date
##' @param format format to use 
##' @param locale locale to use
##' @param ... passed to strftime
##' @return formatted date string
##' @author Christoffer Moesgaard Albertsen
##' @export
formatDate <- function(x, format="",locale = Sys.getlocale("LC_TIME"), ...){
    ol <- Sys.getlocale("LC_TIME");
    Sys.setlocale("LC_TIME",locale)
    v <- strftime(x,format=format,...)
    ## invisible(apply(matrix(strsplit(ol,"(;|=)")[[1]],nrow=2),2,function(xx){
    ##         tryCatch({Sys.setlocale(xx[1],xx[2])},error=function(x)"")
    ##         }))
    Sys.setlocale("LC_TIME",ol)
    v    
}

##' Format number
##'
##' Small wrapper for formatC to limit text needed
##' @param x number
##' @param digits number of digits 
##' @return string
##' @author Christoffer Moesgaard Albertsen
##' @export
fd <- function(x,digits = 1) formatC(x, digits = digits, format="f")



##' Draw figure axis inside plot
##'
##' @param side side of plot to draw on
##' @return Nothing, but plots as side effect
##' @author Christoffer Moesgaard Albertsen
##' @importFrom grDevices axisTicks
##' @importFrom graphics par text segments
##' @export
axisInside <- function(side){
    usr <- graphics::par("usr")
    i1 <- if(side %in% c(2,4)){3:4}else{1:2}
    i2 <- if(side == 2){1}else if(side == 4){2}else if(side == 1){3}else{4}
    i3 <- if(side %in% c(2,4)){1:2}else{3:4}
    ticks <- grDevices::axisTicks(graphics::par("usr")[i1],FALSE)
    sgn <- if(side %in% c(1,2)){ 1 }else{ -1}
    pos <- ((1:4 + 1) %% 4 + 1)[side]
    tA <- usr[i2]+ sgn * 0.01 * diff(usr[i3])
    tB <- ticks
    tx <- if(side %in% c(2,4)){ tA }else{ tB }
    ty <- if(side %in% c(1,3)){ tA }else{ tB }
    graphics::text(tx,ty,ticks,pos = pos)
    sx0 <- if(side %in% c(2,4)){ usr[i2] }else{ ticks }
    sx1 <- if(side %in% c(2,4)){ usr[i2]+ sgn * 0.01 * diff(usr[i3]) }else{ ticks }
    sy0 <- if(side %in% c(1,3)){ usr[i2] }else{ ticks }
    sy1 <- if(side %in% c(1,3)){ usr[i2]+ sgn * 0.01 * diff(usr[i3]) }else{ ticks }
    graphics::segments(sx0,sy0,sx1,sy1)
}


##' Make a pixel matrix/array square
##'
##' @param p Pixel matrix/array
##' @param value Value of added pixels
##' @return 
##' @author Christoffer Moesgaard Albertsen
##' @export
makeSquare <- function(p, value = 0, asp = 1){
    if(is.na(asp)){
        w   <- par("pin")[1] / diff(par("usr")[1:2])
        h   <- par("pin")[2] / diff(par("usr")[3:4])
        asp <- w/h
    }
    p2 <- array(value, c(floor(max(dim(p)[1:2])),floor(max(dim(p)[1:2])*asp),dim(p)[3]))
    ii1 <- 1:dim(p)[1] + floor((dim(p2)[1] - dim(p)[1])/2)
    ii2 <- 1:dim(p)[2] + floor((dim(p2)[2] - dim(p)[2])/2)
    p2[ii1,ii2,] <- p
    p2
}

##' @export
addEllipsis <- function(x,y,w,h, len = 1000, ...){
    xx <- cos(seq(0,2*pi,len = len)) * w/2 + x
    yy <- sin(seq(0,2*pi,len = len)) * h/2 + y
    polygon(xx,yy,...)    
}
##' @export
addTextEllipsis <- function(x,y,label,height, cex=1,font=1,textcol = "black",...){
    w <- strwidth(label,cex=cex,font=font)*1.2
    if(missing(height))
        height <- w
    addEllipsis(x,y,w,height,...)
    text(x,y,label,col=textcol,cex=cex,font=font)
}

##' @export
makeShadowCirc <- function(x,y,r){
    addEllipsis(x,y,r*2,r*2, border = 11, lwd = 10)
}


##' @export
##' @importFrom grDevices col2rgb
##' @importFrom graphics rasterImage
addIcon <- function(x,y,icon, icol, cs = 0.15, angle = 0, asp = NA){
       if(is.na(asp)){
        w   <- par("pin")[1] / diff(par("usr")[1:2])
        h   <- par("pin")[2] / diff(par("usr")[3:4])
        asp <- w/h
    }

    icon0 <- makeSquare(icon,asp = 1)
    if(!missing(icol)){
        icol0 <- grDevices::col2rgb(icol)[,1] / 255
        for(i in 1:3)
            icon0[,,i] <- icol0[i]
    }
    graphics::rasterImage(icon0,x-cs*0.9/asp,y-cs*0.9, x+cs*0.9/asp,y+cs*0.9, angle = angle)
}


##' @export
##' @importFrom graphics text
addCircIcon <- function(x0,dy, txt, icon,iconTxt = NULL, iconFamily="Arial", border = "black", lwd = 10, icol = "black",cs = 0.15, cx = 1, angle = 0, bg = "white"){
    if(length(txt) < 3)
        txt <- c(txt, rep("",3 - length(txt)))
    graphics::text(x0,0.5-dy,txt[1],adj=0.5,font=2,cex=cx)
    graphics::text(x0,0.44-dy,txt[2],adj=0.5,font=2,cex=cx)
    graphics::text(x0,0.39-dy,txt[3],adj=0.5,font=2,cex=cx/2)
    addEllipsis(x0,0.2-dy,cs*2,cs*2, border = border, lwd = lwd, col = bg)
    if(!is.null(icon)){
        addIcon(x0,0.2-dy, icon, icol, cs, angle)
    }
    if(!is.null(iconTxt)){
        cxv <- strheight(iconTxt,family=iconFamily, cex = 1) / (cs * 0.9*ifelse(is.na(border),2,1))
        text(x0,0.2-dy,iconTxt,cex=1 / cxv,family=iconFamily,adj=c(0.5,0.5), col = icol)
    }        
}


##' @importFrom graphics par
##' @export
goldenRatio <- function(pos){
    if(is.character(pos))
        pos <- pmatch(pos, c("bottom","left","top","right"))
    if(is.numeric(pos))
        pos <- match(pos,1:4)
    if(any(is.na(pos)))
        stop("Wrong position argument")
    doOne <- Vectorize(function(i){
        usr <- graphics::par("usr")
        switch(i,
               usr[3] + 1/(1+1.61803398875) * diff(usr[3:4]),
               usr[1] + 1/(1+1.61803398875) * diff(usr[1:2]),
               usr[4] - 1/(1+1.61803398875) * diff(usr[3:4]),
               usr[2] - 1/(1+1.61803398875) * diff(usr[1:2])
               )
        })
    doOne(pos)
}
