

##' Get a pixel matrix from an image file
##'
##' @param file Path to image file
##' @param grey Should output be greyscale?
##' @return A matrix of pixel values (0-255)
##' @author Christoffer Moesgaard Albertsen
##' @importFrom jpeg readJPEG
##' @importFrom png readPNG
##' @importFrom tiff readTIFF
##' @importFrom utils tail
getPixelMatrix <- function(file, grey=TRUE){
    if(grepl("\\.jp(e)*g$",file,ignore.case=TRUE)){
        im <- jpeg::readJPEG(file)
    }else if(grepl("\\.png$",file,ignore.case=TRUE)){
        im <- png::readPNG(file)
    }else if(grepl("\\.tif(f)*$",file,ignore.case=TRUE)){
        im <- tiff::readTIFF(file)
    }else{
        stop(sprintf("Unsupported file format: %s. Only JPEG, PNG, and TIFF are supported.",tail(strsplit(file,".")[[1]],1)))
    }
    if(grey){
        imOut <- round(toGreyscale(im) * 255)
    }else{
        imOut <- round(im * 255)
    }
    return(imOut)
}


##' Plot image from file
##'
##' @param x path to image
##' @param objectFit How the image should fill the plot: "fill", "contain", "cover", "none","scale-down"
##' @param halign Horizontal alignment: c, l, r
##' @param valign Vertical alignment: c, t, b
##' @param noMargin Plot with oma and mar set to zero
##' @param ... other arguments
##' @return Plots the image
##' @author Christoffer Moesgaard Albertsen
##' @export
imagePlot <- function(x,
                      objectFit = c("fill", "contain", "cover", "none","scale-down"),
                      halign = c("c","l","r"),
                      valign = c("c","t","b"),
                      maxWidthPct = 1,
                      maxHeightPct = 1,
                      add = FALSE,
                      noMargin = TRUE,
                      ...
                      ){
    if(is.character(x)){
        im <- getPixelMatrix(x)
    }else if(is.array(x)){
        im <- x
    }else{
        stop("x should be a character or an image array")
    }

    if(maxWidthPct > 1 || maxWidthPct < 0)
        stop("maxWidthPct must be between 0 and 1.")

    if(maxHeightPct > 1 || maxHeightPct < 0)
        stop("maxHeightPct must be between 0 and 1.")
    
    objectFit <- match.arg(objectFit)
    halign <- match.arg(halign)
    valign <- match.arg(valign)

    if(!add){
        if(noMargin)
            par(mar = c(0,0,0,0), oma = c(0,0,0,0))
        plot.new()
    }
    
    px.per.in <- dev.size("px") / dev.size("in")
    ds.in <- dev.size("in")
    ps.in <- par("pin")
    ps.px <- ps.in * px.per.in
    par(usr = c(0,ps.px[1],0,ps.px[2]))
    usr <- par("usr")

    
    tb <- usr[3:4]
    if(valign == "c"){
        tb <- mean(tb) + c(-0.5,0.5) * maxHeightPct * diff(tb)
    }else if(valign == "t"){
        tb[1] <- tb[2] - maxHeightPct * diff(tb)
    }else{
        tb[2] <- tb[1] + maxHeightPct * diff(tb)
    }

    lr <- usr[1:2]
    if(halign == "c"){
        lr <- mean(lr) + c(-0.5,0.5) * maxWidthPct * diff(lr)
    }else if(halign == "r"){
        lr[1] <- lr[2] - maxWidthPct * diff(lr)
    }else{
        lr[2] <- lr[1] + maxWidthPct * diff(lr)
    }
    
    startingPoint <- list(xleft = lr[1], ybottom = tb[1], xright = lr[2], ytop = tb[2])


    center <- c(mean(lr), mean(tb))
    ims.px <- dim(im)[2:1]
    im.ratio <- ims.px / c(diff(lr), diff(tb))

    
    
    worker <- function(s){
        r <- startingPoint
        ## Scale height
        if(valign == "c"){
            r[c(2,4)] <- center[2] + c(-0.5,0.5) * ims.px[2] * (1 / s)
        }else if(valign == "t"){
            r[[2]] <- r[[4]] - 1 * ims.px[2] * (1 / s)
        }else{
            r[[4]] <- r[[2]] + 1 * ims.px[2] * (1 / s)            
        }
        ## Scale width
        if(halign == "c"){
            r[c(1,3)] <- center[1] + c(-0.5,0.5) * ims.px[1] * (1 / s)
        }else if(halign == "r"){
            r[[1]] <- r[[3]] - 1 * ims.px[1] * (1 / s)
        }else{
            r[[3]] <- r[[1]] + 1 * ims.px[1] * (1 / s)            
        }
    r
    }

    fill <- function(){
        startingPoint
    }
    cover <- function() worker(im.ratio[which.min(im.ratio)]);
    contain <- function() worker(im.ratio[which.max(im.ratio)]);
    none <- function() worker(1);
    scaleDown <- function(){
        if(any(im.ratio > 1)){
            return(contain())
        }else{
            return(none())
        }
    }

    r <- switch(objectFit,
                "fill"=fill(),
                "cover"=cover(),
                "contain"=contain(),
                "none"=none(),
                "scale-down"=scaleDown()
                )
    do.call("rasterImage",c(list(image=im), r))    
    invisible(im)
}
