
addTrans <- Vectorize(function(name,alpha = 1){
    if(is.na(name) | is.null(name))
        return(name)
    arg <- as.list(grDevices::col2rgb(name)/255)
    names(arg) <- c("red","green","blue")
    arg$alpha <- alpha
    do.call(grDevices::rgb,arg)
})

addTint <- Vectorize(function(name,tint = 0){
    if(is.na(name) | is.null(name))
        return(name)
    arg <- as.list(grDevices::col2rgb(name)/255)
    names(arg) <- c("red","green","blue")
    arg <- lapply(arg, function(x){x + (1-x) * tint})    
    do.call(grDevices::rgb,arg)
})

addShade <- Vectorize(function(name,shade = 0){
    if(is.na(name) | is.null(name))
        return(name)
    arg <- as.list(grDevices::col2rgb(name)/255)
    names(arg) <- c("red","green","blue")
    arg <- lapply(arg, function(x){x * (1-shade)})    
    do.call(grDevices::rgb,arg)
})

