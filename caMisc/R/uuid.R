uuid0 <- function(){
    toupper(paste(paste(rep(as.raw(0),4), collapse = ""),
                  paste(rep(as.raw(0),2), collapse = ""),
                  paste(rep(as.raw(0),2), collapse = ""),
                  paste(rep(as.raw(0),2), collapse = ""),
                  paste(paste(rep(as.raw(0),6), collapse = ""), collapse = ""),
                  sep = "-")
            )
}

uuid4 <- function(){
    Z <- sample(as.raw(0:255),14,replace = TRUE)
    X <- sample(as.raw(64:79),1)
    Y <- sample(as.raw((16*8):(16*12-1)),1)
    toupper(paste(paste(Z[1:4], collapse = ""),
                  paste(Z[5:6], collapse = ""),
                  paste(X,Z[7], sep = ""),
                  paste(Y,Z[8], sep = ""),
                  paste(Z[9:14], collapse = ""),
                  sep = "-")
            )
}
    

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param version 
##' @return 
##' @author Christoffer Moesgaard Albertsen
##' @export
uuid <- function(version = 4){
    fn <- switch(as.character(version),
                 "0" = uuid0,
                 "4" = uuid4,
                 stop("Version not implemented")
                 )
    return(fn())                 
}


uuid()

