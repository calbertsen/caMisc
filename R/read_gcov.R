
##' Read content of .gcov file
##'
##' @param file path to .gcov file
##' @return a gcov object
##' @author Christoffer Moesgaard Albertsen
##' @export
read_gcov <- function(file){
    l <- readLines(file)
    indicator <- unname(sapply(l,function(x)strsplit(x,":")[[1]][1]))
    hasCover <- grep("[[:digit:]]",indicator)
    noCover <- grep("#####",indicator)
    attr(l,"cover") <- hasCover
    attr(l,"nocover") <- noCover
    class(l) <- "gcov"
    return(l)
}

##' @export
print.gcov <- function(x,showAll=FALSE){
    filename <- strsplit(x[1],":Source:")[[1]][2]
    nc <- length(attr(x,"cover"))
    nnc <- length(attr(x,"nocover"))
    cat(filename,"\n")
    cat("Coverage: ",formatC(nc/(nnc+nc)*100,format="f",digits=2),"%","\n")

    if(showAll){
        cat("\n\n")
        cat(x,sep="\n")
        cat("\n")
    }        
}

##' Create coverage table from list of gcov objects
##'
##' @param x list of gcov objects
##' @param ... additional arguments passed to formatC
##' @return A coverage table
##' @author Christoffer Moesgaard Albertsen
##' @export
gcovTable <- function(x,...){
    if(!(is.list(x) & all(unlist(lapply(x,function(y)"gcov"%in%class(y))))))
        stop("Must be list of gcov")

    fnam <- unlist(lapply(x,function(y)tail(strsplit(strsplit(y[1],":Source:")[[1]][2],"/")[[1]],1)))
    covered <- unlist(lapply(x,function(y)length(attr(y,"cover"))))
    notcovered <- unlist(lapply(x,function(y)length(attr(y,"nocover"))))

    tab <- cbind(c(fnam,"Total"),
                 c(covered,sum(covered)),
                 c(notcovered,sum(notcovered)),
                 formatC(100*c(covered,sum(covered))/(c(covered,sum(covered)) +  c(notcovered,sum(notcovered)))),...)
    colnames(tab) <- c("File","Covered","Not Covered","Coverage (%)")
    rownames(tab) <- 1:nrow(tab)
    return(tab)
}
