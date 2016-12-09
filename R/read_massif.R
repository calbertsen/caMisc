

splitEqual <- function(x){
    tmp <- strsplit(x,"=")[[1]]
    res <- list(tmp[2]);names(res) <- tmp[1]
    return(res)
}

##' @export
read_massif <- function(file, keep_details = FALSE){
    l <- readLines(file)
    l <- l[-grep("^#",l)]
    desc <- strsplit(l[1]," --")[[1]][-1]
    desc <- sapply(desc,splitEqual,USE.NAMES=FALSE)
    cmd <- strsplit(l[2],": ")[[1]][2]
    time_unit <- strsplit(l[3],": ")[[1]][2]

    snaps <- c(grep("^snapshot",l),length(l)+1)
    indx <- cbind(head(snaps,-1),tail(snaps,-1)-1)

    snapshots <- lapply(as.list(1:nrow(indx)),function(i){
        s <- indx[i,1]; e <- indx[i,2]
        main <- sapply(l[s:(s+5)],splitEqual,USE.NAMES=FALSE)
        main[[1]] <- as.numeric(main[[1]])
        main[[2]] <- as.numeric(main[[2]])
        for(i in 3:(length(main)-1))
            main[[i]] <- structure(as.numeric(main[[i]]),class="object_size")
        if(main$heap_tree != "empty" & keep_details){
            main$heap_tree_detail <- as.list(l[(s+6):e])
        }else{
            main$heap_tree_detail <- list()
        }
        return(main)
    })

    res <- list("desc"=desc,
                "cmd"=cmd,
                "time_unit"=time_unit,
                "snapshots"=snapshots)
    class(res) <- "massif"
    return(res)
}

##' @export
as.data.frame.massif <- function(x){
    dat <- unlist(lapply(x$snapshots,function(y)unlist(y[3:5])))
    tid <- unlist(lapply(x$snapshots,function(y)unlist(y[2])))
    d <- matrix(dat,ncol=3,byrow=TRUE)
    colnames(d) <- unique(names(dat))
    d <- as.data.frame(d)
    d$total <- rowSums(d)
    d$time <- tid
    return(d)
}

##' @export
plot.massif <- function(x,...){
    d <- as.data.frame(x)
    Time <- d$time
    Memory <- d$total
    plot(Time,Memory,...)
}

##' @export
points.massif <- function(x,...){
    d <- as.data.frame(x)
    Time <- d$time
    Memory <- d$total
    points(Time,Memory,...)
}

##' @export
lines.massif <- function(x,...){
    d <- as.data.frame(x)
    Time <- d$time
    Memory <- d$total
    lines(Time,Memory,...)
}



