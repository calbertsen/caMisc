

## TODO:
## - Add wide / long direction
## -- Wide uses same info and has values side by side
## -- Long uses different info and has values below as rows

                                        # forestplot
##' @importFrom graphics plot.new par strwidth strheight rect segments text mtext
##' @importFrom stats uniroot
##' @importFrom methods is
##' @importFrom grDevices axisTicks
##' @export
forestplot <- function(values,
                       info,                       
                       trans = identity,
                       summarize = seq_len(nrow(info)),
                       equalWeight = FALSE,
                       xaxs = "r",
                       xlim = NA,
                       xlab = "Value",
                       keepRaw = TRUE,
                       addTotal = TRUE,
                       rowCols = c("white","lightgrey"),
                       valueCol = "black",
                       valueBorder = NA,
                       valueHeight = 0.5,
                       valueWidth = 1/3,
                       valueLty = 1,
                       valueLwd = 1,
                       summaryType = c("diamond"),
                       summaryCol = "red",
                       summaryBorder = NA,
                       summaryHeight = 0.8,
                       summaryWidth = 1/3,
                       summaryLty = 1,
                       summaryLwd = 1,
                       colorBy = Inf,
                       maxCex = 100,
                       sameScale = FALSE,
                       valueFraction = 1/(1 + 1.61803398875)
                       ){
    ## Prepare info
    ## if(length(by) == 0)
    if(methods::is(values,"matrix"))
        values <- list(values)
    if(!methods::is(values,"list") & all(sapply(values,function(x) is(x,"matrix"))))
        stop("values must be a matrix or a list of matrices")
    if(keepRaw){
        info_use <- info
        civ <- lapply(values,function(val) apply(val, 1, function(x) trans(x[1] + c(-2,0,2) * x[2])))
        isSummary <- rep(0,nrow(info))
    }else{
        info_use <- info[0,]
        civ <- lapply(values, function(val) apply(val, 1, function(x) trans(x[1] + c(-2,0,2) * x[2]))[,0,drop=FALSE])
        isSummary <- rep(0,nrow(info))[0]
    }
    ## summaries
    MakeSummary <- function(xx){
        kp <- !(is.nan(xx[,1]) | is.nan(xx[,2]) | is.na(xx[,1]) | is.na(xx[,2]))
        n <- sum(kp)
        vari <- if(equalWeight){ rep(1,n) }else{ xx[kp,2]^2 }
        w <- 1/vari
        w <- w / sum(w)
        Value  <-  sum(w * xx[kp,1]) #mean(xx[kp,1])
        Sd <- sqrt( w %*% diag(xx[kp,2]^2,n,n) %*% t(t(w)))[1,1]
        trans(Value + c(-2,0,2) * Sd)
    }
    addColumns <- function(x, nms){
        newnms <- setdiff(nms, colnames(x))
        if(length(newnms) == 0) return(x)
        xL <- as.list(x)
        xL[newnms] <- replicate(length(newnms),rep(NA,nrow(x)),FALSE)
        as.data.frame(xL[nms])
    }
    safeMax <- function(x) ifelse(length(x) == 0, 0, max(x))

    if(isTRUE(summarize)){
        summarize <- 1:(nrow(info)-1)
    }else if(is.character(summarize)){
        summarize <- na.omit(match(summarize, info))
    }else if(is.integer(summarize) | is.numeric(summarize)){
        summarize <- na.omit(match(summarize, 1:(nrow(info)-1)))
    }else if(is.logical(summarize) & !summarize){
        summarize <- integer(0)
    }else{
        stop("Wrong summarize")
    }      

    if(length(summarize) > 0)
        for(j in seq_along(summarize)){
            i <- summarize[j]
            vv <- lapply(values, function(val) lapply(split(as.data.frame(val), apply(info[,1:i,drop=FALSE],1,paste,collapse="~:~")),MakeSummary))
            ix <- data.frame(do.call("rbind",strsplit(names(vv[[1]]),"~:~")))
            colnames(ix) <- colnames(info)[1:(i)]
            isSummary <- c(isSummary,rep(j, nrow(ix)))
            info_use <- rbind(info_use, addColumns(ix,colnames(info_use)))
            civ <- lapply(seq_along(vv), function(qq) cbind(civ[[qq]], do.call("cbind",vv[[qq]])))
        }
    ## Add total
    if(addTotal){
        vv <- lapply(values, function(val) matrix(MakeSummary(as.data.frame(val)),nrow = 3))
        isSummary <- c(isSummary,safeMax(isSummary) + 1)
        info_use <- rbind(info_use, addColumns(data.frame(NA),colnames(info_use)))
        civ <- lapply(seq_along(vv), function(qq) cbind(civ[[qq]],vv[[qq]]))
    }
    ## Add Total name
    indxPlot <- do.call("order",as.list(info_use))
    colKeep <- 1:ncol(info_use)
    if(!keepRaw)
        colKeep <- seq_len(pmax(safeMax(summarize),1))    
    i2p <- info_use[indxPlot,colKeep, drop = FALSE]
    i2p <- do.call("rbind",lapply(split(i2p, 1:nrow(i2p)), function(xx){
        xx[is.na(xx)][1] <- "Total"
        xx
    }))
    ## Clean info
    i2p <- as.data.frame(lapply(as.list(i2p), function(xx){
        xx[c(FALSE,xx[-1] == head(xx,-1)) | is.na(xx)] <- ""
        xx
    }))
    v2p <- lapply(civ, function(xx) xx[,indxPlot, drop = FALSE])
    s2p <- as.integer(isSummary)[indxPlot]
    if(is.character(colorBy))
        colorBy <- match(colorBy,colnames(info),ncol(info))
    suppressWarnings(colorBy <- as.numeric(colorBy))
    if(length(colorBy) != 1)
        stop("colorBy myst have length 1")
    if(!is.na(colorBy) && colorBy > 0 && colorBy <= ncol(i2p)){        
        colIndx <- cumsum(!(i2p[,colorBy] %in% c(""))) #,"Total")) | apply(i2p,1,function(x) all(x %in% c("","Total")) && tail(x,1) == ""))
    }else{
        colIndx <- 1:nrow(i2p)
    }

    ## Ready for plotting
    graphics::plot.new()
    fin <- graphics::par("fin")
    cin <- graphics::par("cin")
    usr <- graphics::par("usr")
    asp <- fin[2] / fin[1]
    ## Info needed
    nr <- nrow(i2p)
    rowEdges <- seq(usr[4],usr[3],length = nr+1)
    rowCenters <- (head(rowEdges,-1) + tail(rowEdges,-1)) / 2
    rp <- diff(usr[3:4]) / (nr)    
    valueFrac <- rep(valueFraction / length(values),length(values))
    infoFrac <- 1 - sum(valueFrac)
    iwf <- sapply(lapply(as.list(i2p), graphics::strwidth, cex = 1),max)
    iwf <- iwf / sum(iwf)
    colWidth <- diff(usr[1:2]) * infoFrac * iwf
    cc0 <- seq(0.01,5,len=100)
    cexH <- tryCatch(stats::uniroot(function(v){max(sapply(lapply(c(list(LETTERS),list(letters),as.list(i2p)), graphics::strheight, cex = v),max)) - rp*0.5},c(0.001,100))$root,error = function(e) 100)    
    cexW <- tryCatch(stats::uniroot(function(v){sapply(lapply(as.list(i2p), graphics::strwidth, cex = v),max) + graphics::strwidth("M",cex=v) - colWidth * 0.9},c(0.001,100))$root,error = function(e) 100)
    cex <- pmin(cexW, cexH, maxCex)
    lw <- graphics::strwidth("M",cex=cex)
    lh <- graphics::strheight("M",cex=cex)
    ## Add rows
    for(i in 1:nr)
        graphics::rect(usr[1],usr[4] - (i-1)*rp,
             usr[2],usr[4] - (i)*rp, col=rowCols[((colIndx[i]-1)%%length(rowCols)) + 1],border=NA)
    ## Info area
    graphics::rect(usr[1], usr[3],
         usr[1] + diff(usr[1:2]) * infoFrac, usr[4], col = NA, border="black",lwd=2)
    ## Value area
    sapply(seq_along(valueFrac), function(ii)
        graphics::rect(usr[1] + diff(usr[1:2]) * cumsum(c(infoFrac,valueFrac))[ii], usr[3],
             usr[1] + diff(usr[1:2]) * cumsum(c(infoFrac,valueFrac))[ii+1], usr[4],
             col = NA, border = "black",lwd=2)
        )
    ## Fill info
    startIBox <- usr[1] + cumsum(c(0,head(iwf,-1))) * diff(usr[1:2]) * infoFrac
    for(j in seq_along(startIBox))
        graphics::text(startIBox[j] + lw, (head(rowEdges,-1) + tail(rowEdges,-1)) / 2,
             i2p[,j], pos = 4, cex = cex, offset = 0)
    graphics::mtext(colnames(i2p), side = 3, at = startIBox + lw, adj = 0,
          font = graphics::par("font.lab"), cex = graphics::par("cex.lab"), col = graphics::par("col.lab"))
    xlab <- rep(xlab, length(v2p))
    sapply(seq_along(v2p), function(i)
        graphics::title(xlab = xlab[i], adj = cumsum(c(infoFrac,valueFrac))[i] + valueFrac[i]/2)
        )
    if(!is.null(names(values)))
        graphics::mtext(names(values), side = 3, at = usr[1] + diff(usr[1:2]) * head(cumsum(c(infoFrac,valueFrac)),-1), adj = 0,
              font = graphics::par("font.lab"), cex = graphics::par("cex.lab"), col = graphics::par("col.lab"))
    ## Prep x axis !! Handle multiple values
    if(any(is.na(xlim))){
        if(sameScale){
            xlim <- replicate(length(v2p),range(v2p,xlim, na.rm = TRUE, finite = TRUE), simplify = FALSE)
        }else{
            xlim <- lapply(v2p, function(xx) range(xx,xlim, na.rm = TRUE, finite = TRUE))
        }
    }else if(!is(xlim,"list")){
        xlim <- replicate(length(v2p), xlim, simplify = FALSE)
    }
    if(xaxs == "i"){                        # do nothing
        xlim <- xlim
    }else if(xaxs == "r"){             #Extend 4%
        xlim <- lapply(xlim, function(xx){ xx + c(-1,1) * diff(xx) * 0.04 })
    }else{
        stop("xaxs not implemented")
    }
    x2plot <- function(x, vi = 1) {
        approx(xlim[[vi]],
               usr[1] + diff(usr[1:2]) * c(infoFrac + cumsum(c(0,valueFrac))[vi],
                                           infoFrac + cumsum(c(0,valueFrac))[vi+1]),
               rule = 2,
               x)$y
    }
    ## cPch1 <- uniroot(function(v){strheight("\U2666",cex=v) - rp*0.8},c(0.001,10))$root
    ## cPch2 <- uniroot(function(v){strheight("\U2022",cex=v) - rp*0.8},c(0.001,10))$root
    av <- lapply(xlim, function(xx) grDevices::axisTicks(xx,FALSE))
    invisible(sapply(seq_along(av), function(ii) axis(1,at = x2plot(av[[ii]],ii), labels = av[[ii]])))
    ## Fill values
    ## a) Non summary
    if(sum(s2p == 0) > 0){
        for(i in seq_along(v2p)){
            graphics::segments(x2plot(v2p[[i]][1,s2p==0],i),rowCenters[s2p==0],
                     x2plot(v2p[[i]][3,s2p==0],i),rowCenters[s2p==0],
                     col = valueCol,
                     lty = valueLty,
                     lwd = valueLwd)
            graphics::rect(x2plot(v2p[[i]][2,s2p==0],i) - rp * valueHeight * asp / 2 * valueWidth,
                 rowCenters[s2p==0] - rp * valueHeight / 2,
                 x2plot(v2p[[i]][2,s2p==0],i) + rp * valueHeight * asp / 2 * valueWidth,
                 rowCenters[s2p==0] + rp * valueHeight / 2,
                 border = valueBorder, col = valueCol)
        }
    }
    ## b) Summaries
    if(max(s2p) > 0){
        summaryType <- match.arg(rep(summaryType,length.out = max(s2p)),c("diamond","ci"), several.ok = TRUE)
        summaryCol <- rep(summaryCol, length.out = max(s2p))
        summaryLty <- rep(summaryLty, length.out = max(s2p))
        summaryLwd <- rep(summaryLwd, length.out = max(s2p))
        summaryBorder <- rep(summaryBorder, length.out = max(s2p))
        summaryHeight <- rep(summaryHeight, length.out = max(s2p))
        summaryWidth <- rep(summaryWidth, length.out = max(s2p))
        for(ss in seq_len(max(s2p))){
            if(summaryType[ss] == "ci"){
                for(i in seq_along(v2p)){
                    graphics::segments(x2plot(v2p[[i]][1,s2p==ss],i),
                             rowCenters[s2p==ss],
                             x2plot(v2p[[i]][3,s2p==ss],i),
                             rowCenters[s2p==ss],
                             col = summaryCol[ss],
                             lty = summaryLty[ss],
                             lwd = summaryLwd[ss]
                             )
                    graphics::rect(x2plot(v2p[[i]][2,s2p==ss],i) - rp * summaryHeight[ss] * asp / 2 * summaryWidth[ss],
                         rowCenters[s2p==ss] - rp * summaryHeight[ss] / 2,
                         x2plot(v2p[[i]][2,s2p==ss],i) + rp * summaryHeight[ss] * asp / 2 * summaryWidth[ss],
                         rowCenters[s2p==ss] + rp * summaryHeight[ss] / 2,
                         border = summaryBorder[ss], col = summaryCol[ss])
                }
            }else if(summaryType[ss] == "diamond"){
                MakeDiamond <- Vectorize(function(i,jj){
                    l <- v2p[[jj]][1,i]
                    c <- v2p[[jj]][2,i]
                    r <- v2p[[jj]][3,i]
                    polygon(c(x2plot(l,jj), x2plot(c,jj), x2plot(r,jj), x2plot(c,jj), x2plot(l,jj)),
                            c(rowCenters[i],rowCenters[i] + rp * summaryHeight[ss]/2,
                              rowCenters[i],rowCenters[i] - rp * summaryHeight[ss]/2,
                              rowCenters[i]),
                            border = summaryBorder[ss],
                            col = summaryCol[ss])
                })
                invisible(outer(which(s2p==ss),seq_along(v2p), MakeDiamond))
            }
        }
    }
    invisible(x2plot)
}
