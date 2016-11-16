


##' Convert DZSlides to PDF
##'
##' Uses the webshot package to convert html DZSlides to a PDF (through PNG images).
##' @param filein Path and name of input file with DZSlides. Should either be in working directory or be on the form "file:///home/full/path/to/slides.html"
##' @param fileout Name of output pdf file
##' @param sizeFactor Factor to scale png of slides
##' @param delay Delay before capturing slide
##' @return Nothing
##' @author Christoffer Moesgaard Albertsen
##' @import webshot
##' @importFrom XML htmlTreeParse getNodeSet xmlRoot
##' @export
dz2pdf <- function(filein,fileout,sizeFactor = 4, delay = 0.5){
    requireNamespace("webshot",quietly=TRUE)
    if(webshot:::is_windows())
        stop("Not windows ready yet")

    warning("This function does not capture incremental slides!")
     
    ht<-XML::htmlTreeParse(filein)
    nslides <- length(XML::getNodeSet(XML::xmlRoot(ht),"//section"))

    tmpdir <- tempdir()

    filenames <- lapply(as.list(1:nslides),
           function(i){
               na <- tempfile(fileext=".png")
               webshot::webshot(paste0(filein,"#",i),na,
                                vwidth=sizeFactor * 800,
                                vheight=sizeFactor * 600,
                                delay = delay)
               na
           })

    
    prog <- Sys.which("convert")

    if (prog == "")
        stop("convert not found in path")
    
    args <- c(unlist(filenames),fileout)
    system2(prog,args)
}
