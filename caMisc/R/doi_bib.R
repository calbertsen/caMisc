
##' @importFrom utils URLdecode
parseBibString <- function(str){
    txt <- gsub(",$","",strsplit(str,"\n")[[1]])
    tindx <- grep("=",txt)
    splitList <- sapply(txt[tindx],strsplit,split="=")
    name <- lapply(splitList,function(x)gsub("[[:blank:]]","",x[1]))
    val <- lapply(splitList,function(x)gsub("(^\\{|\\}$)","",gsub("^[[:blank:]]","",paste(x[-1],collapse="="))))
    names(val) <- tolower(unlist(name))
    if(any(name=="url")){
        val$url <- utils::URLdecode(val$url)
    }
    if(any(name=="author")){
        a <- val$author
        if(grepl(",",a)){
            aspl <- strsplit(a," and ")[[1]]
            asw <- sapply(aspl,function(an){
                paste(strsplit(an,", ")[[1]][2:1],collapse=" ")
            })
            val$author <- paste(asw,collapse=" and ")
        }
    }
    bibtype <- paste(tail(head(strsplit(regmatches(txt[1],regexpr("^@.*\\{",txt[1])),"")[[1]],-1),-1),collapse="")
    if(bibtype == "article" & all(names(val) != "journal")){
        bibtype <- "misc"
    }
    key <- paste(tail(strsplit(regmatches(txt[1],regexpr("\\{.*$",txt[1])),"")[[1]],-1),collapse="")
    args <- c(bibtype = tolower(bibtype), key = key, val)
    return(do.call("bibentry",args))
}

##' @export
##' @importFrom RCurl basicTextGatherer curlPerform
doi2bib <- function(doi){
    h <- RCurl::basicTextGatherer()
    RCurl::curlPerform(url = paste0("dx.doi.org/",doi),
                       httpheader=c(Accept="application/x-bibtex"),
                       followlocation=TRUE,
                       writefunction = h$update,
                       verbose = FALSE)
    parseBibString(paste(h$value(),"\n\n"))
}

##' @export
##' @importFrom jsonlite fromJSON
##' @importFrom RCurl basicTextGatherer curlPerform
isbn2bib <- function(isbn){
    dat <- jsonlite::fromJSON(paste0("https://www.googleapis.com/books/v1/volumes?q=isbn+",gsub("[^[:digit:]_]","",isbn)),simplifyVector=FALSE)
    if(dat$totalItems > 1)
        warning("Search returned more than one book only first one used")
    if(dat$totalItems == 0)
        stop("Search returned no books")
    id <- dat$items[[1]]$id
    h <- RCurl::basicTextGatherer()
    RCurl::curlPerform(url = paste0("https://books.google.dk/books/download/?output=bibtex&id=",id),
                       writefunction = h$update,
                       verbose = FALSE)
    parseBibString(paste(h$value(),"\n\n"))
}

##' @export
##' @importFrom utils citation
rpkg2bib <- function(pkg,citationNumber = 1){
    cite <- utils::citation(pkg)
    if(length(cite) < citationNumber | citationNumber < 1){
        warning("Wrong citationNumber, first citation used.")
        citationNumber <- 1
    }
    cite <- cite[[citationNumber]]
    cite$key <- paste0(cite$author[[1]]$family,"_",cite$year)
    citeString <- paste(c(as.character(utils::toBibtex(cite)),"\n"),collapse="\n")
    return(parseBibString(citeString))
}

##' @export
##' @importFrom XML xmlToList xmlParse
##' @importFrom utils toBibtex
arxiv2bib <- function(aid){
    dat <- XML::xmlToList(XML::xmlParse(paste0("http://export.arxiv.org/api/query?id_list=",aid)))$entry
    if(any("doi"==names(dat))){
        cite <- doi2bib(dat$doi)
        cite$archivePrefix = "arXiv"
        cite$eprint = tail(strsplit(dat$id,"/")[[1]],1)
        cite$primaryClass = dat$primary_category["term"]
        return(cite)                    
    }else{
        cite <- list(title = gsub("[[:space:]]+"," ",dat$title),
                     author = paste(unlist(dat[which(names(dat)=="author")]),collapse=" and "),
                     year = strftime(as.POSIXct(dat$updated),"%Y"),
                     url = dat$id,
                     archivePrefix = "arXiv",
                     eprint = tail(strsplit(dat$id,"/")[[1]],1),
                     primaryClass = dat$primary_category["term"],
                     note = "Preprint")
        if(any("comment" == names(dat)))
            cite$note <- paste0(cite$note,". ",dat$comment)
        if(any("doi"==names(dat)))
            cite$doi <- dat$doi
        fam <- tail(strsplit(strsplit(cite$author," and ")[[1]]," ")[[1]],1)
        cite$key <- paste0(fam,"_",cite$year)
        cite$bibtype <- "misc"
        citeFin <- do.call("bibentry",cite)
        citeString <- paste(c(as.character(utils::toBibtex(citeFin)),"\n"),collapse="\n")
        return(parseBibString(citeString))
    }
}    


##' @export
##' @importFrom jsonlite fromJSON
##' @importFrom utils URLencode
citation2doi <- function(txt){
    dat <- jsonlite::fromJSON(paste0("http://search.crossref.org/dois?q=",utils::URLencode(txt)),simplifyDataFrame=FALSE)
    if(length(dat) > 1)
        warning("More than one result returned. Using first result.")
    return(sub("http://dx.doi.org/","",dat[[1]]$doi))

}

##' @importFrom utils read.csv write.csv
get_LTWA <- function(){
    fil <- file.path(tempdir(), "ltwa.csv")
    if(file.exists(fil)){
        ltwa <- read.csv(fil, sep = ",", stringsAsFactors = FALSE)
    }else{
        ltwa <- read.csv("https://www.issn.org/wp-content/uploads/2021/07/ltwa_20210702.csv", sep = ";", stringsAsFactors = FALSE)
        ltwa[,1] <- sub("(^-|-$)","[[:alpha:]]+",ltwa[,1])
        write.csv(ltwa, file = fil,row.names = FALSE)
    }
    ltwa
}

subjournal <- function(wd,subdat){
    if(tolower(wd) %in% c("of","and","the","on","an","a"))
        return("")
    indx <- which(sapply(paste0("^",tolower(subdat[,1]),"$"),grepl,x=tolower(wd)))
    if(length(indx) == 0)
        return(wd)
    if(length(indx) > 1)
        warning("More than one match")
    if(unname(subdat[indx[1],2] == "n.a."))
        return(wd)
    return(unname(subdat[indx[1],2]))
}

##' @export
journal2abbr <- function(journal,subdat = get_LTWA()){
    val <- sapply(strsplit(journal,"[[:blank:]]")[[1]],subjournal,subdat=subdat)
    oval <- sapply(val,function(x)if(nchar(x)==0){
                                            return("")
                                        }else if(nchar(x)==1){
                                            return(toupper(x))
                                        }else{
                                            return(paste0(toupper(substr(x,1,1)),substr(x,2,nchar(x))))
                                            })
    oval <- paste(oval[nchar(oval)>0],collapse=" ")
    return(oval)
}
