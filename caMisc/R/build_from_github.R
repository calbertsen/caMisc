packageInstalled <- function(pkg) tryCatch({file.exists(find.package(pkg))},error=function(e)return(FALSE),warning=function(e)return(FALSE))
packageVersionOK <- Vectorize(function(pkg,op,ver){
    packageInstalled(pkg) &&
                   (is.na(ver) ||
                    eval(parse(text=sprintf('packageVersion(pkg) %s package_version("%s")',op,ver))))
})

##' @importFrom jsonlite fromJSON
getDefaultRef <- function(repo){    
    jsonlite::fromJSON(sprintf("https://api.github.com/repos/%s",repo),simplifyDataFrame=FALSE)$default_branch
    ##suppressWarnings(L <- strsplit(readLines(sprintf("https://api.github.com/repos/%s",repo)),",")[[1]])
    ##gsub('(^.+:\\")(.+)(\\"$)',"\\2",L[grepl("default_branch",L)])
}

##' @importFrom jsonlite fromJSON
getGithubURL <- function(repo,
                         ref,
                         https = TRUE){
    baseURL <- sprintf("%s://codeload.github.com",ifelse(https,"https","http"))
    a <- jsonlite::fromJSON(sprintf("https://api.github.com/repos/%s/git/refs",repo),simplifyDataFrame=FALSE)
    allRefs <- sapply(a, function(x) x$ref)
    ## Check branch
    mRef <- grepl(sprintf("/%s$",ref),allRefs)
    if(any(mRef)){
        ref <- allRefs[mRef][1]
    }
    return(sprintf("%s/%s/zip/%s",baseURL,repo,ref))
}

##' Download and build an R package from GitHub
##' 
##' @param repo GitHub user and repository separated by /
##' @param ref Reference to commit or branch. Default is master
##' @param subdir Path to subdir containing the package. Should be NULL if the package is in the top directory
##' @param buildArgs Character vector of arguments passed to R CMD build. Default is "--no-build-vignettes"
##' @return Nothing
##' @author Christoffer Moesgaard Albertsen
##' @importFrom utils download.file unzip
##' @importFrom tools Rcmd
##' @export
buildFromGithub <- function(repo,
                            ref,
                               subdir = NULL,
                               buildArgs = c("--no-build-vignettes")
                               ){
    topdir <- tempdir()
    splitRepo <- unlist(strsplit(repo,"/"))
    if(length(splitRepo) != 2)
        stop("repo must be of the form: github_user/repository")
    user <- splitRepo[1]
    urep <- splitRepo[2]
    if(missing(ref) || is.null(ref))
        ref <- getDefaultRef(repo)
    fil <- file.path(topdir,paste0(urep,"_",gsub("/","_",ref),".zip"))
    ##url <- sprintf("https://github.com/%s/archive/%s.zip",repo,ref)
    url <- getGithubURL(repo,ref)
    secureTrySuccess <- tryCatch({ utils::download.file(url,fil,quiet=TRUE, method = ifelse(capabilities("libcurl"),"libcurl","auto"), mode = "wb")}, error = function(e)1)
    if(secureTrySuccess == 1){
        utils::download.file(gsub("^https://","^http://",url),fil,quiet=TRUE, method = ifelse(capabilities("libcurl"),"libcurl","auto"), mode = "wb")
    }
    a <- utils::unzip(fil,exdir = topdir)
    descriptionPath <- a[grepl(paste0(subdir,"/DESCRIPTION"),a)]
    pkgPath <- gsub("/DESCRIPTION","",descriptionPath)
    tools::Rcmd(c("build",pkgPath,buildArgs))
}

##' Download, build, and install package dependencies
##'
##' @param descriptionPath Path to a DESCRIPTION file
##' @param buildArgs Character vector of arguments passed to R CMD build.
##' @param installArgs Character vector of arguments passed to R CMD INSTALL.
##' @param dependencies Character vector of dependency types to install ("Depends","Imports","LinkingTo","Enhances", or "Suggests")
##' @return Nothing
##' @author Christoffer Moesgaard Albertsen
##' @importFrom stats na.omit
##' @export
installDependencies <- function(descriptionPath,
                                buildArgs = c("--no-build-vignettes"),
                                installArgs = c(),
                                dependencies = c("Depends","Imports","LinkingTo")
                                ){
    if(length(dependencies) == 1 && (is.null(dependencies) || is.na(dependencies)))
        dependencies <- c()
    if(!is.character(dependencies) || !(any(dependencies %in% c("Depends","Imports","LinkingTo","Enhances","Suggests"))))
        stop('Dependencies must be a character vector with a subset of c("Depends","Imports","LinkingTo","Enhances","Suggests")')
    dcf <- read.dcf(descriptionPath,fields=c("Package","Version",dependencies,"Remotes"))
    if(length(dependencies) > 0){
        allDep <- unlist(strsplit(paste(stats::na.omit(dcf[,dependencies]),collapse=", "),",[[:space:]]*"))
        allDepTab <- unique(matrix(t(sapply(allDep,function(d){r <- strsplit(gsub("(\\(|\\))"," ",d),"[[:space:]]+")[[1]];c(r,NA,NA)[1:3]})),ncol = 3))
        allDepTab <- allDepTab[!(allDepTab[,1] %in% c("R","",NA)),,drop=FALSE]
        if(nrow(allDepTab)==0) return();
        allDepList <- split(as.data.frame(allDepTab,stringsAsFactors = FALSE),allDepTab[,1],drop=FALSE)
        ## Handle same package multiple times
        toInst <- lapply(allDepList,function(x){
            cat(x[1,1],"\n")
            pok <- packageVersionOK(x[,1],x[,2],x[,3])
            if(all(pok))
                return()
            indx <- which(!pok)
            y <- x[indx,]
            hasOp <- which(!is.na(y[,2]))
            repo <- paste0("cran/",x[1,1])
            if(length(hasOp)==0 || all(y[hasOp,2]==">="))
                return(c(repo,"master"))
            if(any(y[hasOp,2]=="=="))
                return(c(repo,y[which(y[,2]=="==")[1],3]))
            if(any(y[hasOp,2]=="<="))
                return(c(repo,min(y[which(y[,2]=="=="),3])))
            stop(paste("Sorry. I do not know how to install:",paste0(rownames(x),collapse=", ")))
        })
        if(length(toInst)==0) return();
        if(sum(!unlist(lapply(toInst,is.null))) > 0){
            toInst <- toInst[!unlist(lapply(toInst,is.null))]
            cat("\n\033[1;33mInstalling dependencies:",paste(names(toInst),collapse=", "),"\033[0;0m\n")
            invisible(lapply(toInst,
                            function(x) installFromGithub(repo = x[1],ref=x[2])))
        }
    }
}

##' Download, build, and install an R package from GitHub
##'
##' @param repo GitHub user and repository separated by /
##' @param ref Reference to commit or branch. Default is master
##' @param subdir Path to subdir containing the package. Should be NULL if the package is in the top directory
##' @param buildArgs Character vector of arguments passed to R CMD build.
##' @param installArgs Character vector of arguments passed to R CMD INSTALL.
##' @param dependencies Character vector of dependency types to install ("Depends","Imports","LinkingTo","Enhances", or "Suggests")
##' @return Nothing
##' @author Christoffer Moesgaard Albertsen
##' @importFrom utils download.file unzip
##' @export
installFromGithub <- function(repo,
                              ref,
                              subdir = NULL,
                              buildArgs = c("--no-build-vignettes"),
                              installArgs = c(),
                              dependencies = c("Depends","Imports","LinkingTo"),
                              https = TRUE
                              ){
    topdir <- tempdir()
    splitRepo <- unlist(strsplit(repo,"/"))
    if(length(splitRepo) != 2)
        stop("repo must be of the form: github_user/repository")
    user <- splitRepo[1]
    urep <- splitRepo[2]
    if(missing(ref) || is.null(ref))
        ref <- getDefaultRef(repo)
    fil <- file.path(topdir,paste0(urep,"_",gsub("/","_",ref),".zip"))
    ##url <- sprintf("https://github.com/%s/archive/%s.zip",repo,ref)
    url <- getGithubURL(repo,ref, https)
    utils::download.file(url,fil,quiet=TRUE, method = ifelse(capabilities("libcurl"),"libcurl","auto"), mode = "wb")
    a <- utils::unzip(fil,exdir = topdir)
    descriptionPath <- a[grepl(paste0(subdir,"/DESCRIPTION"),a)][1]
    cat("GithubRepo:",repo,"\n",file=descriptionPath,append=TRUE)
    cat("GithubRef:",ref,"\n",file=descriptionPath,append=TRUE)
    if(length(dependencies) > 0)
        installDependencies(descriptionPath,buildArgs,installArgs,dependencies)
    dcf <- read.dcf(descriptionPath,fields=c("Package","Version"))
    pkg <- paste0(paste0(dcf[1:2],collapse="_"),".tar.gz")
    pkgPath <- gsub("/DESCRIPTION","",descriptionPath)
    oldwd <- getwd()
    tryCatch({
        setwd(topdir)
        cat("\n\033[0;32mBuilding",dcf[1],"version",dcf[2],"\033[0;0m\n")
        tools::Rcmd(c("build",buildArgs,pkgPath))
        cat("\n\033[0;32mInstalling",dcf[1],"version",dcf[2],"\033[0;0m\n")
        tools::Rcmd(c("INSTALL",installArgs,pkg))
        },finally={setwd(oldwd)})
}
