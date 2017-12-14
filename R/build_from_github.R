##' Download and build an R package from GitHub
##'
##' @param repo GitHub user and repository separated by /
##' @param ref Reference to commit or branch. Default is master
##' @param subdir Path to subdir containing the package. Should be NULL if the package is in the top directory
##' @param buildArgs Character vector of arguments passed to R CMD build. Default is "--no-build-vignettes"
##' @return Nothing
##' @author Christoffer Moesgaard Albertsen
buildFromGithub <- function(repo,
                               ref = "master",
                               subdir = NULL,
                               buildArgs = c("--no-build-vignettes")
                               ){
    topdir <- tempdir()
    splitRepo <- unlist(strsplit(repo,"/"))
    if(length(splitRepo) != 2)
        stop("repo must be of the form: github_user/repository")
    user <- splitRepo[1]
    urep <- splitRepo[2]
    fil <- file.path(topdir,paste0(urep,"_",ref,".zip"))
    url <- sprintf("https://github.com/%s/archive/%s.zip",repo,ref)
    download.file(url,fil,quiet=TRUE)
    a <- utils::unzip(fil,exdir = topdir)
    descriptionPath <- a[grepl(paste0(subdir,"/DESCRIPTION"),a)]
    pkgPath <- gsub("/DESCRIPTION","",descriptionPath)
    tools::Rcmd(c("build",pkgPath,buildArgs), stderr = NULL)
}


##' Download, build, and install an R package from GitHub
##'
##' @param repo GitHub user and repository separated by /
##' @param ref Reference to commit or branch. Default is master
##' @param subdir Path to subdir containing the package. Should be NULL if the package is in the top directory
##' @param buildArgs Character vector of arguments passed to R CMD build.
##' @param installArgs Character vector of arguments passed to R CMD INSTALL.
##' @return Nothing
##' @author Christoffer Moesgaard Albertsen
installFromGithub <- function(repo,
                               ref = "master",
                               subdir = NULL,
                             buildArgs = c(),
                             installArgs = c()
                               ){
    topdir <- tempdir()
    splitRepo <- unlist(strsplit(repo,"/"))
    if(length(splitRepo) != 2)
        stop("repo must be of the form: github_user/repository")
    user <- splitRepo[1]
    urep <- splitRepo[2]
    fil <- file.path(topdir,paste0(urep,"_",ref,".zip"))
    url <- sprintf("https://github.com/%s/archive/%s.zip",repo,ref)
    download.file(url,fil,quiet=TRUE)
    a <- utils::unzip(fil,exdir = topdir)
    descriptionPath <- a[grepl(paste0(subdir,"/DESCRIPTION"),a)]
    dcf <- read.dcf(descriptionPath,fields=c("Package","Version"))
    pkg <- paste0(paste0(dcf,collapse="_"),".tar.gz")
    pkgPath <- gsub("/DESCRIPTION","",descriptionPath)
    oldwd <- getwd()
    tryCatch({
        setwd(topdir)
        cat("\n\033[0;32mBuilding",dcf[1],"version",dcf[2],"\033[0;0m\n")
        tools::Rcmd(c("build",pkgPath,buildArgs), stderr = NULL)
        cat("\n\033[0;32mInstalling",dcf[1],"version",dcf[2],"\033[0;0m\n")
        tools::Rcmd(c("INSTALL",pkg,installArgs), stderr = NULL)
        },finally={setwd(oldwd)})
}
