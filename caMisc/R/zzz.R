#' @useDynLib caMisc, .registration = TRUE, .fixes = "C_"

## .onLoad <- function(libname,pkgname){
##     ##packageStartupMessage("Loading compiled code...\n")
##     library.dynam("caMisc", pkgname, libname)
## }

## .onUnload <- function(libpath){
##     library.dynam.unload("caMisc", libpath)
## }
