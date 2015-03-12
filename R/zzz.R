

.onLoad <- function(libname,pkgname){
    cat("Loading compiled code...\n")
    library.dynam("unnamedR", pkgname, libname)
}
