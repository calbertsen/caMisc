##' Create a package skeleton 
##'
##' @param pkg Name of the new package
##' @param path (existing) Directory of package
##' @return Used for side effects 
##' @author Christoffer Moesgaard Albertsen
##' @export
packageSkeleton <- function(pkg, path = file.path(".")){
    ## Create base dir
    dir.create(file.path(path,pkg))
    ## git init
    system("git init .")
    ## Create R subfolders
    dir.create(file.path(path,pkg,"R"))
    dir.create(file.path(path,pkg,"src"))
    dir.create(file.path(path,pkg,"inst"))
    dir.create(file.path(path,pkg,"vignettes"))
    dir.create(file.path(path,pkg,"data"))
    dir.create(file.path(path,pkg,"tests"))
    dir.create(file.path(path,pkg,"tests","tests"))
    ## Create Makefile
    l <- readLines(system.file("pkg_templates","Makefile.template",package="caMisc"))
    l[grepl("~pkgname~",l)] <- sub("~pkgname~",pkg,l[grepl("~pkgname~",l)])
    cat(l, file = file.path(path,"Makefile"), sep ="\n")
    ## Create Rbuildignore
    file.copy(system.file("pkg_templates","Rbuildignore",package="caMisc"),
              file.path(path,pkg,".Rbuildignore"))
    ## Create README
    l <- readLines(system.file("pkg_templates","README.Rmd",package="caMisc"))
    l[grepl("~pkgname~",l)] <- sub("~pkgname~",pkg,l[grepl("~pkgname~",l)])
    cat(l, file = file.path(path,"README.Rmd"), sep ="\n")
    ## Create gitignore
    file.copy(system.file("pkg_templates","gitignore",package="caMisc"),
              file.path(path,".gitignore"))
    ## Create travis
    l <- readLines(system.file("pkg_templates","travis.yml",package="caMisc"))
    l[grepl("~pkgname~",l)] <- sub("~pkgname~",pkg,l[grepl("~pkgname~",l)])
    cat(l, file = file.path(path,".travis.yml"), sep ="\n")
    ## Create description
    l <- readLines(system.file("pkg_templates","DESCRIPTION",package="caMisc"))
    l[grepl("~pkgname~",l)] <- sub("~pkgname~",Sys.Date(),l[grepl("~pkgname~",l)])
    l[grepl("~date~",l)] <- sub("~date~",pkg,l[grepl("~date~",l)])
    cat(l, file = file.path(path,pkg,"DESCRIPTION"), sep ="\n")
    ## Create tests
    file.copy(system.file("pkg_templates","000-testing_functions.R",package="caMisc"),
              file.path(path,pkg,"tests","tests","000-testing_functions.R"))
    l <- readLines(system.file("pkg_templates","test.R",package="caMisc"))
    l[grepl("~pkgname~",l)] <- sub("~pkgname~",pkg,l[grepl("~pkgname~",l)])
    cat(l, file = file.path(path,pkg,"tests",sprintf("run-%s-test.R",pkg)), sep ="\n")
    ## Create LICENSE
    cat("YEAR:",strftime(Sys.Date(),"%Y"),"\n",
        "COPYRIGHT HOLDER: Christoffer Moesgaard Albertsen",
        file = file.path(path,pkg,"LICENSE"))
}
