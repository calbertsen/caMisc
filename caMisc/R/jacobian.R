##' Calculate jacobian of a function
##'
##' @param func function
##' @param x parameter values
##' @param h step size
##' @param ... passed to func
##' @return jacobian matrix
##' @useDynLib caMisc, .registration = TRUE, .fixes = "C_"
##' @export
jacobian <- function(func, x,
                     h = abs(1e-04 * x) + 1e-04 * (abs(x) < sqrt(.Machine$double.eps/7e-07)),
                     ## 0.1 * 10^floor(log10(abs(x))) + 1e-4,
                     ...){
    rho <- environment(func)
    if(!is.environment(rho))
        rho <- globalenv()
    r <- .Call(C_jacobian,
               function(x)func(x,...),
               x,
               rho,
               30L,
               h,##abs(1e-4 * x) + 1e-4 * (abs(x) < 1e-8),
               1e-12)
    do.call("cbind",r[-1])
}
##' Calculate gradient of a function
##'
##' @param func function
##' @param x parameter values
##' @param h step size
##' @param ... passed to func
##' @return gradient vector
##' @export
grad <- function(func, x,
                 h = abs(1e-04 * x) + 1e-04 * (abs(x) < sqrt(.Machine$double.eps/7e-07)),
                 ##0.1 * 10^floor(log10(abs(x))) + 1e-4,
                 ...){
    rho <- environment(func)
    if(!is.environment(rho))
        rho <- globalenv()
    r <- .Call(C_jacobian,
               function(x)func(x,...),
               x,
               rho, 
               30L,
               h,##abs(1e-4 * x) + 1e-4 * (abs(x) < 1e-8),
               1e-12)
    v <- do.call("cbind",r[-1])
    if(nrow(v) == 1)
        return(as.vector(v))
    return(diag(v))         
}
