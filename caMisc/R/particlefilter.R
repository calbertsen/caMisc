#########################
## PF bootstrap
#########################
##' Function to run a bootstrap particle filter for user defined model.
##'
##' The G function should be of the form G <- function(t,X) and return a numeric vector
##' of simulate values from \eqn{X_t | X_{t-1} = X}. For \eqn{t = 1} the function will be called as G(1,NULL).
##' The M function should be of the form M <- function(t, X) and return the density of the observation at time t given the latent state \eqn{X_t}
##' @title Bootstrap particle filter
##' @param N Number of particles
##' @param T Number of time steps
##' @param G Function to simulate hidden states (of dimension p) given previous (See Details)
##' @param M Function to calculate log-likelihood of data for a given particle
##' @param F Do not use!
##' @param envir Environment the functions should be evaluated in (containing data and parameters)
##' @param seed Random seed to start the filter
##' @return A list with a p x N x T array X containing the simulated particles (X) and a vector of length T with the negative log-likelihood contribution at each time point.
##' @author Christoffer Moesgaard Albertsen \email{cmoe@@aqua.dtu.dk}
##' @examples
##' ## Univariate example
##' env <- new.env()
##' env$pars <- list(sdobs = 0.4, sdstate = 0.3)
##' local({xx <- cumsum(rnorm(100,0,pars$sdstate))},env)
##' local({dat <- xx + rnorm(100,0,pars$sdobs)},env)
##' G <- function(t,X){
##'             if(t == 1){
##'                return(rnorm(1,0,pars$sdstate))
##'             }else{
##'                return(rnorm(1,X,pars$sdstate))
##'             }
##' }
##' M <- function(t,X){
##'     return(dnorm(dat[t],X,pars$sdobs, TRUE))
##' }
##' pest <- particlefilter(N = 1000, T = 100,
##'                  G=G,M=M,
##'                  envir=env, seed=1)
##' \dontrun{
##' plot(env$dat)
##' lines(apply(pest$X[1,,],2,mean),col="red")
##' lines(apply(pest$X[1,,],2,mean) + 2 * apply(pest$X[1,,],2,sd),col="red",lty=2)
##' lines(apply(pest$X[1,,],2,mean) - 2 * apply(pest$X[1,,],2,sd),col="red",lty=2)
##' lines(env$xx)
##' }
##'
##' ## Bivariate example
##' env <- new.env()
##' env$pars <- list(sdobs = 0.4, sdstate = 0.3)
##' local({xx <- cbind(cumsum(rnorm(100,0,pars$sdstate)),
##'              cumsum(rnorm(100,0,pars$sdstate)))},env)
##' local({dat <- xx + matrix(rnorm(2 * 100,0,pars$sdobs),ncol=2)},env)
##' G <- function(t,X){
##'             if(t == 1){
##'                return(rnorm(2,0,pars$sdstate))
##'             }else{
##'                return(rnorm(2,X,pars$sdstate))
##'             }
##' }
##' M <- function(t,X){
##'     return(sum(dnorm(dat[t,],X,pars$sdobs,TRUE)))
##' }
##' pest <- particlefilter(N = 1000, T = 100,
##'                  G=G,M=M,
##'                  envir=env, seed=1)
##' \dontrun{
##' layout(cbind(1,c(2,3)))
##' plot(env$dat)
##' lines(env$xx)
##' lines(apply(pest$X,c(3,1),mean),col="red")
##' plot(env$dat[,1])
##' lines(apply(pest$X[1,,],2,mean),col="red")
##' lines(apply(pest$X[1,,],2,mean) + 2 * apply(pest$X[1,,],2,sd),col="red",lty=2)
##' lines(apply(pest$X[1,,],2,mean) - 2 * apply(pest$X[1,,],2,sd),col="red",lty=2)
##' lines(env$xx[,1])
##' plot(env$dat[,2])
##' lines(apply(pest$X[2,,],2,mean),col="red")
##' lines(apply(pest$X[2,,],2,mean) + 2 * apply(pest$X[2,,],2,sd),col="red",lty=2)
##' lines(apply(pest$X[2,,],2,mean) - 2 * apply(pest$X[2,,],2,sd),col="red",lty=2)
##' lines(env$xx[,2])
##' }
##' @export
particlefilter <- function(N, 
                     T,
                     G,
                     M,
                     F = NULL,
                     envir = .GlobalEnv,
                     seed = NULL){

    ## Set correct environment for G and M (only applies locally)
    environment(G) <- envir
    environment(M) <- envir
    if(!is.null(F))
        environment(F) <- envir
    
    ## Set seed
    if(!exists(".Random.seed"))
        set.seed(NULL) 
    oldseed <- .Random.seed
    set.seed(seed)

    ## Simulate X_0 from G
    x1 <- replicate(N, G(1, NULL))
    
    p <- length(x1)/N                  # Get dimension of latent variables
    X <- array(NA,dim=c(p,N,T))        # Container for particles
    XA <- array(NA,dim=c(p,N,T))        # Container for resampled particles
    w <- rep(NA,N)                     # Unnormalized weights
    W <- matrix(NA,ncol=T,nrow=N)      # normalized weights
    L <- rep(NA,T)                     # likelihood of observed data
    GR <- list()                        # gradient of nll
    A <- matrix(NA,ncol=N,nrow=T)      # Ancestor indices

    ## Time t = 1
    X[,,1] <- x1

    w <- apply(X[,,1,drop=FALSE],2,function(x) M(1, x))
    wm <- max(w)
    logsw <- wm + log(sum(exp(w - wm)))
    W[,1] <- exp(w - logsw)
    L[1] <- - logsw + log(N)
    ## Iteration over time
    if(!is.null(F))
        GR[[1]] <- sapply(1:N,function(i) W[i,1]*F(1,X[,i,1],NULL))

    for(i in 2:T){
        ## a) sample index of ancestor
        A[i-1,] <- sample(x = 1:N,
                        size = N,
                        replace = TRUE,
                        prob = W[,i-1])
        XA[,,i-1] <- X[,A[i-1,],i-1]
        
        ## b) sample X_t^n
        X[,,i] <- apply(X[,A[i-1,],i-1,drop=FALSE],2,function(x) G(i, x))
        
        ## c) Compute weights
        w <- apply(X[,,i,drop=FALSE],2,function(x) M(i, x))
        wm <- max(w)
        logsw <- wm + log(sum(exp(w - wm)))
        W[,i] <- exp(w - logsw)
        L[i] <- - logsw + log(N)
        if(!is.null(F))
            GR[[i]] <- sapply(1:N,function(k) W[k,i]*F(i,X[,k,i],X[,A[i-1,k],i-1]))


    }
    A[T,] <- sample(x = 1:N,
                    size = N,
                    replace = TRUE,
                    prob = W[,T])
    XA[,,T] <- X[,A[T,],T]

    
    .Random.seed <<- oldseed 
    list(X=X,XA = XA,L=L,W=W,GR=GR)
}


