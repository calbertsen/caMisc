#include <Eigen/Cholesky>
#include <Eigen/Dense>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

//#include "common.hpp"
//#include "sampler.hpp"

using namespace Eigen;

extern "C" {
  SEXP mhmcmc(SEXP par, SEXP fn, SEXP he, SEXP envir, SEXP NN){
    //par must be numeric
    if(!isNumeric(par)) error("‘par’ must be numeric");
    if(!isFunction(fn)) error("‘fn’ must be a function");
    if(!isFunction(he)) error("‘he’ must be a function");
    if(!isEnvironment(envir)) error("‘envir’ should be an environment");
    if(!isInteger(NN)) error("‘NN’ must be an integer");
    
    //N must be integer

  int N = asInteger(NN);

  //Rprintf("Preparing variables...\n");
    int n = length(par);
    double ar = 0.0;
    MatrixXd res(n,N+1);

    res.col(0) = asVector(par);
    //double cf = 1.0; //0.01;
    SEXP R_fcall = PROTECT(lang2(he, R_NilValue));
    SETCADR(R_fcall, par);
    MatrixXd sdptmp = asMatrix(eval(R_fcall, envir));
    //Rprintf("Inverting Hessian...\n");
    MatrixXd sdp = sdptmp.inverse();
    UNPROTECT(1);
    
    GetRNGstate();

    for(int i = 1; i <= N; i++){
      //Rprintf("Sampling nr %f...\n", i);
      VectorXd x = res.col(i-1);
      VectorXd Y = rmvnorm(1,x,sdp);
      SEXP f_callx = PROTECT(lang2(fn, R_NilValue));
      SETCADR(f_callx, asSEXP(x));
      double fnx = REAL(eval(f_callx, envir))[0];
      //SEXP f_cally = PROTECT(lang2(fn, R_NilValue));
      SETCADR(f_callx, asSEXP(Y));
      double fny = REAL(eval(f_callx, envir))[0];
      UNPROTECT(1);

      double tmp = exp(fnx-fny); //Proposal is symmetric, function is negative log likelihood

      double u = unif_rand();
      //res.row(i) = x+(Y-x)*(a>u ? 1 : 0);
      res.col(i) = x+(Y-x)*(tmp>u ? 1.0 : 0.0);
      ar += (tmp>u ? 1 : 0);
    }
  PutRNGstate();
  
  //Make return object pretty, e.g. add dimnames
  // Maybe return a list with additional information like acceptance rate?

  MatrixXd resOut = res.transpose();
  return(asSEXP(resOut));

}
}
