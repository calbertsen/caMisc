#include <Eigen/Cholesky>
#include <Eigen/Dense>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

//#include "common.hpp"
#include "sampler.hpp"

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
  
  //Make return object pretty


  return(asSEXP(res.transpose()));

}
}
  /*
// [[Rcpp::export]]
NumericMatrix mcmcRW_MH(List obj, NumericVector par, int N, double cf = 1.0){
  Function fn = obj["fn"];
  Function he = obj["he"];
  NumericVector objpar = obj["par"];
  CharacterVector nam = objpar.attr("names");
  int n = par.size();
  double ar = 0.0;
  NumericMatrix res(N,n);
  Rcpp::List dimnms = Rcpp::List::create(res.attr("rownames"),
      nam);
  res.attr("dimnames") = dimnms;
  res.row(0) = par;
  //double cf = 1.0; //0.01;
  NumericVector sdp = sqrtdiag(solve(he(par)));
  //NumericVector sdp = (objpar-objpar)+1.0;
  for(int i = 1; i < N; i++){
    NumericVector x = res.row(i-1);
    NumericVector Y = rnorm(n,x,cf*sdp);
    double fny = Rcpp::as<double>(fn(Y));
    double fnx = Rcpp::as<double>(fn(x));
    //double tmp = exp(fnx-fny)*prod(dnormc(x,Y,cf*sdp))/prod(dnormc(Y,x,cf*sdp));
    double tmp = exp(fnx-fny); //Proposal is symmetric
    //double a = std::min(tmp,1.0);
    double u = runif(1)[0];
    //res.row(i) = x+(Y-x)*(a>u ? 1 : 0);
    res.row(i) = x+(Y-x)*(tmp>u ? 1.0 : 0.0);
    ar += (tmp>u ? 1 : 0);*/
    /*if(ar/i>0.4 && i>20){
      //cf = cf*2*rnorm(1,1,0.01)[0];
      std::cout<<"Adjusting - too high "<<i<<" cf = "<<cf<<std::endl;
      if(cf==0){cf = 0.01;}
    }else if(ar/i<0.1 && i>20){
      //cf = cf*0.5*rnorm(1,1,0.01)[0];
      std::cout<<"Adjusting - too low "<<i<<" cf = "<<cf<<std::endl;
      if(cf==0){cf = 0.01;}
    }
    if(round(i/100.0)==i/100.0){
      std::cout<<i<<std::endl;
      }*/
  /*  }
  Rcpp::Rcout<<"Acceptance rate: "<<ar/(N-1)<<std::endl;
  return(res);
}
*/
