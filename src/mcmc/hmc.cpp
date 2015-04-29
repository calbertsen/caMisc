#include <Eigen/Cholesky>
#include <Eigen/Dense>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

using namespace Eigen;

extern "C" {
  SEXP hmc(SEXP par, SEXP fn, SEXP gr, SEXP envir, SEXP NN, SEXP eps, SEXP LL){
    // Check input


    //Rprintf("Preparing variables...\n");
    int N = asInteger(NN);
    int L = asInteger(LL);

    int n = length(par);
    MatrixXd res(n,N+1);
    res.col(0) = asVector(par);
    

    MatrixXd sdp = MatrixXd(n,n);
    sdp.setZero();
    for(int qq = 0; qq < n; ++qq)
      sdp(qq,qq) = 1.0;
    VectorXd zerv(n);
    zerv.setZero();
    double epsi = REAL(eps)[0];
    
    GetRNGstate();
    for(int i = 1; i <= N; ++i){ // Loop over number of samples
      VectorXd r = rmvnorm(1,zerv,sdp);
      VectorXd rOld = r;
      VectorXd theta = res.col(i-1);
      VectorXd thetOld = theta;
       
      // Leapfrog
      for(int j = 0; j < L; ++j){
	SEXP f_callx = PROTECT(lang2(gr, R_NilValue));
	SETCADR(f_callx, asSEXP(theta));
        VectorXd grval = asVector(eval(f_callx, envir));
	r += 0.5*epsi*grval;
	theta += epsi*r;
	SETCADR(f_callx, asSEXP(theta));
        grval = asVector(eval(f_callx, envir));
	r += 0.5*epsi*grval;
	UNPROTECT(1);
      }

      SEXP f_call = PROTECT(lang2(fn, R_NilValue));
      SETCADR(f_call, asSEXP(thetOld));
      double fnto = REAL(eval(f_call, envir))[0];
      SETCADR(f_call, asSEXP(theta));
      double fnt = REAL(eval(f_call, envir))[0];
      UNPROTECT(1);

      // Inner product of r vectors
      double rr = r.squaredNorm();
      double rrOld = rOld.squaredNorm();

      double tmp = exp(fnt - 0.5*rr)/exp(fnto - 0.5*rrOld);


      double u = unif_rand();
      //res.row(i) = x+(Y-x)*(a>u ? 1 : 0);
      res.col(i) = thetOld+(theta-thetOld)*(tmp>u ? 1.0 : 0.0);
	
    }
    PutRNGstate();
  
    //Make return object pretty, e.g. add dimnames
    // Maybe return a list with additional information like acceptance rate?
    
    MatrixXd resOut = res.transpose();
    return(asSEXP(resOut));
  
  }
}
