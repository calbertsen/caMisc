#include <Eigen/Dense>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <iostream>


using namespace Eigen;


VectorXi findPsort(VectorXd fvals){
  VectorXi res(3); 		// P_{(0)}, P_{(n-1)} P_{(n)}
  res.setZero();

  if(fvals.size() == 1) return res;

  for(int i = 1; i < fvals.size(); ++i){
    if(fvals(i) < fvals(res(0)))
      res(0) = i; res(1) = i; res(2) = i;
  }
    
  for(int i = 0; i < fvals.size(); ++i){
    if(fvals(i) > fvals(res(2))){
      res(1) = res(2);
      res(2) = i;
    }else if(fvals(i) > fvals(res(1))){
      res(1) = i;
    }else{

    }
  }
  return res;
}

extern "C" {
  SEXP NelderMead(SEXP x, SEXP fn, SEXP epsilon, SEXP delta, SEXP keep, SEXP maxiter, SEXP envir){
    if(!isNumeric(x)) error("The initial values must be a numeric vector");
    if(!isFunction(fn)) error("‘fn’ must be a function");
    if(!isNumeric(epsilon) || length(epsilon) > 1) error("‘epsilon’ must be a scalar");
    if(!isEnvironment(envir)) error("‘envir’ should be an environment");

    SEXP f_call = PROTECT(lang2(fn, R_NilValue));

    double eps = asDouble(epsilon);
    std::cout << "eps: " << eps << std::endl << std::endl;
    double delt = asDouble(delta);
    int n = length(x);
    int kmax = asInteger(maxiter); 
    VectorXi indx(n);
    MatrixXd P(n,n+1);
    VectorXd fvals(n+1);
    int k = 0;
    bool kp = true; //asBoolean(keep);

    VectorXd x0 = asVector(x);

    SEXP pkeep, fkeep, ikeep, ans;

    if(kp){
      pkeep = PROTECT(allocVector(VECSXP,kmax));
      fkeep = PROTECT(allocVector(VECSXP,kmax));
      ikeep = PROTECT(allocVector(VECSXP,kmax));
      ans = PROTECT(allocVector(VECSXP,6));
    }else{
      ans = PROTECT(allocVector(VECSXP,3));
    }
    
    
    // Initialize P
    P.col(0) = x0;
    for(int i = 1; i < n+1; ++i)
      P.col(i) = x0 + delt * indexvec(n, i-1);

    // Evaluate fn(p_i)
    for(int i = 0; i < n+1; ++i){
      VectorXd tmp = P.col(i);
      SETCADR(f_call, asSEXP(tmp));
      fvals(i) = REAL(eval(f_call, envir))[0];
    }

    // Find best and two worst values
    indx = findPsort(fvals);

    while(((fvals(indx(2)) - fvals(indx(0))) > eps) && k <= kmax){

      if(kp){
	SET_VECTOR_ELT(pkeep, k, asSEXP(P));
	SET_VECTOR_ELT(fkeep, k, asSEXP(fvals));
	SET_VECTOR_ELT(ikeep, k, asSEXP(indx));
      }
      
      VectorXd c(n);
      for(int i = 0; i < n + 1; ++i)
	if(i != indx(2))
	  c += P.col(i)/((double)n);
      
      VectorXd xr = c + (c - P.col(indx(2)));
      SETCADR(f_call, asSEXP(xr));
      double fxr  = REAL(eval(f_call, envir))[0];

      if( fvals(indx(0)) < fxr && fxr < fvals(indx(1))){
	// x^{(r)} replaces p_{(n)}
	P.col(indx(2)) = xr;
	fvals(indx(2)) = fxr;
      }

      if( fxr < fvals(indx(0)) ){
	// Best trial replaces p_{(n)}
	VectorXd xe = c + 1.5 * (c - P.col(indx(2)));
	SETCADR(f_call, asSEXP(xe));
	double fxe  = REAL(eval(f_call, envir))[0];

	if(fxe < fxr){
	  P.col(indx(2)) = xe;
	  fvals(indx(2)) = fxe;
	}else{
	  P.col(indx(2)) = xr;
	  fvals(indx(2)) = fxr;
	}
      }

      if( fxr >= fvals(indx(1)) ){
	// Replace p_{(n)} by x^{(c)}
	VectorXd xc = c + 0.5 * (c - P.col(indx(2)));
	SETCADR(f_call, asSEXP(xc));
	double fxc  = REAL(eval(f_call, envir))[0];

	if( fxc < fxr && fxr < fvals(indx(2)) ){
	  P.col(indx(2)) = xc;
	  fvals(indx(2)) = fxc;	    
	}else if(fxc > fxr){
	  P.col(indx(2)) = xr;
	  fvals(indx(2)) = fxr;	    
	}else{
	  // Full contraction
	  for(int i = 0; i < n+1; ++i){
	    P.col(i) = 0.5 * (P.col(i) + P.col(indx(0)));
	    VectorXd tmp = P.col(i);
	    SETCADR(f_call, asSEXP(tmp));
	    fvals(i) = REAL(eval(f_call, envir))[0];
	  }
	  
	}
	
      }
      indx = findPsort(fvals);

      k++;

    }


    SET_VECTOR_ELT(ans,0,asSEXP(fvals(indx(0))));
    SET_VECTOR_ELT(ans,1,asSEXP((VectorXd)P.col(indx(0))));
    SET_VECTOR_ELT(ans,2,asSEXP(k));
    if(kp){
      SET_VECTOR_ELT(ans,3,pkeep);
      SET_VECTOR_ELT(ans,4,fkeep);
      SET_VECTOR_ELT(ans,5,ikeep);
    }

    
    if(kp){
      UNPROTECT(5);
    }else{
      UNPROTECT(2);
    }
    
    return ans;
  }

}
