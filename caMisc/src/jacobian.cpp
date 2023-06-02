#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <algorithm>

#include <vector>

typedef std::vector<double>::size_type size_type;


SEXP asSEXP2(std::vector<double> x){
  size_type n = x.size();
  SEXP r = PROTECT(Rf_allocVector(REALSXP,n));
  double* pv = REAL(r);
  for(size_type i = 0; i < n; ++i)
    pv[i] = x[i];
  UNPROTECT(1);
  return r;
}

SEXP asSEXP2(double x){
  SEXP r = PROTECT(Rf_allocVector(REALSXP,1));
  double* pv = REAL(r);
  pv[0] = x;
  UNPROTECT(1);
  return r;
}

std::vector<double> asVector2(SEXP x){
  size_type n = Rf_length(x);
  double* pv = REAL(x);
  std::vector<double> r(n,0.0);
  for(size_type i = 0; i < n; ++i)
    r[i] = pv[i];
  return r;
}

bool all_finite(std::vector<double> x){
  for(size_type i = 0; i < x.size(); ++i)
    if(!R_finite(x[i]))
      return false;
  return true;
}

bool any_notfinite(std::vector<double> x){
  for(size_type i = 0; i < x.size(); ++i)
    if(!R_finite(x[i]))
      return true;
  return false;
}

std::vector<double> prod(std::vector<double> x, double y){
  std::vector<double> r(x.size(),0.0);
  for(size_type i = 0; i < r.size(); ++i)
    r[i] = x[i] * y;
  return r;
}

std::vector<double> add(std::vector<double> x, double y){
  std::vector<double> r(x.size(),0.0);
  for(size_type i = 0; i < r.size(); ++i)
    r[i] = x[i] + y;
  return r;
}

std::vector<double> add(std::vector<double> x, std::vector<double> y){
  std::vector<double> r(x.size(),0.0);
  for(size_type i = 0; i < r.size(); ++i)
    r[i] = x[i] + y[i];
  return r;
}


std::vector<double> subtract(std::vector<double> x, double y){
  std::vector<double> r(x.size(),0.0);
  for(size_type i = 0; i < r.size(); ++i)
    r[i] = x[i] - y;
  return r;
}

std::vector<double> subtract(std::vector<double> x, std::vector<double> y){
  std::vector<double> r(x.size(),0.0);
  for(size_type i = 0; i < r.size(); ++i)
    r[i] = x[i] - y[i];
  return r;
}

typedef struct r_function {

  SEXP fcall;
  SEXP env;
  size_type nIn;
  size_type nOut;

  std::vector<double> operator()(std::vector<double> x){
    return this->operator()(asSEXP2(x));
  }

  std::vector<double> operator()(SEXP x){
    SEXP ans;
    SETCADR(fcall, x);    
    PROTECT(ans = Rf_duplicate(Rf_eval(fcall, env)));
    UNPROTECT(1);
    return asVector2(ans);
  }

} r_function, *Rfunction;


extern "C" {

  /*
    Implementation of:
    C.J.F. Ridders,
    Accurate computation of F′(x) and F′(x) F″(x),
    Advances in Engineering Software (1978),
    Volume 4, Issue 2,
    1982,
    Pages 75-76,
    ISSN 0141-1195,
    https://doi.org/10.1016/S0141-1195(82)80057-0.
  */

  SEXP jacobian(SEXP fn, SEXP par, SEXP rho, SEXP maxit, SEXP h, SEXP tolerance){
    SEXP ans;


    // Check input
    //// fn must be a function
    if(!Rf_isFunction(fn))
      Rf_error("fn must be a function");
    ///// par is a numeric vector
    if(!(Rf_isReal(par) && Rf_length(par) > 0))
      Rf_error("par must be a non-zero length numeric vector");
    ///// rho is an environment
    if(!Rf_isEnvironment(rho))
      Rf_error("rho must be an environment");
    ///// maxit is an integer
    if(!(Rf_isInteger(maxit) || Rf_isReal(maxit)) && Rf_length(maxit) == 1)
      Rf_error("maxit must be a single integer");
    ///// h is a vector of the same length as par
    if(!(Rf_isReal(h) && Rf_length(h) == Rf_length(par)))
      Rf_error("h must be a numeric vector with the same length as par");
    //// tolerance is a scalar
    if(!(Rf_isReal(tolerance) && Rf_length(tolerance) == 1))
      Rf_error("tolerance must be a numeric scalar");

    
    Rfunction RF = (Rfunction) R_alloc(1, sizeof(r_function));
    RF->fcall = PROTECT(Rf_lang2(fn, R_NilValue));
    RF->env = rho;
    RF->nIn = Rf_length(par);

    std::vector<double> funres = RF->operator()(par);
    // Check if values are finite
    for(size_type i = 0; i < funres.size(); ++i)
      if(any_notfinite(funres))
	Rf_error("Function evaluation returned non-finite values");
    
    RF->nOut = funres.size();
    
    ans = PROTECT(Rf_allocVector(VECSXP, RF->nIn+1));
    SET_VECTOR_ELT(ans, 0, asSEXP2(funres));

    std::vector<double> dpar = asVector2(par);

    std::vector<double> h0 = asVector2(h);
    
    size_type ntab = Rf_asInteger(maxit);
    double tol = Rf_asReal(tolerance);
    // Loop over parameters
    for(size_type p = 0; p < RF->nIn; ++p){
      std::vector<double> dir(RF->nIn,0.0);
      dir[p] = 1.0;
      
      double err = R_PosInf;
      double hh = 2.0 * h0[p];
      
      std::vector<double> v1(RF->nIn,0.0);
      std::vector<double> v2(RF->nIn,0.0);

      do {
	hh /= 2.0;
	v1 = RF->operator()(add(dpar, prod(dir, hh)));
	v2 = RF->operator()(subtract(dpar,prod(dir,hh)));
      }while(any_notfinite(v1) || any_notfinite(v2));

      //array<double> tab(RF->nOut, ntab, ntab);
      std::vector<std::vector<std::vector<double> > > tab(ntab);
      for(size_type i = 0; i < tab.size(); ++i){
	tab[i] = std::vector<std::vector<double> >(ntab);
	for(size_type j = 0; j < tab.size(); ++j){
	  tab[i][j] = std::vector<double>(RF->nOut,0.0);
	}
      }

      //tab.col(0).col(0) = (v1 - v2) / (2.0 * hh));     
      std::vector<double> tmp0 = prod(subtract(v1, v2), 1.0 / (2.0 * hh));
      for(size_type ii = 0; ii < RF->nOut; ++ii)
	tab[0][0][ii] = tmp0[ii];

      for(size_type i = 1; i < ntab; ++i){
	do {
	  hh /= 2.0;
	  v1 = RF->operator()(add(dpar, prod(dir, hh)));
	  v2 = RF->operator()(subtract(dpar,prod(dir,hh)));
 	}while(any_notfinite(v1) || any_notfinite(v2));
	
	//tab.col(i).col(0) = (v1 - v2) / (2.0 * hh);
	std::vector<double> tmpi = prod(subtract(v1, v2), 1.0 / (2.0 * hh));
	for(size_type ii = 0; ii < RF->nOut; ++ii)
	  tab[0][i][ii] = tmp0[ii];

	double f = 1.0;
	for(size_type m = 1; m <= i; ++m){
	  f *= 4.0;
	  //tab.col(i).col(m) = ((vector<double>)tab.col(i).col(m-1) * f - (vector<double>)tab.col(i-1).col(m-1)) / (f - 1.0);
	  double errtmp = 0;
	  for(size_type ii = 0; ii < RF->nOut; ++ii){
	    tab[m][i][ii] = (tab[m-1][i][ii] * f - tab[m-1][i-1][ii]) / (f-1.0);
	    double et0 = std::max(std::fabs(tab[m][i][ii] - tab[m-1][i][ii]),
				  std::fabs(tab[m][i][ii] - tab[i-1][i-1][ii]));
	    if(et0 > errtmp)
	      errtmp = et0;
	  }
	  // double errtmp = std::max((tab.col(i).col(m) - tab.col(i).col(m-1)).abs().maxCoeff(),
	  // 			   (tab.col(i).col(m) - tab.col(i-1).col(i-1)).abs().maxCoeff());
	  if(errtmp < err){
	    SET_VECTOR_ELT(ans, p+1, asSEXP2(tab[m][i]));
	    err = errtmp;
	    if(err < tol)
	      goto endloop;
	  }
	}
	double ex = 0;
	for(size_type ii = 0; ii < RF->nOut; ++ii){
	  double ex = std::max(ex, std::fabs(tab[i][i][ii] - tab[i-1][i-1][ii]));
	}
	if(ex > 2.0 * err)
	  break;
	//if((tab.col(i).col(i) - tab.col(i-1).col(i-1)).abs().maxCoeff() > 2.0 * err)
	// break;
      }
    endloop:
      Rf_setAttrib(VECTOR_ELT(ans,p+1), Rf_install("error"), asSEXP2(err));
    }    
    
    UNPROTECT(2);
    return ans;
  
  }

}
