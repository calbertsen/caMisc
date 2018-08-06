#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern "C" {

  SEXP fagmmcmc(SEXP par, SEXP fn, SEXP mu, SEXP sdlist, SEXP envir, SEXP NN);
  SEXP hmc(SEXP par, SEXP fn, SEXP gr, SEXP envir, SEXP NN, SEXP eps, SEXP LL);
  SEXP mhmcmc(SEXP par, SEXP fn, SEXP he, SEXP envir, SEXP NN);

  //SEXP NelderMead(SEXP x, SEXP fn, SEXP epsilon, SEXP delta, SEXP keep, SEXP maxiter, SEXP envir)
  
  SEXP rmvnorm(SEXP n,SEXP mu, SEXP sigma);
  SEXP rmvlnorm(SEXP n,SEXP logmu, SEXP sigma);
  SEXP rlogistic(SEXP n,SEXP logimu, SEXP sigma);
  SEXP rcategory(SEXP n,SEXP probs);
  SEXP rmvgaussmix(SEXP n, SEXP mu, SEXP sigma, SEXP alpha);
  SEXP rmvt(SEXP n,SEXP mu, SEXP sigma, SEXP df);
  
#define CALLDEF(name,n) {#name, (DL_FUNC) &name, n}
  
  static const
  R_CallMethodDef callMethods[] = {

    CALLDEF(fagmmcmc,6),
    CALLDEF(hmc,7),
    CALLDEF(mhmcmc,5),
    //CALLDEF(NelderMead,7),
    CALLDEF(rmvnorm,3),
    CALLDEF(rmvlnorm,3),
    CALLDEF(rlogistic,3),
    CALLDEF(rcategory,2),
    CALLDEF(rmvgaussmix,4),
    CALLDEF(rmvt,4),
    
    {NULL,NULL,0}
  };

  void R_init_caMisc(DllInfo *info)
  {
    /* Register the .C and .Call routines.
       No .Fortran() or .External() routines,
       so pass those arrays as NULL.
    */
    R_registerRoutines(info,
		       NULL, callMethods,
		       NULL, NULL);
    R_useDynamicSymbols(info, (Rboolean)FALSE);
  }


}
