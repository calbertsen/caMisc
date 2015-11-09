#include <Eigen/Dense>

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

using namespace Eigen;

SEXP asSEXP(double x){
  SEXP y = PROTECT(allocVector(REALSXP,1));
  REAL(y)[0] = x;

  UNPROTECT(1);
  return y; 

}

SEXP asSEXP(VectorXd x){
  int n = x.size();
  SEXP y = PROTECT(allocVector(REALSXP,n));

  for(int i = 0; i < n; ++i)
    REAL(y)[i] = x[i];

  UNPROTECT(1);
  return y; 
}
SEXP asSEXP(VectorXi x){
  int n = x.size();
  SEXP y = PROTECT(allocVector(REALSXP,n));

  for(int i = 0; i < n; ++i)
    REAL(y)[i] = x[i];

  UNPROTECT(1);
  return y; 
}

SEXP asSEXP(MatrixXd x){
  int nr = x.rows();
  int nc = x.cols();
  SEXP y = PROTECT(allocMatrix(REALSXP,nr,nc));
  for(int i = 0; i < nr; ++i)
    for(int j = 0; j < nc; ++j)
      REAL(y)[i+nr*j] = x(i,j);

  UNPROTECT(1);
  return y;
}

MatrixXd asMatrix(SEXP x)
{

  if(!isMatrix(x))error("Element must be a matrix");

  int nr = nrows(x);
  int nc = ncols(x);
  MatrixXd y(nr,nc);

  for(int i = 0; i < nr; ++i)
    for(int j = 0; j < nc; ++j)
      y(i,j)=REAL(x)[i+nr*j];

  return y;
}

double asDouble(SEXP x){
  if(!(isNumeric(x) && length(x)==1))error("Element must be a numeric of length 1");
  return REAL(x)[0];
}

VectorXd asVector(SEXP x) {

  if(!isNumeric(x))error("Element must be a numeric vector");

  int n = length(x);
  Eigen::VectorXd y(n);

  for(int i = 0; i < n; ++i)
    y(i) = REAL(x)[i];

  return y;
}


SEXP getListElement(SEXP list, const char *str)
{
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);

    for (R_len_t i = 0; i < length(list); i++)
        if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
           elmt = VECTOR_ELT(list, i);
           break;
        }
    return elmt;
}

VectorXd cumsum(VectorXd x){
  VectorXd y(x.size());
  y(0) = x(0);
  if(x.size()==1) 
    return y;

  for(int i = 1; i < x.size(); ++i){
    y(i) = y(i-1)+x(i);
  }
  return y;
}


VectorXd indexvec(int n, int i){
  VectorXd e(n);
  e.setZero();
  e(i) = 1.0;
  return(e);
}
