#include <Eigen/Dense>

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

using namespace Eigen;


SEXP asSEXP(VectorXd x){
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

VectorXd asVector(SEXP x) {

  if(!isNumeric(x))error("Element must be a numeric vector");

  int n = length(x);
  Eigen::VectorXd y(n);

  for(int i = 0; i < n; ++i)
    y(i) = REAL(x)[i];

  return y;
}
