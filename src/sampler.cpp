#include <Eigen/Cholesky>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

#include <common.cpp>
using namespace Eigen;

MatrixXd rmvnorm(int n,VectorXd mu, MatrixXd sigma){
  //if(!(mu.size() == sigma.cols())) 
  //if(!(sigma.cols()==sigma.rows()))


  LLT<MatrixXd> llt(sigma);
  MatrixXd L = llt.matrixL();
  MatrixXd Y(sigma.cols(),n);
  GetRNGstate();
  for(int j = 0; j < Y.rows(); ++j)
    for(int i = 0; i < Y.cols(); ++i){
      Y(j,i) = norm_rand();
    }
  PutRNGstate();
  
  MatrixXd X = (L*Y).transpose();
  for(int i = 0; i < X.rows(); ++i){
    X.row(i) += mu;
  }
  return X;
}

MatrixXd rmvlnorm(int n, VectorXd logmu, MatrixXd sigma){
  MatrixXd Y = rmvnorm(n,logmu,sigma);
  MatrixXd X = Y.array().exp().matrix();
  return X;
}

MatrixXd rlogistic(int n, VectorXd logimu, MatrixXd sigma){
  MatrixXd Y = rmvlnorm(n,logimu,sigma);
  MatrixXd X = Y;
  for(int i = 0; i < Y.rows(); ++i){
    X.row(i) *= 1/X.row(i).sum(); 
  }
  return X;
}

// Exports to R

extern "C" {
  SEXP rmvnorm(SEXP n,SEXP mu, SEXP sigma){
    int nn = asInteger(n);
    VectorXd mean = asVector(mu);
    MatrixXd sig = asMatrix(sigma);
    MatrixXd X = rmvnorm(nn,mean,sig);
    return asSEXP(X);
  }
}
extern "C" {
  SEXP rmvlnorm(SEXP n,SEXP logmu, SEXP sigma){
    int nn = asInteger(n);
    VectorXd mean = asVector(logmu);
    MatrixXd sig = asMatrix(sigma);
    MatrixXd X = rmvlnorm(nn,mean,sig);
return asSEXP(X);
  }
}

extern "C" {
  SEXP rlogistic(SEXP n,SEXP logimu, SEXP sigma){
    int nn = asInteger(n);
    VectorXd mean = asVector(logimu);
    MatrixXd sig = asMatrix(sigma);
    MatrixXd X = rlogistic(nn,mean,sig);
return asSEXP(X);
  }
}
