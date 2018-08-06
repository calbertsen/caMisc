#include <Eigen/Cholesky>
#include <Eigen/Dense>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

//#include "common.hpp"
//#include "sampler.hpp"

using namespace Eigen;


double dist(VectorXd x){
  double res = 0.0;
  for(int i = 0; i < x.size(); ++i){
    res += pow(x(i),2);
  }
  return res;
}

int whichmin(VectorXd x){
  int res = 0;
  double minVal = x(0);
  if(x.size() == 1) return res;

  for(int i = 1; i < x.size(); ++i){
    if(x(i) < minVal){
      res = i;
      minVal = x(i);
    }
  }
  return res;
}

double dmvnorm(VectorXd x, VectorXd mu, MatrixXd sigma){
  VectorXd xx = x-mu;
  MatrixXd Q = sigma.inverse();
  double logdetQ = log(Q.determinant());
  double quadform = xx.transpose()*Q*xx;
  double res = .5*logdetQ - .5*quadform - xx.size()*log(sqrt(2.0*M_PI));
  return exp(res);
}

double dmvgaussmix(VectorXd x, MatrixXd mu, Matrix<MatrixXd,Dynamic,1> sigma, VectorXd alpha, int give_log = true){
  double res = 0.0;
  for(int i = 0; i < mu.cols(); ++i){
    res += alpha(i)*dmvnorm(x,mu.col(i),sigma(i));
  } 
  return res;
}


extern "C" {
  SEXP fagmmcmc(SEXP par, SEXP fn, SEXP mu, SEXP sdlist, SEXP envir, SEXP NN){

    MatrixXd mup = asMatrix(mu);
    Matrix<MatrixXd,Dynamic,1> sdp(length(sdlist));
    for(int i = 0; i < sdp.rows(); ++i){
      sdp(i) = asMatrix(VECTOR_ELT(sdlist, i));
    }

    int Ttrain = INTEGER(NN)[0];
    int Tstop = INTEGER(NN)[1];
    int Ttot = INTEGER(NN)[2];
    int N = mup.cols();
    
    int di = length(par);
    
    MatrixXd res(di,Ttot);

    res.col(0) = asVector(par);
    
    VectorXd acceptp(Ttot);
    

    double eps = 1e-10;
    MatrixXd Id(di,di);
    Id.setIdentity();
    VectorXd m(N);
    m.fill(1.0);
    VectorXd w(N);
    w = m/m.sum();
 
    GetRNGstate();

    //MH step
    for(int t = 1; t<Ttot; ++t){
      VectorXd xnew = rmvgaussmix(1,mup,sdp,w);
      VectorXd xold = res.col(t-1);
      SEXP f_callx = PROTECT(lang2(fn, R_NilValue));
      SETCADR(f_callx, asSEXP(xnew));
      double fnew = REAL(eval(f_callx, envir))[0];
      SETCADR(f_callx, asSEXP(xold));
      double fold = REAL(eval(f_callx, envir))[0];
      UNPROTECT(1);

      double u = unif_rand();
      double d = exp(fold-fnew)*dmvgaussmix(xold,mup,sdp,w)/dmvgaussmix(xnew,mup,sdp,w);
      VectorXd xuse = xold+(xnew-xold)*(d>u ? 1.0 : 0.0);
      res.col(t) = xuse;
      acceptp(t) = (d>u ? 1.0 : 0.0);
      
      //Update parameters
      VectorXd distMu(N);
      for(int i = 0; i < N;  ++i){
	distMu(i) = dist(mup.col(i)-xuse);
      }
      int j = whichmin(distMu);
      m[j]++;
      if(t >= Ttrain && t < Tstop){
	double mAkt = m(j);
	mup.col(j) = xuse/mAkt+(mAkt-1.0)/mAkt*mup.col(j);
	MatrixXd cov = sdp(j);
	VectorXd muAkt = xuse-mup.col(j);
	VectorXd prodAkt = muAkt*muAkt.transpose();
	sdp(j) << cov*(mAkt-2.0)/(mAkt-1.0)+1.0/(mAkt-1.0)*(prodAkt/mAkt+eps*Id);
	w = m/m.sum();
      }
    }
    PutRNGstate();
 
    //Make return object pretty, e.g. add dimnames
    // Maybe return a list with additional information like acceptance rate, means, cov.mats,weights?
    
    MatrixXd resOut = res.transpose();
    return(asSEXP(resOut));
        
  }
}
