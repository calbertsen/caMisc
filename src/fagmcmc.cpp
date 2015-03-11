#include <Rcpp.h>
#include <math.h>
#include <RcppEigen.h>


// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using namespace Eigen;
using Eigen::Map;
using Eigen::MatrixXi;

NumericVector sqrtdiag(NumericMatrix x){
  NumericVector y(x.ncol());
  for(int i = 0;i<y.size();i++){
    y(i) = sqrt(x(i,i));
  }
  return(y);   
}

NumericVector pmax(NumericVector a, double b){
  Environment base("package:base");
  Function f = base["pmax"];
  return f(a,b);
}  
NumericMatrix transpose(NumericMatrix x){
  Environment base("package:base");
  Function f = base["t"];
  return f(x);
}  
NumericVector sample(int n, NumericVector alpha){
  Environment base("package:base");
  Function f = base["sample.int"];
  return f(alpha.size(),n,true,alpha);
}  

NumericMatrix diagMat(int n){
  NumericMatrix res(n,n);
  for(int i = 0; i < n; i++){
    res(i,i) = 1;
  }
  return res;
}
NumericMatrix diagSqMat(NumericVector x){
  NumericMatrix res(x.size(),x.size());
  for(int i = 0; i < x.size(); i++){
    res(i,i) = sqrt(x[i]);
  }
  return res;
}

NumericVector rnorm(int n, NumericVector mean, NumericVector sd){
  RNGScope scope;		// ensure RNG gets set/reset
  NumericVector res(n);
  for(int i = 0; i < n; i++){
    res(i) = rnorm(1,mean(i),sd(i))[0];
  }
  return res;
}
double dist(NumericVector x){
  double res = 0.0;
  for(int i = 0; i < x.size(); ++i){
    res += pow(x[i],2);
  }
  return res;
}
NumericMatrix fill_mat(NumericVector x,int ncol, int nrow){
  NumericMatrix res(nrow,ncol);
  for(int i = 0; i < nrow*ncol; ++i){
    res[i] = x[i];
  }
  return res;
}
NumericMatrix chol(const NumericMatrix &a){
  //From TMB
    int n=a.rows();
    NumericMatrix l(n,n);
    std::fill(l.begin(),l.end(),0.0);
    double tmp;
    for(int i=0;i<n;i++)
      for(int j=0;j<=i;j++){
	tmp=a(i,j);
	for(int k=0;k<j;k++)tmp-=l(i,k)*l(j,k);
	if(i==j)l(i,j)=sqrt(tmp);
	else l(i,j)=tmp/l(j,j);
      }
    return l;
}
NumericVector lsolve(NumericMatrix &l,
NumericVector y){
  //From TMB
    int n=l.rows();
    NumericVector x(n);
    double tmp;
    for(int i=0;i<n;i++){
      tmp=y(i);
      for(int k=0;k<i;k++)tmp-=x(k)*l(i,k);
      x(i)=tmp/l(i,i);
    }
    return x;
  }
double Quadform(NumericMatrix L, NumericVector x){
    //From TMB
    NumericVector u=lsolve(L,x);
    return sum(u*u);
}


NumericMatrix mvrnorm(int n,NumericVector mu, NumericMatrix Sigma, double tol = 1.0e-6)
{
  RNGScope scope;		// ensure RNG gets set/reset
  // Simplification of mvrnorm in package MASS
  int p = mu.size();
  const Map<MatrixXd> SigmaE(as<Map<MatrixXd> >(Sigma));
  SelfAdjointEigenSolver<MatrixXd> eigensolver(SigmaE);
  VectorXd evaE = eigensolver.eigenvalues();
  MatrixXd eveE = eigensolver.eigenvectors();
  NumericMatrix X = fill_mat(rnorm(n*p),n,p);
  NumericMatrix evd = diagSqMat(pmax(wrap(evaE), 0));
  const Map<MatrixXd> XE(as<Map<MatrixXd> >(X));
  const Map<MatrixXd> evdE(as<Map<MatrixXd> >(evd));
  const Map<MatrixXd> muE(as<Map<MatrixXd> >(mu));
  MatrixXd YE(n,p);
  YE = eveE * evdE * XE;
  for(int i = 0; i < n; ++i){
    YE.col(i) += muE;
  }
  return wrap(YE.transpose());//transpose(Y);
}
double mvdnorm(NumericVector obs, NumericVector mu, NumericMatrix sigma){
    //From TMB
  NumericVector x = obs-mu;
  NumericMatrix L = chol(sigma);
  double logdetQ = 0.0;
  for(int i=0;i<L.rows();i++)logdetQ -= 2.0*log(L(i,i));
  double res = -.5*logdetQ + .5*Quadform(L,x) + x.size()*log(sqrt(2.0*M_PI));
  return exp(-res);
}
// [[Rcpp::export]]
NumericMatrix mvrgaussmix(int n, NumericMatrix mu, List sigma, NumericVector alpha){
  RNGScope scope;
  NumericMatrix res(n,mu.nrow());
  NumericVector I = sample(n,alpha);
  for(int i = 0; i < n; ++i){
    int j = I[i]-1;
    res.row(i) = mvrnorm(1,mu(_,j),sigma[j]);
  }
  return res;
}
// [[Rcpp::export]]
double mvdgaussmix(NumericVector x,NumericMatrix mu, List sigma, NumericVector alpha){
  double res = 0.0;
  for(int i = 0; i < alpha.size(); ++i){
    res += alpha[i] * mvdnorm(x,mu(_,i),sigma[i]);
  }
  return res;
}

NumericVector cumavg(NumericVector x){
  NumericVector y(x.size());
  double cumsum = 0.0;
  for(int i = 0; i < x.size(); ++i){
      cumsum += x[i];
      y[i] = cumsum/(i+1);
  }
  return y;
}

// [[Rcpp::export]]
List mcmcFAGM_MH(NumericVector T, NumericMatrix mu, List sd, List obj){
  
  NumericMatrix mup = Rcpp::clone(mu);
  List sdp = Rcpp::clone(sd);

  int Ttrain = T[0];
  int Tstop = T[1];
  int Ttot = T[2];
  int N = mup.ncol();

  Function fn = obj["fn"];
  //Function he = obj["he"];
  NumericVector objpar = obj["par"];
  CharacterVector nam = objpar.attr("names");
  int di = objpar.size();

  NumericMatrix res(Ttot,di);
  Rcpp::List dimnms = Rcpp::List::create(res.attr("rownames"),
      nam);
  res.attr("dimnames") = dimnms;
  res.row(0) = objpar;
  
  NumericVector acceptp(Ttot);
  

  double eps = 1e-10;
  NumericMatrix Id = diagMat(di);
  NumericVector m(N);
  std::fill(m.begin(),m.end(),1.0);
  NumericVector w(N);
  w = m/sum(m);
 

  //MH step
  for(int t = 1; t<Ttot; ++t){
    NumericVector xnew = mvrgaussmix(1,mup,sdp,w);
    NumericVector xold = res.row(t-1);
    double fnew = Rcpp::as<double>(fn(xnew));
    double fold = Rcpp::as<double>(fn(xold));
    
    RNGScope scope;
    double u = runif(1)[0];
    double d = exp(fold-fnew)*mvdgaussmix(xold,mup,sdp,w)/mvdgaussmix(xnew,mup,sdp,w);
    NumericVector xuse = xold+(xnew-xold)*(d>u ? 1.0 : 0.0);
    res.row(t) = xuse;
    acceptp(t) = (d>u ? 1.0 : 0.0);

    //Update parameters
    NumericVector distMu(N);
    for(int i = 0; i < N;  ++i){
      distMu(i) = dist(mup(_,i)-xuse);
    }
    int j = std::distance(distMu.begin(), std::min_element(distMu.begin(), distMu.end()));
    m[j]++;
    if(t >= Ttrain && t < Tstop){
      double mAkt = m[j];
      mup(_,j) = xuse/mAkt+(mAkt-1.0)/mAkt*mup(_,j);
      NumericMatrix cov = sdp[j];
      NumericVector muAkt = xuse-mup(_,j);
      const Map<VectorXd> muAktE(as<Map<VectorXd> >(muAkt));
      NumericMatrix prodAkt = wrap(muAktE*muAktE.transpose());
      sdp[j] = fill_mat(cov*(mAkt-2.0)/(mAkt-1.0)+1.0/(mAkt-1.0)*(prodAkt/mAkt+eps*Id),di,di);
      w = m/sum(m);
    }
  }
  return Rcpp::List::create(Rcpp::Named("chain") = res,
			    Rcpp::Named("acceptance") = acceptp,
			    Rcpp::Named("avg.acceptance") = cumavg(acceptp),
			    Rcpp::Named("mean") = mup,
			    Rcpp::Named("cov.mat") = sdp,
			    Rcpp::Named("weights") = w);
 
}

