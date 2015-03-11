#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;
  
NumericMatrix diagMat(int n){
  NumericMatrix res(n,n);
  for(int i = 0; i < n; i++){
    res(i,i) = 1;
  }
  return res;
}

NumericMatrix solve(NumericMatrix x){
  Environment base("package:base");
  Function f = base["solve"];
  NumericMatrix res;
  try
  {
    res = f(x);
  }
  catch(...)
  {
    res = diagMat(x.ncol());
  }
  return res;
}
double prod(NumericVector x){
  double ans = x(0);
  for(int i = 1; i < x.size(); i++){
    ans *= x(i);
  }
  return ans;
}
NumericVector sqrtdiag(NumericMatrix x){
  NumericVector y(x.ncol());
  for(int i = 0;i<y.size();i++){
    y(i) = sqrt(x(i,i));
  }
  return(y);   
}
NumericVector rnorm(int n, NumericVector mean, NumericVector sd){
  NumericVector res(n);
  for(int i = 0; i < n; i++){
    res(i) = rnorm(1,mean(i),sd(i))[0];
  }
  return res;
}
NumericVector dnormc(NumericVector x, NumericVector mean, NumericVector sd){
  NumericVector  lres = -log(sd*sqrt(2*M_PI))-0.5*pow(x-mean,2)/pow(sd,2);
  return exp(lres);
}


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
    ar += (tmp>u ? 1 : 0);
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
    }
  Rcpp::Rcout<<"Acceptance rate: "<<ar/(N-1)<<std::endl;
  return(res);
}
