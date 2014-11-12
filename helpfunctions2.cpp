#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector CDFmin(IntegerVector x, int n) {
  int count = 0;
  for(int i = 0; i <= n-1; i++){
    if(x[i] == -1){
      count ++;
    }
    x[i] = count;
  }
  return x;
}
