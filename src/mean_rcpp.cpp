#include <Rcpp.h>

// [[Rcpp::export]]
double mean_rcpp(Rcpp::NumericVector x){
  int n = x.size(); // Size of vector
  double sum = 0;   // Sum value

  // For loop, note cpp index shift to 0
  for(int i = 0; i < n; i++){
    // Shorthand for sum = sum + x[i]
    sum += x[i];
  }

  return sum/n;  // Obtain and return the Mean
}
