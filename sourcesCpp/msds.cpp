#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector localMSD(NumericVector positions, int windowSize) {
  
  int n = positions.size();
  
  if (windowSize >= n) {
    windowSize = n - 1;
  }
  
  // container for results
  NumericMatrix mat(n-1, windowSize-1);
  
  // container for mean square displacements 
  NumericVector msds(n);
  
  // calculate square displacement at each distance
  for(int j = 1; j < windowSize; j++) {
    
    // container for square displacements internally
    // NumericVector msds_internal(n-j);
    
    for(int i = 0; i < n-j; i++) {
      double value = positions(i) - positions(i+j);
      // square value
      value = value * value;
      
      mat(i,j-1) = value / j;
      
     // msds_internal(i) = value;
    }
   // msds(j-1) = mean(msds_internal) ;
  }
  
  for (int j = 0; j <= n - windowSize; j++) {
    msds(j) = mean(mat(j,_));
  }
  
  // NumericVector output(1);
  
  // output(0) = mean(msds);
  
  return(msds);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
# library(tidyverse)
# library(magrittr)

localMSD(1:20, 6)
localMSD(cumsum(rnorm(30)), 6)
*/
