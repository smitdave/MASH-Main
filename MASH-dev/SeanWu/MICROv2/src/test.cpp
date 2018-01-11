#include <Rcpp.h>

//' @export
// [[Rcpp::export]]
void test_rcpp(){
  Rcpp::Rcout << "Hello, world!" << std::endl;
}
