
#include <Rcpp.h>
using namespace Rcpp;

#include <string>
#include <vector>
#include <fstream>

#include "json.hpp"

using json = nlohmann::json;

//' @export
// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}

//' @export
// [[Rcpp::export]]
void testJSON(const std::string outfile, const std::vector<double> numvec, const std::vector<std::string> charvec){
  std::ofstream o(outfile);
  json j;
  j["id"] = 1;
  j["nums"] = numvec;
  j["chars"] = charvec;
  o << std::setw(4) << j << std::endl;
}