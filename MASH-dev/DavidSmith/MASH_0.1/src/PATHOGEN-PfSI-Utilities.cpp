#include <Rcpp.h>
using namespace Rcpp;

// need to use if statements because C++ switch only works for integer input
int util_switch_PfSI(std::string event){
  if(event=="time"){
    return(0);
  } else if(event=="init"){
    return(1);
  } else if(event=="S"){
    return(2);
  } else if(event=="I"){
    return(3);
  } else if(event=="F"){
    return(4);
  } else if(event=="P"){
    return(5);
  } else if(event=="PEvaxx"){
    return(6);
  } else if(event=="PEwane"){
    return(7);
  } else if(event=="GSvaxx"){
    return(8);
  } else if(event=="GSwane"){
    return(9);
  } else {
    stop("event not recognized");
  }
}

//' PfSI: Write One Human History
//'
//' This is a C++ helper function for \code{\link{util_PfSIHistory}}
//'
//' @param historyIxH a single human history from \code{Human$get_History()}
//' @param timeBins vector of time bins (state transition times)
//' @param timeSeries matrix of occupancy vectors
//'
//'
//' @export
// [[Rcpp::export]]
void util_PfSIoneHistory(const Rcpp::List &historyIxH, const NumericVector &timeBins, NumericMatrix timeSeries){
  Rcpp::NumericVector time = historyIxH["eventT"]; // pull out the jump times
  Rcpp::StringVector state = historyIxH["events"]; // put out the state transitions
  // iterate through local jump times
  for(int ixT = 1; ixT <  time.size(); ixT++){

    // times over which to propagate forward current state
    double tNow = time[ixT];
    std::vector<int> tIter;
    auto it = std::find_if(timeBins.begin(), timeBins.end(), [tNow](double i){return i > tNow;});
    while (it != timeBins.end()) {
      tIter.emplace_back(std::distance(timeBins.begin(), it));
      it = std::find_if(std::next(it), timeBins.end(), [tNow](double i){return i > tNow;});
    }

    // propagate current state
    for(auto it = tIter.begin(); it != tIter.end(); it++){
      int col = util_switch_PfSI(Rcpp::as<std::string>(state[ixT-1]));
      timeSeries.at(*it,col) = timeSeries.at(*it,col) - 1;
      col = util_switch_PfSI(Rcpp::as<std::string>(state[ixT]));
      timeSeries.at(*it,col) = timeSeries.at(*it,col) + 1;
    }

  }
}
