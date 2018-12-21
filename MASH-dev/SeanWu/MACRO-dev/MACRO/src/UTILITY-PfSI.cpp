/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Utility Rcpp Code for PfSI Model
 *
 *  Sean Wu
 *  November 2018
 */

/* Rcpp API */
#include <Rcpp.h>

/* C++ */
#include <vector>
#include <algorithm>
#include <string>

/* shared functions, etc */
#include "UTIL-Common.hpp"




/* go from state to index */
inline size_t state_PfSI_index(const char* state){
  if(strcmp(state,"S") == 0){
    return 0;
  } else if(strcmp(state,"I") == 0){
    return 1;
  } else if(strcmp(state,"P") == 0){
    return 2;
  } else if(strcmp(state,"F") == 0){
    return 3;
  } else if(strcmp(state,"PEvaxx") == 0){
    return 4;
  } else if(strcmp(state,"GSvaxx") == 0){
    return 5;
  } else {
    Rcpp::stop("illegal state (called from 'state_search')");
  }
}

//' Utility: Discretize PfSI Event Output to Daily Jump Process
//'
//' Take output of PfSI human infection logging and return a day by day state space data frame.
//'
//' @param out raw output of PfSI in \code{\link{data.frame}} format
//'
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame Util_PfSI_State(const Rcpp::DataFrame& out){

  /* fields we need */
  std::vector<int> humanID = Rcpp::as<std::vector<int>>(out["humanID"]);
  std::vector<double> time = Rcpp::as<std::vector<double>>(out["time"]);
  std::vector<std::string> event = Rcpp::as<std::vector<std::string> >(out["event"]);

  /* sort all events in increasing time */
  std::vector<size_t> t_sort(event.size());
  std::generate(t_sort.begin(), t_sort.end(), [i = 0]() mutable { return i++; });

  std::sort(t_sort.begin(),
            t_sort.end(),
            [&](int i1, int i2) { return time[i1] < time[i2];});

  reorder(t_sort.begin(), t_sort.end(), event.begin());
  reorder(t_sort.begin(), t_sort.end(), time.begin());
  reorder(t_sort.begin(), t_sort.end(), humanID.begin());

  /* state space aggregation */
  unsigned int tmax = std::ceil(time.back());

  /* output matrix */
  Rcpp::IntegerMatrix states(tmax,6);
  Rcpp::colnames(states) = Rcpp::CharacterVector::create("S","I","P","F","PEvaxx","GSvaxx");

  /* poulate initial state */
  size_t t = 0;
  for(; t<tmax; t++){
    if(time[t] <= 0.0){
      size_t s = state_PfSI_index(event[t].c_str());
      states(0,s) += 1;
    } else {
      break;
    }
  }

  /* populate other states */
  for(; t<tmax; t++){

  }

  return Rcpp::DataFrame::create(Rcpp::Named("humanID")= Rcpp::wrap(humanID),
                                 Rcpp::Named("time") = Rcpp::wrap(time),
                                 Rcpp::Named("event") = Rcpp::wrap(event));
};
