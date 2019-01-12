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
#include <numeric> /* iota */

/* shared functions, etc */
#include "UTIL-Common.hpp"

static const double epsilon = 1e-12;


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
  } else if(strcmp(state,"PEwane") == 0){
    return 6;
  } else if(strcmp(state,"GSwane") == 0){
    return 7;
  } else {
    Rcpp::stop("illegal state (called from 'state_PfSI_index')");
  }
}

//' Utility: Discretize PfSI Event Output to Daily Jump Process
//'
//' Take output of PfSI human infection logging and return a day by day state space data frame.
//'
//' @param out raw output of PfSI in \code{\link{data.frame}} format
//' @param dt size of time step to aggregate continuous time output
//'
//' @export
// [[Rcpp::export]]
Rcpp::List Util_PfSI_State(Rcpp::DataFrame& out){

  /* get time of jumps */
  std::vector<double> time = Rcpp::as<std::vector<double>>(out["time"]);
  std::vector<std::string> state0 = Rcpp::as<std::vector<std::string> >(out["state0"]);
  std::vector<std::string> state1 = Rcpp::as<std::vector<std::string> >(out["state1"]);

  /* sort all events in increasing time */
  std::vector<size_t> t_sort(time.size());
  std::iota(t_sort.begin(), t_sort.end(), static_cast<size_t>(0));

  std::sort(t_sort.begin(), t_sort.end(),
            [&](const size_t& a, const size_t& b) {
              return (time[a] < time[b]);
            }
  );

  reorder(t_sort.begin(), t_sort.end(), state0.begin());
  reorder(t_sort.begin(), t_sort.end(), state1.begin());
  reorder(t_sort.begin(), t_sort.end(), time.begin());

  /* state space aggregation */
  unsigned int tmax = std::ceil(time.back());

  /* populate initial state */
  Rcpp::IntegerVector state_init(8);
  state_init.names() = Rcpp::CharacterVector::create("S","I","P","F","PEvaxx","GSvaxx","PEwane","GSwane");

  size_t i = 0;
  while(time.at(i) < epsilon){
    std::cout << "i: " << i << " state1.at(i): " << state1.at(i) << " time: " << time.at(i) << "\n";
    state_init.at(state_PfSI_index(state1.at(i).c_str())) += 1;
    i++;
  }

  /* generate output matrix */
  Rcpp::IntegerMatrix states(tmax+1,8);
  Rcpp::colnames(states) =  Rcpp::CharacterVector::create("S","I","P","F","PEvaxx","GSvaxx","PEwane","GSwane");
  states.row(0) = state_init;

  // time.erase(time.begin(),time.begin()+(i-1));
  // state0.erase(state0.begin(),state0.begin()+(i-1));
  // state1.erase(state1.begin(),state1.begin()+(i-1));

  return Rcpp::List::create(Rcpp::Named("time")= Rcpp::wrap(time),
                                 Rcpp::Named("state0") = Rcpp::wrap(state0),
                                 Rcpp::Named("state1") = Rcpp::wrap(state1),
                                 Rcpp::Named("state_init") = state_init
                               );
};



// # process output
// h_inf <- read.csv(file = log_pars[[2]]$outfile,stringsAsFactors = FALSE)
//
// time <- h_inf$time
// state0 <- h_inf$state0
// state1 <- h_inf$state1
//
// t_sort <- order(time)
//
// time <- time[t_sort]
// state0 <- state0[t_sort]
// state1 <- state1[t_sort]
//
// tmax <- ceiling(tail(time,1))
//
// state_init <- c("S"=0,"I"=0,"P"=0,"F"=0,"PEvaxx"=0,"GSvaxx"=0,"PEwane"=0,"GSwane"=0)
//
// i <- 1
// while(time[i] <= 0){
//   state_init[state1[i]] <- state_init[state1[i]] + 1
//   i <- i + 1
// }
//
// # fill the rest of the matrix with these states
// states <- matrix(0,byrow = T,nrow=tmax+1,ncol = 8,dimnames = list(0:tmax,c("S","I","P","F","PEvaxx","GSvaxx","PEwane","GSwane")))
// states[1,] <- state_init
//
// time <- time[i:length(time)]
// state0 <- state0[i:length(state0)]
// state1 <- state1[i:length(state1)]
//
// # aggregate over days
// for(i in 1:(nrow(states)-1)){
//
//   # to update for this time step, modify the state from the previous step
//   states[(i+1),c("S","I","P")] <- states[i,c("S","I","P")]
//
//   # get the stuff that happens in this time step
//   ix <- which(time < i & time >= (i-1))
//
//   # events occured
//   if(length(ix) > 0){
//
//     # check for point events
//     ix_p <- which(state0[ix] == "_")
//
//     if(length(ix_p) > 0){
//       for(j in ix_p){
//         states[(i+1),state1[ix][j]] <- states[(i+1),state1[ix][j]] + 1
//       }
//     }
//
//     # check for jump events
//     ix_t <- which(state0[ix] != "_")
//
//     if(length(ix_t) > 0){
//       for(j in ix_t){
//
//         states[(i+1),state0[ix][j]] <- states[(i+1),state0[ix][j]] - 1
//         states[(i+1),state1[ix][j]] <- states[(i+1),state1[ix][j]] + 1
//
//       }
//     }
//   }
// }
