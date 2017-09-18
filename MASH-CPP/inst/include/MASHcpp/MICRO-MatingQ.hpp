///////////////////////////////////////////////////////////////////////////////
//      __  _________________  ____
//     /  |/  /  _/ ____/ __ \/ __ \
//    / /|_/ // // /   / /_/ / / / /
//   / /  / // // /___/ _, _/ /_/ /
//  /_/  /_/___/\____/_/ |_|\____/
//
//  MASH-CPP
//  MICRO MatingQ class definition
//  MASH-CPP Team
//  September 18, 2017
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MASHCPP_MATINGQ_HPP_
#define _MASHCPP_MATINGQ_HPP_

#include <Rcpp.h>

namespace MASHcpp {

  class MatingQ{

  public:

    MatingQ();

    int get_N(){
      return(N);
    };

    void add_male2Q(const std::string &maleID_new, const double &mateFitness_new){

      // chcek if new host is already in this maleID_new
      std::vector<std::string>::iterator it = std::find(maleID.begin(), maleID.end(), who_new);
      if(it != maleID_new.end()){
        // host is already in this RiskQ
        int ix = std::distance(maleID_new.begin(), it);
        maleID_new[ix] = maleID_new;
        mateFitness[ix] = mateFitness_new;
      } else {
        // host is new to this RiskQ
        N++;
        maleID.push_back(maleID_new);
        mateFitness.push_back(mateFitness_new);
      }

    };

    Rcpp::List get_MatingQ(){
      return(
        Rcpp::List::create(
          Rcpp::Named("maleID") = maleID,
          Rcpp::Named("mateFitness") = mateFitness
        )
      );
    };

    void clear_MatingQ(){
      maleID.clear();
      mateFitness.clear();
    };

  private:

    int N;                                  // number of males in this MatingQ
    std::vector<std::string> maleID;        // ID of male in queue
    std::vector<double>      mateFitness;   // mating fitness

  };

  inline MatingQ::MatingQ(){
    N = 0;
    maleID.reserve(30);
    mateFitness.reserve(30);
  }

}

#endif
