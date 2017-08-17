////////////////////////////////////////////////////////////
//
//  MASH
//  MICRO
//  Mating queue class definition
//  Sean Wu
//  August 7, 2017
//
////////////////////////////////////////////////////////////

#ifndef _MASHCPP_MATINGQ_HPP_
#define _MASHCPP_MATINGQ_HPP_

#include <Rcpp.h>

namespace MASHcpp {

  class MatingQ{

  public:

    add_male2Q(){

    };

    get_MatingQ(){

    };

    clear_MatingQ(){

    };

  private:

    int N; // number of males in this MatingQ
    std::vector<std::string> maleIx; // ID of male in queue
    std::vector<double>      mateWeight; // relative mating ability

  };

  inline MatingQ::MatingQ(){
    N = 0;
    maleIX.reserve(30);
    mateWeight.reserve(30);
  }

}

#endif
