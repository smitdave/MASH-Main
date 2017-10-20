////////////////////////////////////////////////////////////
//
//  MASH
//  HUMANS Generic History Structures
//  Sean Wu
//  July 18, 2017
//
////////////////////////////////////////////////////////////

#ifndef _MASH_HIST_HPP_
#define _MASH_HIST_HPP_

#include <Rcpp.h>

namespace MASH {

// HistoryGeneric stores events as strings and times as double precision floats
class HistoryGeneric {
public:

  // constructor
  HistoryGeneric(const int &N = 100){
    events.reserve(N);
    events.push_back("init");
    eventT.reserve(N);
    eventT.push_back(-1);
  }

  // history tracking
  void track_history(const double &tEvent, const std::string &event){
    events.push_back(event);
    eventT.push_back(tEvent);
  };

  // return history
  Rcpp::List get_history(){
    return(
      Rcpp::List::create(
        Rcpp::Named("events") = events,
        Rcpp::Named("eventT") = eventT
      )
    );
  };

private:
  std::vector<std::string> events;
  std::vector<double>      eventT;
};

// HistoryTravel stores events for MACRO Human Travel
class HistoryTravel{
public:

  // constructor
  HistoryTravel(const int &N = 20){
    locationHVec.reserve(N);
    tTravelVec.reserve(N);
  }

  // travel my travel history
  void track_travel(const double &tTravel, const int &locationH){
    tTravelVec.push_back(tTravel);
    locationHVec.push_back(locationH);
  };

  Rcpp::List get_travelHistory(){
    return(
      Rcpp::List::create(
        Rcpp::Named("location") = locationHVec,
        Rcpp::Named("tTravel") = tTravelVec
      )
    );
  };

private:
  std::vector<int>    locationHVec;
  std::vector<double> tTravelVec;
};

}

#endif
