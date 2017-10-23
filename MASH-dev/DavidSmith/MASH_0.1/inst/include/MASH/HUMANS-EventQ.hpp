////////////////////////////////////////////////////////////
//
//  MASH
//  HUMANS Event Queue Structure
//  Sean Wu
//  July 18, 2017
//
////////////////////////////////////////////////////////////

#ifndef _MASH_HUMANEVENTQ_HPP_
#define _MASH_HUMANEVENTQ_HPP_

#include <Rcpp.h>

namespace MASH {

// comparator funcion for sorting events by 'tEvent'
inline bool compare_tEvent(const Rcpp::List& eventA, const Rcpp::List& eventB) { return double(eventA["tEvent"]) < double(eventB["tEvent"]); }

class HumanEventQ {
public:
  // constructor
  HumanEventQ(const int &initQ = 100){
    EventQ.reserve(initQ);
    EventQ.push_back(Rcpp::List::create(Rcpp::Named("tEvent")=73000,Rcpp::Named("PAR")=R_NilValue,Rcpp::Named("tag")="death"));
    queueN = EventQ.size();
  }

  // return first event as list
  Rcpp::List firstEvent(){
    return(EventQ.front());
  };

  // return time of first event
  int firstTime(){
    return(EventQ[0]["tEvent"]);
  };

  // remove first event from queue
  void rmFirstEventFromQ(){
    EventQ.erase(EventQ.begin());
    queueN -= 1;
  };

  // remove all events with certain tag from queue
  void rmTagFromQ(const std::string &tag){
    EventQ.erase(std::remove_if(
      EventQ.begin(), EventQ.end(),
      [tag](const Rcpp::List& Event) {
        return(
          tag.compare(Rcpp::as<std::string>(Event["tag"]))==0
        );
      }), EventQ.end());
    queueN = EventQ.size();
  };

  // get current number of events in queue
  int get_queueN(){
    return(queueN);
  };

  // get entire event queue
  Rcpp::List get_EventQ(){
    return(Rcpp::wrap(EventQ));
  };

  // add an event to the queue and re-sort the queue
  void addEvent2Q(const Rcpp::List &event){
    EventQ.push_back(event);
    std::sort(EventQ.begin(), EventQ.end(), compare_tEvent);
    queueN += 1;
  };

private:
  std::vector<Rcpp::List> EventQ; // event queue
  int queueN = 0; // number of events in queue
};

}

#endif
