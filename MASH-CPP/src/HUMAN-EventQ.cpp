/////////////////////////////////////////////////////////////////////////////
//      ______                 __     ____
//     / ____/   _____  ____  / /_   / __ \__  _____  __  _____
//    / __/ | | / / _ \/ __ \/ __/  / / / / / / / _ \/ / / / _ \
//   / /___ | |/ /  __/ / / / /_   / /_/ / /_/ /  __/ /_/ /  __/
//  /_____/ |___/\___/_/ /_/\__/   \___\_\__,_/\___/\__,_/\___/
//
//  MASH
//  HUMANS Event Queue Structure
//  Sean Wu
//  July 18, 2017
//
///////////////////////////////////////////////////////////////////////////


#include <Rcpp.h>
#include "MASHcpp/HUMAN-EventQ.hpp"

using namespace Rcpp;

namespace MASHcpp {

// comparator funcion for sorting events by 'tEvent'
inline bool compare_tEvent(const Rcpp::List& eventA, const Rcpp::List& eventB) { return double(eventA["tEvent"]) < double(eventB["tEvent"]); }

// constructor
HumanEventQ::HumanEventQ(const int &initQ){
  EventQ.reserve(initQ);
  EventQ.push_back(Rcpp::List::create(Rcpp::Named("tEvent")=73000,Rcpp::Named("PAR")=R_NilValue,Rcpp::Named("tag")="death"));
  queueN = EventQ.size();
};

// destructor
HumanEventQ::~HumanEventQ(){};

// return first event as list
Rcpp::List HumanEventQ::firstEvent(){
  return(EventQ.front());
};

// return time of first event
double HumanEventQ::firstTime(){
  return(EventQ[0]["tEvent"]);
};

// remove first event from queue
void HumanEventQ::rmFirstEventFromQ(){
  EventQ.erase(EventQ.begin());
  queueN -= 1;
};

// remove all events with certain tag from queue
void HumanEventQ::rmTagFromQ(const std::string &tag){
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
int HumanEventQ::get_queueN(){
  return(queueN);
};

// get entire event queue
Rcpp::List HumanEventQ::get_EventQ(){
  return(Rcpp::wrap(EventQ));
};

// add an event to the queue and re-sort the queue
void HumanEventQ::addEvent2Q(const Rcpp::List &event){
  EventQ.push_back(event);
  std::sort(EventQ.begin(), EventQ.end(), compare_tEvent);
  queueN += 1;
};

// clear the queue
void HumanEventQ::clearQ(){
  EventQ.clear();
  EventQ.push_back(Rcpp::List::create(Rcpp::Named("tEvent")=73000,Rcpp::Named("PAR")=R_NilValue,Rcpp::Named("tag")="death"));
  queueN = 1;
};

}
