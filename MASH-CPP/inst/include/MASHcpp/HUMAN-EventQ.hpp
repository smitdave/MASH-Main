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

#ifndef _MASHCPP_HUMANEVENTQ_HPP_
#define _MASHCPP_HUMANEVENTQ_HPP_

#include <Rcpp.h>

namespace MASHcpp {

class HumanEventQ {
public:
  // constructor
  HumanEventQ(const int &initQ = 10);

  // destructor
  ~HumanEventQ();

  // return first event as list
  Rcpp::List firstEvent();

  // return time of first event
  double firstTime();

  // remove first event from queue
  void rmFirstEventFromQ();

  // remove all events with certain tag from queue
  void rmTagFromQ(const std::string &tag);

  // get current number of events in queue
  int get_queueN();

  // get entire event queue
  Rcpp::List get_EventQ();

  // add an event to the queue and re-sort the queue
  void addEvent2Q(const Rcpp::List &event);

  // clear the queue
  void clearQ();

private:
  std::vector<Rcpp::List> EventQ; // event queue
  int queueN; // number of events in queue
};

}

#endif
