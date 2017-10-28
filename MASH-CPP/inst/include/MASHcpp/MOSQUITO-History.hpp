///////////////////////////////////////////////////////////////////////////////
//
//      __  _______  _____ ____  __  ________________
//     /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
//    / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
//   / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
//  /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
//
//  MASH-CPP
//  Mosquito History Class Definition
//  MASH-CPP Team
//  September 18, 2017
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MASHCPP_MOSYHIST_HPP_
#define _MASHCPP_MOSYHIST_HPP_

#include <Rcpp.h>

namespace MASHcpp {

///////////////////////////////////////////////////////////////////////////////
// MosquitoFemaleHistory: class to store female mosquito histories; \code{\link{MicroMosquitoFemale}} and \code{\link{MicroMosquito}}
///////////////////////////////////////////////////////////////////////////////

class MosquitoFemaleHistory{
// public members
public:

  ///////////////////////////////////
  // History Constructor
  ///////////////////////////////////

  // MosquitoFemaleHistory: default empty constructor
  MosquitoFemaleHistory();

  // historyInit: set initial values site of emergence when mosy born
  void historyInit(const Rcpp::Environment &privateEnv){
    ID = Rcpp::as<std::string>(privateEnv["id"]); // mosquito ID
    ixH.push_back(privateEnv["locNow"]);
    pSetH.push_back(privateEnv["pSetNow"]);
    timeH.push_back(privateEnv["tNow"]);
    stateH.push_back("emerge");
  };


  ///////////////////////////////////
  // History Tracking
  ///////////////////////////////////

  void set_mateID(const std::string &mateID_new){
    mateID = mateID_new;
  };

  // pass the private environment of the enclosing mosquito to the function
  void historyTrack(const Rcpp::Environment &privateEnv, const bool &alive){

    if(alive){
      stateH.push_back(privateEnv["state"]);  // state trajectory
      timeH.push_back(privateEnv["tNow"]);  // transition times
      ixH.push_back(privateEnv["locNow"]);  // sites visited
      pSetH.push_back(privateEnv["pSetNow"]);  // point sets visited
    } else {
      stateH.push_back(privateEnv["stateNew"]); // state trajectory
      timeH.push_back(privateEnv["tNext"]); // transition times
      this->calcBionomics(); // track bionomics upon death
    }

  };

  // historyFeed: track feeding history
  void historyFeed(const Rcpp::Environment &privateEnv){

    std::string hostID = privateEnv["hostID"];

    if(hostID[0]!=(int('z'))){
      // human host
      feedAllH += 1;    // number of blood meals
      feedAllT.push_back(privateEnv["tNow"]);   // times of blood meals
      feedHumanH += 1;    // number of blood meals on human hosts
      feedHumanT.push_back(privateEnv["tNow"]);   // times of blood meals on human hosts
      feedIxH.push_back(privateEnv["hostID"]);    // ids of all blood hosts
      bmSizeH.push_back(privateEnv["bmSize"]);    // size of blood meal
      batchH.push_back(privateEnv["batch"]);    // size of egg batch
    } else {
      // non-human host
      feedAllH += 1;    // number of blood meals
      feedAllT.push_back(privateEnv["tNow"]);   // times of blood meals
      feedIxH.push_back(privateEnv["hostID"]);    // ids of all blood hosts
      bmSizeH.push_back(privateEnv["bmSize"]);    // size of blood meal
      batchH.push_back(privateEnv["batch"]);    // size of egg batch
    }

  };

  // calcBionomics: track mosquito bionomics upon death
  void calcBionomics(){
    if(stateH.back() != "D"){
      Rcpp::stop("mosquito not dead yet but bionomics are being calculated!");
    } else {
      // calculate mean and total batch sizes
      if(batchH.size() != 0){
        int batchH_sum = std::accumulate(batchH.begin(), batchH.end(), 0);
        double batchH_mean = batchH_sum / batchH.size();
        bionomics_mBatch = batchH_mean;
        bionomics_tBatch = batchH_sum;
      } else {
        bionomics_mBatch = 0.0;
        bionomics_tBatch = 0;
      }

      // for when the mosy dies after a single blood meal
      if(feedAllT.size() < 2){
        bionomics_bmInt.push_back(0);
      } else {
        std::adjacent_difference(feedAllT.begin(), feedAllT.end(), std::back_inserter(bionomics_bmInt));
        bionomics_bmInt.erase(bionomics_bmInt.begin());
      }

      if(feedHumanT.size() < 2){
        bionomics_bmIntH.push_back(0);
      } else {
        std::adjacent_difference(feedHumanT.begin(), feedHumanT.end(), std::back_inserter(bionomics_bmIntH));
        bionomics_bmIntH.erase(bionomics_bmIntH.begin());
      }

      // lifespan
      bionomics_lifespan = timeH.back() - timeH.front();
    }
  };


  ///////////////////////////////////
  // History Export
  ///////////////////////////////////

  // exportHistory: export this mosquito history
  Rcpp::List exportHistory(){

    return(
      Rcpp::List::create(
        // ID
        Rcpp::Named("ID") = ID,
        Rcpp::Named("mateID") = mateID,
        // history objects
        Rcpp::Named("stateH") = stateH,
        Rcpp::Named("timeH") = timeH,
        Rcpp::Named("ixH") = ixH,
        Rcpp::Named("pSetH") = pSetH,
        Rcpp::Named("feedAllH") = feedAllH,
        Rcpp::Named("feedAllT") = feedAllT,
        Rcpp::Named("feedHumanH") = feedHumanH,
        Rcpp::Named("feedHumanT") = feedHumanT,
        Rcpp::Named("feedIxH") = feedIxH,
        Rcpp::Named("bmSizeH") = bmSizeH,
        Rcpp::Named("batchH") = batchH,
        // bionomics objects
        Rcpp::Named("bionomics_mBatch") = bionomics_mBatch,
        Rcpp::Named("bionomics_tBatch") = bionomics_tBatch,
        Rcpp::Named("bionomics_bmInt") = bionomics_bmInt,
        Rcpp::Named("bionomics_bmIntH") = bionomics_bmIntH,
        Rcpp::Named("bionomics_lifespan") = bionomics_lifespan
      )
    );

  };

// private members
private:

  // ID
  std::string              ID;
  std::string              mateID;

  // history objects
  std::vector<std::string>         stateH;
  std::vector<double>              timeH;
  std::vector<int>                 ixH;
  std::vector<std::string>         pSetH;
  int                              feedAllH;
  std::vector<double>              feedAllT;
  int                              feedHumanH;
  std::vector<double>              feedHumanT;
  std::vector<std::string>         feedIxH;
  std::vector<int>                 bmSizeH;
  std::vector<int>                 batchH;

  // bionomics
  double               bionomics_mBatch;
  int                  bionomics_tBatch;
  std::vector<double>  bionomics_bmInt;
  std::vector<double>  bionomics_bmIntH;
  double               bionomics_lifespan;
};

// inline definition of constructor to accept default argument values
inline MosquitoFemaleHistory::MosquitoFemaleHistory(){

  stateH.reserve(20);
  timeH.reserve(20);
  ixH.reserve(20);
  pSetH.reserve(20);
  feedAllH = 0;
  feedAllT.reserve(20);
  feedHumanH = 0;
  feedHumanT.reserve(20);
  feedIxH.reserve(20);
  bmSizeH.reserve(20);
  batchH.reserve(20);

  bionomics_mBatch = 0.0;
  bionomics_tBatch = 0;
  bionomics_bmInt.reserve(6); // might not be necessary
  bionomics_bmIntH.reserve(6); // might not be necessary
  bionomics_lifespan = 0.0;

}

///////////////////////////////////////////////////////////////////////////////
// MosquitoMaleHistory: class to store male mosquito histories; \code{\link{MicroMosquitoFemale}} and \code{\link{MicroMosquito}}
///////////////////////////////////////////////////////////////////////////////

class MosquitoMaleHistory{
// public members
public:

  ///////////////////////////////////
  // History Constructor
  ///////////////////////////////////

  // MosquitoFemaleHistory: default empty constructor
  MosquitoMaleHistory();

  // historyInit: set initial values site of emergence when mosy born
  void historyInit(const Rcpp::Environment &privateEnv){
    ID = Rcpp::as<std::string>(privateEnv["id"]); // mosquito ID
    ixH.push_back(privateEnv["locNow"]);
    pSetH.push_back(privateEnv["pSetNow"]);
    timeH.push_back(privateEnv["tNow"]);
    stateH.push_back("emerge");

  };


  ///////////////////////////////////
  // History Tracking
  ///////////////////////////////////

  // pass the private environment of the enclosing mosquito to the function
  void historyTrack(const Rcpp::Environment &privateEnv, const bool &alive){

    if(alive){
      stateH.push_back(privateEnv["state"]);  // state trajectory
      timeH.push_back(privateEnv["tNow"]);  // transition times
      ixH.push_back(privateEnv["locNow"]);  // sites visited
      pSetH.push_back(privateEnv["pSetNow"]);  // point sets visited
    } else {
      stateH.push_back(privateEnv["stateNew"]); // state trajectory
      timeH.push_back(privateEnv["tNext"]); // transition times
    }
  };

  ///////////////////////////////////
  // History Export
  ///////////////////////////////////

  // exportHistory: export this mosquito history
  Rcpp::List exportHistory(){

    return(
      Rcpp::List::create(
        // ID
        Rcpp::Named("ID") = ID,
        // history objects
        Rcpp::Named("stateH") = stateH,
        Rcpp::Named("timeH") = timeH,
        Rcpp::Named("ixH") = ixH,
        Rcpp::Named("pSetH") = pSetH
      )
    );

  };

// private members
private:

  // ID
  std::string              ID;

  // history objects
  std::vector<std::string> stateH;
  std::vector<double>      timeH;
  std::vector<int>         ixH;
  std::vector<std::string> pSetH;
};

// inline definition of constructor to accept default argument values
inline MosquitoMaleHistory::MosquitoMaleHistory(){

  stateH.reserve(20);
  timeH.reserve(20);
  ixH.reserve(20);
  pSetH.reserve(20);

}

}

#endif
