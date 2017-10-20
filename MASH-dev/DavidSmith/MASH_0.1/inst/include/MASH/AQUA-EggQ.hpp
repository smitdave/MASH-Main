////////////////////////////////////////////////////////////
//
//  MASH
//  AQUATIC ECOLOGY Egg Queue Structure
//  Sean Wu
//  July 19, 2017
//
////////////////////////////////////////////////////////////

#ifndef _MASH_EGGQ_HPP_
#define _MASH_EGGQ_HPP_

#include <Rcpp.h>

namespace MASH {

// ImagoSlot: struct to store information for batches of emerging adults
struct EggSlot{
  EggSlot(const int &N_new, const double &tOviposit_new, const int &genotype_new, const std::string &damID_new, const std::string &sireID_new);
  int N;
  double tOviposit;
  int genotype;
  std::string damID;
  std::string sireID;
};

inline EggSlot::EggSlot(const int &N_new, const double &tOviposit_new, const int &genotype_new, const std::string &damID_new, const std::string &sireID_new){
  N = N_new;
  tOviposit = tOviposit_new;
  genotype = genotype_new;
  damID = damID_new;
  sireID = sireID_new;
}

// EggQVector: store EggSlot structs
typedef std::vector<EggSlot> EggQVector;

// EggQ: Egg Queue class definition
class EggQ {
// public members
public:

  ///////////////////////////////////
  // Egg Queue Constructor
  ///////////////////////////////////

  // constructor defined below
  EggQ();


  ///////////////////////////////////
  // Queue Management
  ///////////////////////////////////

  // clear_EggQ: Clear out all populated slots (N>0) in an EggQ
  void clear_EggQ(){

      // find all non-null slots
      std::vector<int> fullIx;
      auto it = std::find_if(EggQVec.begin(), EggQVec.end(), [](EggSlot ix){
          return(ix.N != 0);
      });
      while(it != EggQVec.end()){
          fullIx.emplace_back(std::distance(EggQVec.begin(), it));
          it = std::find_if(std::next(it), std::end(EggQVec), [](EggSlot ix){
              return(ix.N != 0);
          });
      }

      for(std::vector<int>::iterator it = fullIx.begin(); it != fullIx.end(); it++){
          EggQVec[*it].N = 0;
          EggQVec[*it].tOviposit = -1;
          EggQVec[*it].genotype = -1;
          EggQVec[*it].damID = -1;
          EggQVec[*it].sireID = -1;
      }

      N -= fullIx.size();

  };

  // clear_EggQTime: Clear out all EggQ slots where tOviposit <= time
  void clear_EggQTime(const double &time){

    // find slots where tEmerge <= time
    std::vector<int> timeIx;
    auto it = std::find_if(EggQVec.begin(), EggQVec.end(), [time](EggSlot ix){
        return(ix.tOviposit <= time);
    });
    while(it != EggQVec.end()){
        timeIx.emplace_back(std::distance(EggQVec.begin(), it));
        it = std::find_if(std::next(it), std::end(EggQVec), [time](EggSlot ix){
            return(ix.tOviposit <= time);
        });
    }

    for(std::vector<int>::iterator it = timeIx.begin(); it != timeIx.end(); it++){
        EggQVec[*it].N = 0;
        EggQVec[*it].tOviposit = -1;
        EggQVec[*it].genotype = -1;
        EggQVec[*it].damID = -1;
        EggQVec[*it].sireID = -1;
    }

    N -= timeIx.size();
  }

  // add_EggQ: Add egg batches to EggQ
  void add_EggQ(const int &N_new, const double &tOviposit_new, const int &genotype_new, const std::string &damID_new, const std::string &sireID_new){

      // find null slot
      auto it = std::find_if(EggQVec.begin(), EggQVec.end(), [](EggSlot ix){
          return(ix.N == 0);
      });

      // insert the new slot into EggQ
      if(it == EggQVec.end()){
          // there are no null slots
          EggQVec.push_back(EggSlot(N_new,tOviposit_new,genotype_new,damID_new,sireID_new));
      } else {
          // there is a null slot
          size_t ix = std::distance(EggQVec.begin(), it);
          EggQVec[ix].N = N_new;
          EggQVec[ix].tOviposit = tOviposit_new;
          EggQVec[ix].genotype = genotype_new;
          EggQVec[ix].damID = damID_new;
          EggQVec[ix].sireID = sireID_new;
      }

      N += 1;

  };


  ///////////////////////////////////
  // Queue Tracking
  ///////////////////////////////////

  // track_EggQ: Return the total number of eggs in this EggQ whose tOviposit <= time
  double track_EggQ(const double &time){

      // find slots where tEmerge <= time
      std::vector<int> timeIx;
      auto it = std::find_if(EggQVec.begin(), EggQVec.end(), [time](EggSlot ix){
          return(ix.tOviposit <= time);
      });
      while(it != EggQVec.end()){
          timeIx.emplace_back(std::distance(EggQVec.begin(), it));
          it = std::find_if(std::next(it), std::end(EggQVec), [time](EggSlot ix){
              return(ix.tOviposit <= time);
          });
      }

      int totalEggs = 0;
      for(auto it = timeIx.begin(); it != timeIx.end(); it++){
          totalEggs += EggQVec[*it].N;
      }

      return(totalEggs);
  };


  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  int get_N(){
    return(N);
  };
  void set_N(const int &N_new){
    N = N_new;
  };

  // please do not use; it is very inefficient, mostly for debugging
  Rcpp::List get_EggQ(){
    std::vector<Rcpp::List> out;
    out.reserve(EggQVec.size());
    for(auto it = EggQVec.begin(); it != EggQVec.end(); it++){
      out.push_back(
        Rcpp::List::create(
          Rcpp::Named("N") = it->N,
          Rcpp::Named("tOviposit") = it->tOviposit,
          Rcpp::Named("genotype") = it->genotype,
          Rcpp::Named("damID") = it->damID,
          Rcpp::Named("sireID") = it->sireID
        )
      );
    }
    return(Rcpp::wrap(out));
  };

  // get_EggQTime: get all EggSlot egg batches where tOviposit <= tNow
  Rcpp::List get_EggQTime(const double &tNow, const bool &clear){

    // find slots where tEmerge <= time
    std::vector<int> timeIx;
    auto it = std::find_if(EggQVec.begin(), EggQVec.end(), [tNow](EggSlot ix){
        return(ix.tOviposit <= tNow && ix.N != 0);
    });
    while(it != EggQVec.end()){
        timeIx.emplace_back(std::distance(EggQVec.begin(), it));
        it = std::find_if(std::next(it), std::end(EggQVec), [tNow](EggSlot ix){
            return(ix.tOviposit <= tNow && ix.N != 0);
        });
    }

    std::vector<Rcpp::List> out;
    for(auto it = timeIx.begin(); it != timeIx.end(); it++){
      out.push_back(
        Rcpp::List::create(
          Rcpp::Named("N") = EggQVec[*it].N,
          Rcpp::Named("tEmerge") = EggQVec[*it].tOviposit,
          Rcpp::Named("genotype") = EggQVec[*it].genotype,
          Rcpp::Named("damID") = EggQVec[*it].damID,
          Rcpp::Named("sireID") = EggQVec[*it].sireID
        )
      );

      // clear out the slots if requested
      if(clear){
        EggQVec[*it].N = 0;
        EggQVec[*it].tOviposit = -1;
        EggQVec[*it].genotype = -1;
        EggQVec[*it].damID = -1;
        EggQVec[*it].sireID = -1;
        N -= 1;
      }

    }

    return(Rcpp::wrap(out));
  };


// private members
private:

  EggQVector EggQVec;
  int N; // size of queue

};

// inline definition of constructor to accept default argument values
inline EggQ::EggQ(){

  N = 0;
  EggQVec.reserve(50);
  EggQVec.insert(EggQVec.end(), 10, EggSlot(0,-1,-1,"-1","-1"));

}

}

#endif
