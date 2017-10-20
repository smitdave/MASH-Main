////////////////////////////////////////////////////////////
//
//  MASH
//  AQUATIC ECOLOGY Imago Queue Structure
//  Sean Wu
//  July 18, 2017
//
////////////////////////////////////////////////////////////

#ifndef _MASH_IMAGOQ_HPP_
#define _MASH_IMAGOQ_HPP_

#include <Rcpp.h>

namespace MASH {

// ImagoSlot: struct to store information for batches of emerging adults
struct ImagoSlot{
  ImagoSlot(const int &N_new, const double &tEmerge_new, const int &genotype_new, const std::string &damID_new, const std::string &sireID_new);
  int N;
  double tEmerge;
  int genotype;
  std::string damID;
  std::string sireID;
};

inline ImagoSlot::ImagoSlot(const int &N_new, const double &tEmerge_new, const int &genotype_new, const std::string &damID_new, const std::string &sireID_new){
  N = N_new;
  tEmerge = tEmerge_new;
  genotype = genotype_new;
  damID = damID_new;
  sireID = sireID_new;
}

// ImagoQVector: store ImagoSlot structs
typedef std::vector<ImagoSlot> ImagoQVector;

// ImagoQ: Imago Queue class definition
class ImagoQ {
// public members
public:

  ///////////////////////////////////
  // Imago Queue Constructor
  ///////////////////////////////////

  // constructor defined below
  ImagoQ();


  ///////////////////////////////////
  // Queue Management
  ///////////////////////////////////

  // clear_ImagoQ: Clear out all populated slots (N>0) in an ImagoQ
  void clear_ImagoQ(){

      // find all non-null slots
      std::vector<int> fullIx;
      auto it = std::find_if(ImagoQVec.begin(), ImagoQVec.end(), [](ImagoSlot ix){
          return(ix.N != 0);
      });
      while(it != ImagoQVec.end()){
          fullIx.emplace_back(std::distance(ImagoQVec.begin(), it));
          it = std::find_if(std::next(it), std::end(ImagoQVec), [](ImagoSlot ix){
              return(ix.N != 0);
          });
      }

      for(std::vector<int>::iterator it = fullIx.begin(); it != fullIx.end(); it++){
          ImagoQVec[*it].N = 0;
          ImagoQVec[*it].tEmerge = -1;
          ImagoQVec[*it].genotype = -1;
          ImagoQVec[*it].damID = -1;
          ImagoQVec[*it].sireID = -1;
      }

      N -= fullIx.size();

  };

  // clear_ImagoQTime: Clear out all ImagoQ slots where tEmerge <= time
  void clear_ImagoQTime(const double &time){

    // find slots where tEmerge <= time
    std::vector<int> timeIx;
    auto it = std::find_if(ImagoQVec.begin(), ImagoQVec.end(), [time](ImagoSlot ix){
        return(ix.tEmerge <= time);
    });
    while(it != ImagoQVec.end()){
        timeIx.emplace_back(std::distance(ImagoQVec.begin(), it));
        it = std::find_if(std::next(it), std::end(ImagoQVec), [time](ImagoSlot ix){
            return(ix.tEmerge <= time);
        });
    }

    for(std::vector<int>::iterator it = timeIx.begin(); it != timeIx.end(); it++){
        ImagoQVec[*it].N = 0;
        ImagoQVec[*it].tEmerge = -1;
        ImagoQVec[*it].genotype = -1;
        ImagoQVec[*it].damID = -1;
        ImagoQVec[*it].sireID = -1;
    }

    N -= timeIx.size();
  }

  // add_ImagoQ: Add emerging adults to the ImagoQ
  void add_ImagoQ(const int &N_new, const double &tEmerge_new, const int &genotype_new, const std::string &damID_new, const std::string &sireID_new){

      // find null slot
      auto it = std::find_if(ImagoQVec.begin(), ImagoQVec.end(), [](ImagoSlot ix){
          return(ix.N == 0);
      });

      // insert the new slot into ImagoQ
      if(it == ImagoQVec.end()){
          // there are no null slots
          ImagoQVec.push_back(ImagoSlot(N_new,tEmerge_new,genotype_new,damID_new,sireID_new));
      } else {
          // there is a null slot
          size_t ix = std::distance(ImagoQVec.begin(), it);
          ImagoQVec[ix].N = N_new;
          ImagoQVec[ix].tEmerge = tEmerge_new;
          ImagoQVec[ix].genotype = genotype_new;
          ImagoQVec[ix].damID = damID_new;
          ImagoQVec[ix].sireID = sireID_new;
      }

      N += 1;

  };


  ///////////////////////////////////
  // Queue Tracking
  ///////////////////////////////////

  // track_ImagoQ: Return the total number of emerging adults in this ImagoQ whose tEmerge <= time
  double track_ImagoQ(const double &time){

      // find slots where tEmerge <= time
      std::vector<int> timeIx;
      auto it = std::find_if(ImagoQVec.begin(), ImagoQVec.end(), [time](ImagoSlot ix){
          return(ix.tEmerge <= time);
      });
      while(it != ImagoQVec.end()){
          timeIx.emplace_back(std::distance(ImagoQVec.begin(), it));
          it = std::find_if(std::next(it), std::end(ImagoQVec), [time](ImagoSlot ix){
              return(ix.tEmerge <= time);
          });
      }

      int totalAdults = 0;
      for(auto it = timeIx.begin(); it != timeIx.end(); it++){
          totalAdults += ImagoQVec[*it].N;
      }

      return(totalAdults);
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
  Rcpp::List get_ImagoQ(){
    std::vector<Rcpp::List> out;
    out.reserve(ImagoQVec.size());
    for(auto it = ImagoQVec.begin(); it != ImagoQVec.end(); it++){
      out.push_back(
        Rcpp::List::create(
          Rcpp::Named("N") = it->N,
          Rcpp::Named("tEmerge") = it->tEmerge,
          Rcpp::Named("genotype") = it->genotype,
          Rcpp::Named("damID") = it->damID,
          Rcpp::Named("sireID") = it->sireID
        )
      );
    }
    return(Rcpp::wrap(out));
  };

  // get_ImagoQtNow: get all ImagoSlot emerging adult batches where tEmerge <= tNow
  Rcpp::List get_ImagoQTime(const double &tNow, const bool &clear){

    // find slots where tEmerge <= time
    std::vector<int> timeIx;
    auto it = std::find_if(ImagoQVec.begin(), ImagoQVec.end(), [tNow](ImagoSlot ix){
        return(ix.tEmerge <= tNow && ix.N != 0);
    });
    while(it != ImagoQVec.end()){
        timeIx.emplace_back(std::distance(ImagoQVec.begin(), it));
        it = std::find_if(std::next(it), std::end(ImagoQVec), [tNow](ImagoSlot ix){
            return(ix.tEmerge <= tNow && ix.N != 0);
        });
    }

    std::vector<Rcpp::List> out;
    for(auto it = timeIx.begin(); it != timeIx.end(); it++){
      out.push_back(
        Rcpp::List::create(
          Rcpp::Named("N") = ImagoQVec[*it].N,
          Rcpp::Named("tEmerge") = ImagoQVec[*it].tEmerge,
          Rcpp::Named("genotype") = ImagoQVec[*it].genotype,
          Rcpp::Named("damID") = ImagoQVec[*it].damID,
          Rcpp::Named("sireID") = ImagoQVec[*it].sireID
        )
      );

      // clear out the slots if requested
      if(clear){
        ImagoQVec[*it].N = 0;
        ImagoQVec[*it].tEmerge = -1;
        ImagoQVec[*it].genotype = -1;
        ImagoQVec[*it].damID = "-1";
        ImagoQVec[*it].sireID = "-1";
        N -= 1;
      }

    }

    return(Rcpp::wrap(out));
  };


// private members
private:

  ImagoQVector ImagoQVec;
  int N; // size of queue

};

// inline definition of constructor to accept default argument values
inline ImagoQ::ImagoQ(){

  N = 0;
  ImagoQVec.reserve(50);
  ImagoQVec.insert(ImagoQVec.end(), 10, ImagoSlot(0,-1,-1,"-1","-1"));

}

}

#endif
