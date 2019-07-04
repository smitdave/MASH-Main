/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Generic Logging Class
 *
 *  Sean Wu
 *  November 2018
 */


#ifndef Logger_hpp
#define Logger_hpp

/* ######################################################################
 # includes and forward declarations
###################################################################### */

/* C++ includes */
#include <fstream>
#include <unordered_map>
#include <string>
#include <vector>
#include <iostream>


/* ######################################################################
# class declaration
###################################################################### */

class logger {
public:

  /* ctor & dtor */
  logger() {
    #ifdef DEBUG_MACRO
    std::cout << "logger born at " << this << std::endl;
    #endif
  }

  ~logger(){
    close();
    #ifdef DEBUG_MACRO
    std::cout << "logger dying at " << this << std::endl;
    #endif
  }

  /* delete copy constructor/assignment operator, default move constructor/assignment operator */
  logger(const logger&) = delete;
  logger& operator=(const logger&) = delete;
  logger(logger&&) = default;
  logger& operator=(logger&&) = default;

  /* open & close logging streams */
  void open(const std::string& outfile, const std::string& key, const std::string& header);
  void close();

  /* return reference to output stream */
  std::ofstream& get_stream(const std::string& key){return logstreams.at(key);};

private:
  std::unordered_map<std::string, std::ofstream>   logstreams;
  // std::vector<std::string>                         headers;

};


/* ######################################################################
# class methods
###################################################################### */

/* open a logging stream */
inline void logger::open(const std::string& outfile, const std::string& key, const std::string& header){
  // std::cout << "opening a log stream; outfile: " << outfile << " key: " << key << " header: " << header << "\n";
  logstreams.insert(std::make_pair(key,std::ofstream{outfile}));
  logstreams.at(key) << header << "\n";
};

/* close streams */
inline void logger::close(){
  for(auto& it : logstreams){
    it.second.close();
  }
};


#endif
