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

#ifndef LOGGER_HPP
#define LOGGER_HPP

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
  logger(){
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
  void open(const std::string& outfile, const std::string& key, const std::string& header){
    logstreams.insert(std::make_pair(key,std::ofstream{outfile}));
    logstreams.at(key) << header << "\n";
  };

  void close(){
    for(auto& it : logstreams){
      it.second.close();
    }
  };

  /* return reference to output stream */
  std::ofstream& get_stream(const std::string& key){return logstreams.at(key);};

private:
  std::unordered_map<std::string, std::ofstream>   logstreams;
};


#endif
