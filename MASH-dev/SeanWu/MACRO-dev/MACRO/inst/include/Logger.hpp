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


#ifndef LOGGER_MGDRIVE_EPI
#define LOGGER_MGDRIVE_EPI

/* ######################################################################
 # includes and forward declarations
###################################################################### */

/* C++ includes */
#include <fstream>
#include <vector>
#include <string>
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
  void open(const std::string& outfile);
  void close();

  /* return reference to output stream */
  std::ofstream& get_out(){return logstream;};

private:

  std::ofstream logstream;

};

/* open a logging stream */
void logger::open(const std::string& outfile){
  logstream.open(outfile);
};

/* close streams */
void logger::close(){
  logstream.close();
};


#endif
