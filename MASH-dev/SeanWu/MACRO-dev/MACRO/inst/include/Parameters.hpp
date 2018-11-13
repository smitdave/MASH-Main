/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Generic Parameters Class
 *
 *  Sean Wu
 *  November 2018
 */

#ifndef Parameters_hpp
#define Parameters_hpp

/* standard includes */
#include <stdio.h>
#include <iostream>

/* hash-table */
#include <unordered_map>
#include <string> /* for keys */

class parameters {
public:

  /* constructor */
  parameters(const size_t n) : params(n) {
    #ifdef DEBUG_MACRO
    std::cout << "parameters born at " << this << std::endl;
    #endif
  };

  /* destructor */
  ~parameters(){
    #ifdef DEBUG_MACRO
    std::cout << "parameters dying at " << this << std::endl;
    #endif
  };

  /* delete copy constructor/assignment operator, default move constructor/assignment operator */
  parameters(const parameters&) = delete;
  parameters& operator=(const parameters&) = delete;
  parameters(parameters&&) = default;
  parameters& operator=(parameters&&) = default;

  /* assign a key-value pair */
  void                                              set_param(const std::string& key, const double val){
    params.insert(std::make_pair(key,val));
  };

  /* get a value by key */
  double                                            get_param(const std::string& key){
    return params.at(key);
  };

private:
  std::unordered_map<std::string, double>      params;
};


#endif
