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

/* class definition */
class parameters {
public:

  /* constructor */
  parameters(const size_t n) : paramsI(n), paramsD(n) {
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
  template <typename T>
  void                                         set_param(const std::string& key, const T val);

  /* get a value by key */
  template <typename T>
  T                                            get_param(const std::string& key);

private:
  std::unordered_map<std::string, int>         paramsI;
  std::unordered_map<std::string, double>      paramsD;
};

/* assign a key-value pair */
template <>
inline void parameters::set_param<int>(const std::string &key, const int val){
  paramsI.insert(std::make_pair(key,val));
};

template <>
inline void parameters::set_param<double>(const std::string &key, const double val){
  paramsD.insert(std::make_pair(key,val));
};

/* get a value by key */
template <>
inline double parameters::get_param<double>(const std::string &key){
  return paramsD.at(key);
}

template <>
inline int parameters::get_param<int>(const std::string &key){
  return paramsI.at(key);
}

#endif
