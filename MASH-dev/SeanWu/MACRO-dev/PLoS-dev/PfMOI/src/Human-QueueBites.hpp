/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Queue bites algorithms for MACRO
 *
 *  Sean Wu
 *  August 2019
*/

#ifndef QUEUE_BITES_HPP
#define QUEUE_BITES_HPP

#include <memory>

#include <RcppArmadillo.h>


/* ################################################################################
 * forward declarations & alias/typedefs
################################################################################ */

class human;


/* ################################################################################
 * base class declaration
################################################################################ */

class queue_bites {
public:

  // ctor & dtor
  queue_bites(human* const humanPtr_) : humanPtr(humanPtr_) {};
  virtual ~queue_bites() = default;

  /* move operators */
  queue_bites(queue_bites&&) = default;
  queue_bites& operator=(queue_bites&&) = default;

  /* copy operators */
  queue_bites(queue_bites&) = delete;
  queue_bites& operator=(queue_bites&) = delete;

  /* sample bites */
  virtual int sample_bites(const double eir) = 0;

  /* factory */
  static std::unique_ptr<queue_bites> factory(const int type, human* const humanP, void* data);

protected:
  human*      humanPtr;
};


/* ################################################################################
 * poisson biting
################################################################################ */

class queue_bites_pois : public queue_bites {
public:

  // ctor & dtor
  queue_bites_pois(human* const humanPtr_) : queue_bites(humanPtr_) {};
  ~queue_bites_pois() = default;

  /* move operators */
  queue_bites_pois(queue_bites_pois&&) = default;
  queue_bites_pois& operator=(queue_bites_pois&&) = default;

  /* copy operators */
  queue_bites_pois(queue_bites_pois&) = delete;
  queue_bites_pois& operator=(queue_bites_pois&) = delete;

  /* sample bites */
  virtual int sample_bites(const double eir);

};


/* ################################################################################
 * negative binomial biting
################################################################################ */

class queue_bites_nbinom : public queue_bites {
public:

  // ctor & dtor
  queue_bites_nbinom(human* const humanPtr_, const double disp_) : queue_bites(humanPtr_), disp(disp_) {};
  ~queue_bites_nbinom() = default;

  /* move operators */
  queue_bites_nbinom(queue_bites_nbinom&&) = default;
  queue_bites_nbinom& operator=(queue_bites_nbinom&&) = default;

  /* copy operators */
  queue_bites_nbinom(queue_bites_nbinom&) = delete;
  queue_bites_nbinom& operator=(queue_bites_nbinom&) = delete;

  /* sample bites */
  virtual int sample_bites(const double eir);

private:
  double      disp; // overdispersion parameter
};

#endif
