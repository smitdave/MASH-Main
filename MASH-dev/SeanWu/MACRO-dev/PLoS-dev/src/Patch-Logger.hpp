/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  A hierarchy of classes that live in the Patch.
 *
 *  Sean Wu
 *  August 2019
*/

#include <array>


/* ################################################################################
 * forward declarations & alias/typedefs
################################################################################ */

/* forward definitions */
class human;


/* ################################################################################
 * base class declaration
 * log_human accepts a human pointer and derived classes cast to what they need
 * log_output writes out to .csv every day
################################################################################ */

class patch_logger {
public:

  /* constructor & destructor */
  patch_logger();
  virtual ~patch_logger() = default;

  /* move operators */
  patch_logger(patch_logger&&) = default;
  patch_logger& operator=(patch_logger&&) = default;

  /* copy operators */
  patch_logger(patch_logger&) = delete;
  patch_logger& operator=(patch_logger&) = delete;

  /* interface */
  virtual void log_human(const human* const h) = 0;
  virtual void log_output() = 0;

};


/* ################################################################################
 * PfSI
################################################################################ */

class patch_logger_pfsi : public patch_logger {
public:

  /* constructor & destructor */
  patch_logger_pfsi();
  ~patch_logger_pfsi();

  /* move operators */
  patch_logger_pfsi(patch_logger_pfsi&&) = default;
  patch_logger_pfsi& operator=(patch_logger_pfsi&&) = default;

  /* copy operators */
  patch_logger_pfsi(patch_logger_pfsi&) = delete;
  patch_logger_pfsi& operator=(patch_logger_pfsi&) = delete;

  /* concrete interface */
  virtual void log_human(const human* const h);
  virtual void log_output();

private:

  /* PfSI specific data members */
  std::array<int,3>     SIP_visitor;
  std::array<int,3>     SIP_resident_home;
  std::array<int,3>     SIP_resident_away;
  int                   inc_travel;
  int                   inc_resident;
};


/* ################################################################################
 * PfMOI
################################################################################ */

class patch_logger_pfmoi : public patch_logger {
public:

  /* constructor & destructor */
  patch_logger_pfmoi();
  ~patch_logger_pfmoi();

  /* move operators */
  patch_logger_pfmoi(patch_logger_pfmoi&&) = default;
  patch_logger_pfmoi& operator=(patch_logger_pfmoi&&) = default;

  /* copy operators */
  patch_logger_pfmoi(patch_logger_pfmoi&) = delete;
  patch_logger_pfmoi& operator=(patch_logger_pfmoi&) = delete;

  /* concrete interface */
  virtual void log_human(const human* const h);
  virtual void log_output();

private:

  /* PfMOI specific data members */
  std::array<int,3>     SIP_visitor;
  std::array<int,3>     SIP_resident_home;
  std::array<int,3>     SIP_resident_away;
  int                   inc_travel;
  int                   inc_resident;
};
