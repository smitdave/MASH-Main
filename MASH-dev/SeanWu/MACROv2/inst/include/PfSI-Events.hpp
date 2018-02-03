/*
 * ################################################################################
 *        ____  _________ ____
 *       / __ \/ __/ ___//  _/
 *      / /_/ / /_ \__ \ / /
 *     / ____/ __/___/ // /
 *    /_/   /_/  /____/___/
 *
 *    PfSI Events
 *    MASH Team
 *    January 2017
 *
 * ################################################################################
*/

#ifndef PFSI_EVENTS
#define PFSI_EVENTS

#include "Events.hpp"

class human_pfsi;
struct Message;


/* ################################################################################
 * InfectiousBite
 * ################################################################################
*/

class InfectiousBite_pfsi : public Event<human_pfsi> {
public:

  void Fire(human_pfsi* human);

  void OnMessage(human_pfsi* human, const Message& message);

private:

  double ttInfectionPf();

  /* private constructor */
  InfectiousBite_pfsi(){};

  /* delete all copy & move semantics */
  InfectiousBite_pfsi(const InfectiousBite_pfsi&) = delete;
  InfectiousBite_pfsi& operator=(const InfectiousBite_pfsi&) = delete;
  InfectiousBite_pfsi(InfectiousBite_pfsi&&) = delete;
  InfectiousBite_pfsi& operator=(InfectiousBite_pfsi&&) = delete;

};

#endif
