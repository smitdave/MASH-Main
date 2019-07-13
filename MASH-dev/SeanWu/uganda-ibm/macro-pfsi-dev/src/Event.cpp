/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Generic Event Class: events occur on humans;
 *  and are specialized for each model (eg PfSI, PfMOI, etc.)
 *
 *  Sean Wu (slwu89@berkeley.edu)
 *  November 2018
*/

#include "Event.hpp"


/* ################################################################################
 * class boilerplate
################################################################################ */

/* constructor */
event::event(std::string tag_, double tEvent_, std::function<void()> eventF_):
  tag(tag_),tEvent(tEvent_),eventF(eventF_) {};

/* destructor */
event::~event() = default;
