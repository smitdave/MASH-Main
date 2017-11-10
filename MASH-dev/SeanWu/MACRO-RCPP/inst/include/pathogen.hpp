/*
 * ################################################################################
 *
 *        ____        __  __
 *       / __ \____ _/ /_/ /_  ____  ____ ____  ____
 *      / /_/ / __ `/ __/ __ \/ __ \/ __ `/ _ \/ __ \
 *     / ____/ /_/ / /_/ / / / /_/ / /_/ /  __/ / / /
 *    /_/    \__,_/\__/_/ /_/\____/\__, /\___/_/ /_/
 *                                /____/
 *
 *    Pathogen Abstract Base Class Implementation
 *    MASH Team
 *    October 2017
 *
 * ################################################################################
*/

#ifndef PATHOGEN_HPP
#define PATHOGEN_HPP

#include <stdio.h>
#include <iostream>

#include <string>

// forward declarations
class human;

/*
 * ################################################################################
 *    Base Pathogen Class
 * ################################################################################
*/

class pathogen_base {
friend class                               human;
public:
 pathogen_base(const std::string &_pathogen_model);
 virtual ~pathogen_base();

 static pathogen_base*                     make_pathogen(std::string model);

 std::string                               get_pathogen_model();
 human*                                    get_my_human();

protected:
 std::string                               pathogen_model;
 human*                                    my_human;

};


/*
 * ################################################################################
 *    PfSI Pathogen Class
 * ################################################################################
*/

class pathogen_PfSI : public pathogen_base {
friend class                               human;
public:
 pathogen_PfSI(const std::string &_pathogen_model, const int &_pfid);
 ~pathogen_PfSI();


 void                                       set_pfid(const int &p);
 int                                        get_pfid();

private:
 int                                        pfid;
};


#endif /* PATHOGEN_HPP */
