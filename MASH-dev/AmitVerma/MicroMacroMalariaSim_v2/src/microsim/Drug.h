/* 
 * File:   Drug.h
 * Author: amit
 *
 */

#ifndef DRUG_H
#define	DRUG_H
#include "GlobalParameters.h"

class Drug {
    std::string name;
    unsigned tMax;
    std::vector<double> pKill;
    std::vector<double> gKill;
    std::vector<std::vector<double> > iGKill;
public:
    std::string getName() const;
    unsigned getTMax() const;
    void setTMax(const unsigned&);
    double getPKill(const unsigned&) const;
    double getGKill(const unsigned&) const;
    double getIGKill(const unsigned&, const unsigned&) const;
    void print() const;
    Drug(string&, unsigned&, vector<double>&, vector<double>&, vector<vector<double> >&);
    Drug(const Drug& orig);
    Drug();
    virtual ~Drug();
private:

};

#endif	/* DRUG_H */

