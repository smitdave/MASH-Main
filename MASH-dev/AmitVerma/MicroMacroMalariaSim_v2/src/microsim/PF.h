/* 
 * File:   PF.h
 * Author: amit
 *
 * P. falciparum Infections
 */

#ifndef PF_H
#define	PF_H

class PF {
private:
    int biteD;
    int startD;
    int endD;
    int dend;
    double merz;
    double grow;
    double maxD;
    double shld;
    double sigma;
    double delta;
    unsigned P;
    unsigned G;
    unsigned iG[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    double pDdef  = 0;   
    double pDlt   = 0;  
    //double pKill  = 0; // #Book keeping variable. Decline in densities over what was expected. (Probably cumulative). Tells about the cumulative effects.   
    //double gKill  = 1; //#Proportional reduction, 1 means no effect
    //double iGkill[10] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
public:
    unsigned getP() const;
    unsigned updateP(double);
    unsigned updateG(double, double [], double);
    void updateIG();
    unsigned outOfLiver();
    double getGrowthRate(double);
    double medPFDens(int, double);
    bool isActive();
    PF(int);
    PF(const PF& orig);
    virtual ~PF();
private:

};

#endif	/* PF_H */

