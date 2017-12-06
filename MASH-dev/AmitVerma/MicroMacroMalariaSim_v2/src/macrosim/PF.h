/* 
 * File:   PF.h
 * Author: amit
 *
 * Created on April 11, 2014, 1:55 PM
 */

#ifndef PF_H
#define	PF_H

class PF {
private:
    int startDay;
    int biteDay;
    int endDay;
    long p;
    int iG[10];
    int g;
    double merz;
    double grow;
    double maxD;
    double shoulder;
    double sigma;
    double delta;
    double pKill;
    double pDDef;
    double pDelta;
    double gKill;
    double iGKill[10];
public:
    void updateP(long);
    long getP() const;
    long getGrowthRate();
    long medPFDens();
    PF();
    PF(const PF& orig);
    virtual ~PF();
private:

};

#endif	/* PF_H */

