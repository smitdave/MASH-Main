/* 
 * File:   Pond.h
 * Author: amit
 *
 * Created on April 11, 2014, 3:10 PM
 */

#ifndef POND_H
#define	POND_H

class Pond {
private:
    int tt;
    double pd;
    double K;
    double gamma;
    double sigma;
    double psi;
    double wt;
    double eggs;
    double L1a;
    double L1b;
    double L2a;
    double L2b;
    double L3a;
    double L3b;
    double L4a;
    double L4b;
    double pupae;
    double emerge; 
public:
    Pond();
    Pond(const Pond& orig);
    virtual ~Pond();
private:
};

#endif	/* POND_H */

