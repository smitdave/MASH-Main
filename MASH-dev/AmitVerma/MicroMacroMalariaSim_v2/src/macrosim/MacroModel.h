/* 
 * File:   MacroModel.h
 * Author: amit
 *
 * Created on April 22, 2013, 3:07 PM
 */

#ifndef MACROMODEL_H
#define	MACROMODEL_H

class Human;
class MacroModel {
public:
    virtual void calculateParameters()=0;
    virtual void updateParameters()=0;
    virtual double getTotMosquitoes()=0;
    virtual double getInfectedButNotInfectiousMosquitoes()=0;
    virtual double getInfectiousMosquitoes()=0;
    virtual void printState()=0;
    virtual void dailyUpdate(double)=0;
    virtual void updateHEpsilon(Human*)=0;
};

#endif	/* MACROMODEL_H */

