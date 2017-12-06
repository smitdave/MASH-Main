/* 
 * File:   RXItem.h
 * Author: amit
 *
 */

#ifndef RXITEM_H
#define	RXITEM_H

#include "Drug.h"
#include "GlobalParameters.h"

class RXItem {
public:
    int startD;
    int endD;
    const Drug& drug;
    RXItem(const Drug&, int, int);
    //RXItem(const RXItem& orig);
    virtual ~RXItem();
private:

};

#endif	/* RXITEM_H */

