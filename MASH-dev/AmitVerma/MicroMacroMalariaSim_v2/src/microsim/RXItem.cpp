/* 
 * File:   RXItem.cpp
 * Author: amit
 * 
 */

#include "RXItem.h"

RXItem::RXItem(const Drug& d, int sd, int ed) : drug(d) {
    startD = sd;
    endD = ed;
}

//RXItem::RXItem(const RXItem& orig) {
//}

RXItem::~RXItem() {
}

