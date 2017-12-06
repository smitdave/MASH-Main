/* 
 * File:   PF.cpp
 * Author: amit
 * 
 * Created on April 11, 2014, 1:55 PM
 */

#include "PF.h"

void PF::updatePF() {
    p += outofLiver;
    if (p > 0) {
        p *= getGrowthRate();
    }
    if (p < 0) {
        p = 0;
    }
}

long PF::outOfLiver() {
    if (currentDay == startDay) {
        return pow(10, merz);
    } else return 0;
}

long PF::getGrowthRate() {

}

long PF::medPFDens() {
    int age = currentDay - startDay;
    int merlin = min(dend, end) - currentDay;
    if (age < 0 || merlin < 0) {
        P = noValue
    } else {
        if (age < shld) {
            P = min(maxD, merz + grow * age) +
                    Pkill + (PDdef - Pkill) * exp((PDlt - t) / delta)
        } else {
            P = maxD * ((end - t) / (end - strt - shld))^sigma +
                    Pkill + (PDdef - Pkill) * exp((PDlt - t) / delta)
        }
    }

    P
}

long PF::getP() const {
    return p;
}

PF::PF() {
}

PF::PF(const PF& orig) {
}

PF::~PF() {
}

