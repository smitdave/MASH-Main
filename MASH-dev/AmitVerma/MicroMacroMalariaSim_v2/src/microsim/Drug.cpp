/* 
 * File:   Drug.cpp
 * Author: amit
 * 
 */

#include "Drug.h"

using namespace std;

std::string Drug::getName() const {
    return name;
}

unsigned Drug::getTMax() const {
    return tMax;
}

void Drug::setTMax(const unsigned& tm) {
    tMax = tm;
}

double Drug::getPKill(const unsigned& i) const {
    try {
        return pKill[i];
    } catch (const std::out_of_range& oor) {
        std::cerr << "Out of Range error: " << oor.what() << '\n';
        std::cerr << "Exiting." << std::endl;
        exit(1);
    }
}

double Drug::getGKill(const unsigned& i) const {
    try {
        return gKill[i];
    } catch (const std::out_of_range& oor) {
        std::cerr << "Out of Range error: " << oor.what() << '\n';
        std::cerr << "Exiting." << std::endl;
        exit(1);
    }
}

double Drug::getIGKill(const unsigned& i, const unsigned& j) const {
    try {
        return iGKill[i][j];
    } catch (const std::out_of_range& oor) {
        std::cerr << "Out of Range error: " << oor.what() << '\n';
        std::cerr << "Exiting." << std::endl;
        exit(1);
    }
}

void Drug::print () const {
    cout << "\n\nName: " << name << endl;
    cout << "tMax: " << tMax << endl;
    cout << "pKill:";
    for (int i=0; i<pKill.size(); i++) {
        cout << " " << pKill[i];
    }
    cout << endl;
    cout << "gKill:";
    for (int i=0; i<gKill.size(); i++) {
        cout << " " << gKill[i];
    }
    cout << endl;
    cout << "iGKill:" << endl;
    for (int i=0; i<iGKill.size(); i++) {
        for (int j=0; j<iGKill[i].size(); j++) {
            cout << iGKill[i][j] << " ";
        }
        cout << endl;
    }
    cout << endl;    
}

Drug::Drug(string& n, unsigned& t, vector<double>& p, vector<double>& g, vector<vector<double> >& ig) {
    name = n;
    tMax = t;
    pKill = p;
    gKill = g;
    iGKill = ig;
}

Drug::Drug(const Drug& orig) {
}

Drug::Drug() {
}

Drug::~Drug() {
}

