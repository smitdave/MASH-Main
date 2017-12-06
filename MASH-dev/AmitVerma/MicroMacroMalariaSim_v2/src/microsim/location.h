#ifndef LOCATION_H_
#define LOCATION_H_

#include <iostream>
#include <iomanip>
#include<cstdlib>
#include "House.h"
#include "Pond.h"

class House;
class Pond;

/// A structure to hold location information.
/**
 * A location could either be a house or a pond
 */
struct location{
private:
	void setLocation(House *h, Pond *p);
public:
	bool isHouse;   ///< True if current location is a House
	bool isPond;    ///< True if current location is a Pond
	double x;    ///< x coordinate
	double y;    ///< y coordinate
	union{
		House* house;
		Pond* pond;
	};
	//location(double coordX=0, double coordY=0);
	location(House *h, Pond *p);
	//void setLocation(double coordX = 0, double coordY= 0);
};

#endif 
