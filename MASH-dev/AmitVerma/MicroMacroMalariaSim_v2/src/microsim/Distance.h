/*
 * Distance.h
 *
 *  Created on: Apr 12, 2012
 *      Author: amit.verma
 */

#ifndef DISTANCE_H_
#define DISTANCE_H_
#include "location.h"


struct location;


class Distance {
public:
	Distance(location*, double, double);
	virtual ~Distance();
	double d;
	location* loc;
};

#endif /* DISTANCE_H_ */
