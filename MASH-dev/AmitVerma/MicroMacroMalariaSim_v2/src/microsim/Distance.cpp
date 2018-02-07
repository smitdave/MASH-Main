/*
 * Distance.cpp
 *
 *  Created on: Apr 12, 2012
 *      Author: amit.verma
 */

#include "Distance.h"

Distance::Distance(location *l, double x, double y) {
	loc = l;
	d=(x-loc->x)*(x-loc->x)+(y-loc->y)*(y-loc->y);
}
Distance::~Distance() {

}
