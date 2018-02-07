
#include "location.h"

using namespace std;

class House;
class Pond;

//location::location(double coordX, double coordY){
//    /**
//     *
//     * @param coordX double variable to set x coordinate
//     * @param coordY double variable to set y coordinate
//     */
//	setLocation(coordX, coordY);
//}
location::location(House *h, Pond *p){
	setLocation(h, p);
}
void location::setLocation(House *h, Pond *p){
	if((h==NULL && p==NULL) || (h!=NULL && p!=NULL)){
		cout<<"\nError! Locations assigned both house and pond pointers!";
		exit(1);
	}
	else if(p==NULL){
		house = h;
		x = h->coordX;
		y = h->coordY;
		isHouse = true;
		isPond = false;
	}
	else{
		pond = p;
		x = p->coordX;
		y = p->coordY;
		isPond = true;
		isHouse = false;
	}
}


//void location::setLocation(double coordX, double coordY){
     /**
     *
     * @param coordX double variable to set x coordinate
     * @param coordY double variable to set y coordinate
     */
//	x = coordX;
//	y = coordY;
//	isHouse=NULL;
//	isPond=NULL;
//}
