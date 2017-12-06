/*
 * Mosquito.cpp
 *
 */

#include <cstdlib>
#include "Mosquito.h"
#include <iostream> //EDITED 03/19/2010

using namespace std;

#define NOT_AT_HOUSE -1

Mosquito::Mosquito(int mid, int bd, int dd, int paid, int poid, double prRestIn, double prAliveRest, double prAliveOvi, int rdays, int ovidays, int sporodays) { //TD
    //mozzyID = mid;
	mozzyID = mid;
    bday = bd;
    dday = dd;
    patch = findPatch(paid);
    currentLocation = patch->findPond(poid)->loc;
    //pond = patch->findPond(poid);
    //house = NULL;
/*  patchID = paid;
    pondID = poid;
    houseID = NOT_AT_HOUSE;*/
    isInfected = false;//TD added
    probRestIndoor = prRestIn;
    restDays = rdays;
    ovipositDays = ovidays;
    probAliveAtRest = prAliveRest;
    probAliveAtOviposit = prAliveOvi;
    restStartDay = NOT_RESTING;
    whereResting = NOT_RESTING;
    ovipositStartDay = -1;
    searchPatch = NULL;
    ySearch = currentLocation->pond->coordY;
    nextActionDay = (double)currentDay;
    dayUsed = 0.0;
    stage = 1;
    dayNextStage = 0;
    infectionStartDay = -1;
    sporogonyDays = sporodays;
}


Mosquito::~Mosquito() {
}

bool Mosquito::searchHouse (vector<Patch*> &patches) {
	
	for(int i=0; i< searchPatch->houses.size(); i++) {

		House *hs = searchPatch->houses[i];
		//double probMatch = hs.hMozzyAttractIndex * hs.Q * hs.itnEffect() * hs.irsEffect();
		//cout<<endl<<"search patchid: "<<searchPatchID<<"\t"<<"house id: "<<hs.houseID; //EDITED 03/19/2010
		double probMatch = hs->hMozzyAttractIndex ;
		double unifRand = (double)rand()/(double)RAND_MAX;
		if(unifRand < probMatch) {
			//cout<<"\n probMatch = "<<probMatch <<"  unifRand = " <<unifRand <<endl;
			//cout<<"\nFound Match"; //EDITED 03/19/2010
			//houseID = patches[searchPatchID]->houses[i]->houseID;
			currentLocation = searchPatch->houses[i]->loc;
			if(biteHuman(hs)){
				// set state to resting
				double unifRand = (double)rand()/(double)RAND_MAX;
				if (unifRand < probRestIndoor)
					whereResting = RESTING_INDOOR;
				else
					whereResting = RESTING_OUTDOOR;
				restStartDay = currentDay;

				return true;
			}
		}
	}
	return false;

}
bool Mosquito::searchHouse () {
    //cout<<"SH ";
	//cout<<"test13"<<endl;
    House *hs = currentLocation->house;
    		//cout<<"\nhouse id: "<<hs->houseID<<endl;
            if(biteHuman(hs)){
                    // set state to resting
                    double unifRand = (double)rand()/(double)RAND_MAX;
                    if (unifRand < probRestIndoor)
                            whereResting = RESTING_INDOOR;
                    else
                            whereResting = RESTING_OUTDOOR;
                    restStartDay = currentDay;

                    return true;
            }
    return false;
}
bool Mosquito::fly(bool searchHouse, bool reflect, double delta, double k, double time){
	// Choose a direction
	bool right = rand()%2;	
	
	// Set i to mosquitos location in sorted locations vector and ySearchLine to take into account todays wind
	int i;
	double ySearchLine;
	if(currentLocation->isHouse) i = currentLocation->house->locationVector;
	else if (currentLocation->isPond) i = currentLocation->pond->locationVector;
	else{
           // cout<<"\n"<<"not house not pond";
		for(i = 0; i < sortedLocations[windForecast].size(); i++){
                    if(currentLocation->x > sortedLocations[windForecast][i]->x){
                        if(i == 0 && !reflect) i = sortedLocations[windForecast].size()-1;
                        break;
                    }
		}
	}
        i--;
	ySearchLine = sin(windForecast)*currentLocation->x + cos(windForecast)*currentLocation->y;

        //cout<<"\ni: "<<i;
	do{
           // cout<<"\nreflect: "<<reflect;
           // cout<<"\nright: "<<right;
           // cout<<"\t"<<sortedLocations[windForecast].size()-1;
		// Boundary condition
		if(reflect && ((right && (i == sortedLocations[windForecast].size()-1)) || (!right && (i <= 0)))) right = !right;
		else if(!reflect && right && (i == sortedLocations[windForecast].size()-1)) i = 0;
		else if(!reflect && !right && (i <= 0)) i = sortedLocations[windForecast].size()-1;
                //cout<<"\nright: "<<right;
		// Move along sorted locations vector
		if(right) i++;
		else i--;
	
		// Update currentLocation to location under consideration
		currentLocation = sortedLocations[windForecast][i];
		
		// Search time
		dayUsed += time;

              //  cout<<"\nsach batao....";
              //  cout<<"\n"<<currentLocation.isPond;
              //    cout<<"\t"<<currentLocation.isHouse;
              //    cout<<"\t"<<windForecast<<"\t"<<i;
		
		// Skip if looking at a different type of location
		if(searchHouse && !currentLocation->isHouse) continue;
		else if(!searchHouse && currentLocation->isHouse) continue;

		// Calculate y relative to wind for destination 	
		double y = sin(windForecast)*currentLocation->x + cos(windForecast)*currentLocation->y;
		
		// Skip if y of destination is below mosquitos current line of search
		if(y < ySearchLine) continue;
		
		// Get y of destination relative to current location
		y = abs(y - ySearchLine);
		
		// Get probability of choosing this location as the next destination
                if(currentLocation->house==NULL) {
                   // cout<<"\ncurrentLocation.house=null";
                }/*
                if(currentLocation.house->hMozzyAttractIndex==NULL) {
                    cout<<"\ncurrentLocation.house->hMozzyAttractIndex=null";
                }
                if(currentLocation.house->hMozzySearchEfficiency==NULL) {
                    cout<<"\ncurrentLocation.house->hMozzySearchEfficiency=null";
                }
*/              else {
            //cout<<"\ncurrentLocation.house good!";
            //cout<<"\n"<<currentLocation.isPond;
            //cout<<"\n"<<currentLocation.isHouse;
            //cout<<"\nbuffer";
            //cout<<"\n"<<currentLocation.house->hMozzyAttractIndex;
            //cout<<"\n"<<currentLocation.house->hMozzySearchEfficiency;
        }
        double prbChoose;
        //if(searchHouse) prbChoose = currentLocation.house->hMozzyAttractIndex*exp(-delta*(pow(y/currentLocation.house->hMozzySearchEfficiency, k)));
        //else prbChoose = currentLocation.pond->pMozzyAttractIndex*exp(-delta*(pow(y/currentLocation.pond->pMozzySearchEfficiency, k)));
        if (searchHouse) prbChoose = currentLocation->house->hMozzyAttractIndex * 1000; //*(1/(1+0.000000000001*delta*(pow(y/currentLocation.house->hMozzySearchEfficiency, k))));
        else prbChoose = currentLocation->pond->pMozzyAttractIndex * 1000; //*(1/(1+0.000000000001*delta*(pow(y/currentLocation.pond->pMozzySearchEfficiency, k))));

        // If choose house return the house
        //cout<<"\n"<<prbChoose;
        if ((double) rand() / (double) RAND_MAX < prbChoose) return true;
        //cout<<"\n"<<dayUsed;

    } while (dayUsed < 1);

    // Get house currently sensing x coordinate
    double nearWindX = cos(windForecast) * currentLocation->x - sin(windForecast) * currentLocation->y;
    // Set location to x coordinate of house currently sensing and y coordinate of ySearceLine (location started at today)
    //NEED FIX
    //currentLocation.setLocation((cos(-windForecast) * nearWindX - sin(-windForecast) * ySearchLine), (sin(-windForecast) * nearWindX + cos(-windForecast) * ySearchLine));

    return false;
}

bool Mosquito::flyToHouse(double timePerSearch, int searchAlgo) {
	if (searchAlgo==1) {
			return fly(true,true,0.1,1,timePerSearch);
	}
	do {
		dayUsed = dayUsed + timePerSearch;

		//cout<<"\ntest7"<<endl;
		if (currentLocation->isHouse) {
			//currentLocation.setLocation(currentLocation.house->nearestHouses[(int)(((double) rand() / (double) RAND_MAX)*nSearchHouse)]->loc->house,NULL);
			currentLocation = currentLocation->house->nearestHouses[(int)(((double) rand() / (double) RAND_MAX)*nSearchHouse)]->loc;
		}
		if (currentLocation->isPond) {
			//cout<<"\ntest8"<<endl;
			int ab = (int)(((double) rand() / (double) RAND_MAX)*nSearchHouse);
			//cout<<"Pond id: "<<currentLocation->pond->pondID<<" ab: "<<ab<<endl;
			//cout<<"ab details: "<<currentLocation->pond->nearestHouses[ab]->loc->house->houseID<<endl;
			//currentLocation.setLocation(currentLocation.pond->nearestHouses[ab]->loc->house,NULL);
			currentLocation = currentLocation->pond->nearestHouses[ab]->loc;
			//cout<<"\ntest9"<<endl;
		}
		//cout<<"\ndayUsed: "<<dayUsed<<endl;
		double prbChoose = currentLocation->house->hMozzyAttractIndex * 1000;
		//cout<<"\ntest10"<<endl;
		if ((double) rand() / (double) RAND_MAX < prbChoose) {
			//cout<<"\nhouse id: "<<currentLocation->house->houseID<<endl;
			//cout<<"\ntest11"<<endl;
			return true;
		}

	}while(dayUsed<1.0);
	return false;
}

bool Mosquito::flyToPond(double timePerSearch, int searchAlgo) {
	if (searchAlgo==1) {
		return fly(false,true,0.1,1);
	}
	do {
		dayUsed = dayUsed + timePerSearch;
		//currentLocation.setLocation(NULL,currentLocation.house->nearestPonds[(int)(((double) rand() / (double) RAND_MAX)*nSearchPond)]->loc->pond);
		
		
//		cout<<"\n";
//		cout<<"    mozzyID: "<<mozzyID;
//		cout<<"    nSearchPond: "<<nSearchPond;
		int selp = (int)(((double) rand() / (double) RAND_MAX)*nSearchPond);
//		cout<<"    sel pond indx: "<<selp<<endl;
//		cout<<"    current house: "<<currentLocation->house->houseID<<endl;
//		if (currentLocation->pond!=NULL) cout<<"    current pond: "<<currentLocation->pond->pondID<<endl;
//		cout<<"    patchID: "<<currentLocation->house->nearestPonds[selp]->loc->pond->patch->patchID<<endl;
//		cout<<"    pondID: "<<currentLocation->house->nearestPonds[selp]->loc->pond->pondID<<endl;
		
		
		//currentLocation = currentLocation->house->nearestPonds[(int)(((double) rand() / (double) RAND_MAX)*nSearchPond)]->loc;
		double prbChoose = currentLocation->pond->pMozzyAttractIndex * 1000;
		if ((double) rand() / (double) RAND_MAX < prbChoose) {
			currentLocation = currentLocation->house->nearestPonds[selp]->loc;
			return true;
		}
	}while(dayUsed<1.0);
	return false;
}


bool Mosquito::biteHuman(House *hs) {
    for (int i = 0; i < hs->nHumans; i++) {

        //double probBite = hs.humans[i].biteWeight * hs.humans[i].atRisk;
        double probBite = hs->humans[i]->biteWeight * 1000;

        if (hs->humans[i]->house->houseID == hs->houseID)
            probBite = probBite * 0.9;
        else
            probBite = probBite * 0.1;

        if (hs->humans[i]->itn)
            probBite = probBite * hs->humans[i]->itnUse;

        double unifRand = (double) rand() / (double) RAND_MAX;
        //cout << "\n probBite = "<<probBite <<" unifRand = "<<unifRand<< endl;

        if (unifRand < probBite) {
        	biteLog <<"\n"<<currentDay<<" "<<mozzyID<<" "<<isInfected<<" "<<hs->humans[i]->humanID<<" "<<hs->humans[i]->isInfected<<" ";
        	biteLog <<hs->houseID<<" "<<hs->patch->patchID<<" ";
        	nBites++;
            hs->humans[i]->nBites++;
            //cout<<"\n\n\n\n++*+*+*+*+*+*"<<hs.humans[i].nBites<<"+*+*+*+**++*+*+*+\n\n\n\n";
            if (isInfected) { // is mozzy infected
                hs->humans[i]->nInfectiousBites++;
                if(hs->humans[i]->isInfected)
                	mihi++;
                else mihu++;
                unifRand = (double) rand() / (double) RAND_MAX;
                if (unifRand < hs->humans[i]->rateGetInfected){
                    hs->humans[i]->receiveInfection();
                    biteLog << 1;
                }
                else biteLog << 0;
            } else {
                if (hs->humans[i]->isInfected) {
                	muhi++;
                    unifRand = (double) rand() / (double) RAND_MAX;
                    if (unifRand < hs->humans[i]->infectivity) {
                        infectionStartDay = currentDay + sporogonyDays;
                    	muhi_it++;
                    	biteLog << 2;
                    }
                    else biteLog << 0;
                }
                else {
                	muhu++;
                	biteLog << 0;
                }
            }
            //cout<<"bite ";
            return true;
        }
    }
    return false;
}

double Mosquito::getPrDeathAtRest(vector<Patch*> &patches) {
    double probDeath = 0.0;
    if (whereResting == 0) {
        probDeath = 1 - probAliveAtRest;
    }
    if (whereResting == 1) {
        //probDeath = 1 - probAliveAtRest * patches[searchPatchID]->houses[houseID]->irsEffect();
        probDeath = 1 - probAliveAtRest * currentLocation->house->irsEffect();
    }
    return probDeath;
}

double Mosquito::getPrDeathAtRest() {
    double probDeath = 0.0;
    if (whereResting == 0) {
        probDeath = 1 - probAliveAtRest;
    }
    if (whereResting == 1) {
        //probDeath = 1 - probAliveAtRest * patches[searchPatchID]->houses[houseID]->irsEffect();
        probDeath = 1 - probAliveAtRest * currentLocation->house->irsEffect();
    }
    return probDeath;
}
