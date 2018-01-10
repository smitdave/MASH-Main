//
//  Site.cpp
//  MASH
//


#include "Site.hpp"

Site::Site(){
    //@ Void site constructor
    position={0,0,0};
    areaRepellant=AreaRepellant();
    areaSpray=AreaSpray();
}
Site::Site(int idIn, coordinate positionIn){
    //@ Site constructor with ID and Position
    id=idIn;
    position=positionIn;
}
coordinate Site::getCoordinates(){
    //@ Returns position
    return position;
}
double Site::distanceToSite(Site otherSite){
    //@ Calculates Euclidean distance between sites
    double distance = calculateEuclideanDistance(position, otherSite.position);
    return distance;
}
AreaRepellant Site::getAreaRepellant(){
    //@ Returns area repellant
    return areaRepellant;
}
AreaSpray Site::getAreaSpray(){
    //@ Returns area spray
    return areaSpray;
}
