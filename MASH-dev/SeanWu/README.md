# Sean Wu

## MBITES

### Issues & Work
  * `checkForResources`: need to have a version that makes dispersal probability inversely proportional to the sum of the resource search weights at the site.
  * Need to figure out how to properly handle the 1st model of oogenesis: ie; if eggs are not mature yet, the mosquito will go back to blood feeding, it should probably not produce any more eggs, but we need to set the `gravid=TRUE` flag anyway to ensure that `checkRefeed` gets called in `updateState` so that the checks for egg maturation get called and potentially a oviposition bout queued up. Check all this with DS.
