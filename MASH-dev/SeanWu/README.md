# Sean Wu

## MBITES

### Issues & Work
  * MBITES-Resting can set `search=TRUE` if the mosquito has failed a lot of bouts at one site, but this probably gets overwritten or not accounted for properly in `updateState`; need to go through with a flowchart and examine in detail all the places that set `search` field to something and see if it makes sense.
