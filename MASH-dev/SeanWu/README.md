# Sean Wu

## MBITES

### Issues & Work
  * MBITES-Resting can set `search=TRUE` if the mosquito has failed a lot of bouts at one site, but this probably gets overwritten or not accounted for properly in `updateState`; need to go through with a flowchart and examine in detail all the places that set `search` field to something and see if it makes sense.
  * `PPR_a`, `PPR_b`: literally just made them up and looked at what the function does.
  * `chm_a`, `chm_b`: ditto
  * `bloodPerEgg`: cant even make up because i literally have no idea
  * check how `InAndOut` is being used; make sure that when i get to a new site there is no weird dependency on the last landing spot i was at, at a different site.
