
# move the mosquito
search <- function(mosy){

  # # potentially different movement from my site depending if i'm looking for blood or habitats
  # if(mosy$state == "F"){
  #   move <- get("landscape",.GlobalEnv)[[mosy$site]]$move_2F # probabilities
  #   dest <- get("landscape",.GlobalEnv)[[mosy$site]]$move_2F_id # destinations
  # } else if(mosy$state == "B"){
  #   move <- get("landscape",.GlobalEnv)[[mosy$site]]$move_2L # probabilities
  #   dest <- get("landscape",.GlobalEnv)[[mosy$site]]$move_2L_id # destinations
  # } else {
  #   stop("mosquito ",mosy$id," has illegal state value (in search): ",mosy$state,"\n")
  # }

  move <- get("landscape",.GlobalEnv)[[mosy$site]]$move # probabilities
  dest <- get("landscape",.GlobalEnv)[[mosy$site]]$move_id # destinations

  mosy$site <- sample(dest, 1, prob = move)

}
