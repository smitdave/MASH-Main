
# move the mosquito
search <- function(mosy){

  move <- get("landscape",.GlobalEnv)[[mosy$site]]$move # probabilities
  dest <- get("landscape",.GlobalEnv)[[mosy$site]]$move_id # destinations

  mosy$site <- sample(dest, 1, prob = move)

}
