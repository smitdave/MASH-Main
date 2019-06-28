


make_site <- function(id,has_f,has_l,haz,move,move_id,constant,lambda=NULL){

  site <- list()

  site$move <- move
  site$move_id <- move_id

  site$haz <- haz

  # little booleans for quick checks
  site$has_f <- has_f
  site$has_l <- has_l

  if(has_f){
    site$riskQ <- make_RiskQ()
  }

  if(has_l){
    site$id_l <- 1
    site$prob_l <- 1
    site$constant <- constant
    site$lambda <- lambda
    site$ImagoQ <- make_ImagoQ()
  }

  list2env(site,hash=TRUE)
}
