





#' MACRO Human Module Setup
#'
#'
#'
#' @export
MACRO.Human.Setup <- function(){

  # my home patch
  Human$set(which = "private",name = "home_patchID",
            value = integer(1), overwrite = TRUE
  )

  # trip frequency
  Human$set(which = "private",name = "tripFrequency",
            value = numeric(1), overwrite = TRUE
  )

  # trip duration
  Human$set(which = "private",name = "tripDuration",
            value = numeric(1), overwrite = TRUE
  )

}
