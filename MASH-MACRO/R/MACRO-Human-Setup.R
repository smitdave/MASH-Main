





#' MACRO Human Module Setup
#'
#'
#'
#' @export
MACRO.Human.Setup <- function(){


  # trip frequency
  Human$set(which = "private",name = "tripFrequency",
            value = numeric(1), overwrite = TRUE
  )

  # trip duration
  Human$set(which = "private",name = "tripDuration",
            value = numeric(1), overwrite = TRUE
  )

}
