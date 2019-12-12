###############################################################################
#         ____  ___  ________  ______  _____________   __
#        / __ \/   |/_  __/ / / / __ \/ ____/ ____/ | / /
#       / /_/ / /| | / / / /_/ / / / / / __/ __/ /  |/ /
#      / ____/ ___ |/ / / __  / /_/ / /_/ / /___/ /|  /
#     /_/   /_/  |_/_/ /_/ /_/\____/\____/_____/_/ |_/
#
#     PATHOGEN-Generic Parameters
#     MBITES Team
#     March 2018
#
###############################################################################



#' Generic Pathogen Class
#'
#' i'm a class!
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: i'm a method!
#'
#' @section **Fields**:
#'  * field: i'm a field!
#'
#' @export
Generic_Pathogen_Parameters <- R6::R6Class(classname = "Generic_Pathogen_Parameters",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(){

                     # default values
                     private$b = 0.55
                     private$c = 0.15
                     private$human_incubation = 0L
                     private$mosquito_incubation = 11L

                     # futile.logger::flog.trace("Generic_Pathogen_Parameters being born at self: %s , private: %s",pryr::address(self),pryr::address(private))
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     # futile.logger::flog.trace("Generic_Pathogen_Parameters being killed at self: %s , private: %s",pryr::address(self),pryr::address(private))
                   } # end destructor

                 ),

                 # private members
                 private = list(

                   b = numeric(1), # mosy -> human transmission efficiency
                   c = numeric(1), # human -> mosy transmission efficiency
                   human_incubation = integer(1), # human incubation
                   mosquito_incubation = integer(1) # EIP

                 )
) # end Generic_Pathogen_Parameters class definition


# accessors

get_b_Generic_Pathogen_Parameters <- function(){
  return(private$b)
}

get_c_Generic_Pathogen_Parameters <- function(){
  return(private$c)
}

get_human_incubation_Generic_Pathogen_Parameters <- function(){
  return(private$human_incubation)
}

get_mosquito_incubation_Generic_Pathogen_Parameters <- function(){
  return(private$mosquito_incubation)
}

Generic_Pathogen_Parameters$set(which = "public",name = "get_b",
  value = get_b_Generic_Pathogen_Parameters, overwrite = TRUE
)

Generic_Pathogen_Parameters$set(which = "public",name = "get_c",
  value = get_c_Generic_Pathogen_Parameters, overwrite = TRUE
)

Generic_Pathogen_Parameters$set(which = "public",name = "get_human_incubation",
  value = get_human_incubation_Generic_Pathogen_Parameters, overwrite = TRUE
)

Generic_Pathogen_Parameters$set(which = "public",name = "get_mosquito_incubation",
  value = get_mosquito_incubation_Generic_Pathogen_Parameters, overwrite = TRUE
)

# add to package namespace (a bit hacky)
PathogenParameters <- Generic_Pathogen_Parameters$new()
