###############################################################################
#         ____  ___  ________  ______  _____________   __
#        / __ \/   |/_  __/ / / / __ \/ ____/ ____/ | / /
#       / /_/ / /| | / / / /_/ / / / / / __/ __/ /  |/ /
#      / ____/ ___ |/ / / __  / /_/ / /_/ / /___/ /|  /
#     /_/   /_/  |_/_/ /_/ /_/\____/\____/_____/_/ |_/
#
#     PATHOGEN-Generic Pedigree
#     MBITES Team
#     March 2018
#
###############################################################################

#' PATHOGEN Generic Pedigree
#'
#' This class is a singleton object in the \code{MBITES} package namespace that stores global values needed for generic pathogen simulation.
#' It can be accessed by \code{MBITES:::Pedigree}.
#'
#'
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * field: im a field!
#'
Generic_Pedigree <- R6::R6Class(classname = "Generic_Pedigree",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::HashMap,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(){
                     futile.logger::flog.trace("Generic_Pedigree being born at self: %s , private: %s",pryr::address(self),pryr::address(private))
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     futile.logger::flog.trace("Generic_Pedigree being killed at self: %s , private: %s",pryr::address(self),pryr::address(private))
                   } # end destructor

                 ), # end public members

                 # private members
                 private = list(

                   pathogen_id = 0L

                 ) # end private members
) # end Generic_Pedigree class definition


get_pathogen_id_Generic_Pedigree <- function(){
  private$pathogen_id = private$pathogen_id + 1L
  return(private$pathogen_id)
}

Generic_Pedigree$set(which = "public",name = "get_pathogen_id",
  value = get_pathogen_id_Generic_Pedigree, overwrite = TRUE
)


# assign to MBITES namespace (a bit hacky)
Pedigree <- Generic_Pedigree$new()
