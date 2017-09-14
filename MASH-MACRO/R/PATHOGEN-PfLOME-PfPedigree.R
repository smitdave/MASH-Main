###############################################################################
#       ____        __  __
#      / __ \____ _/ /_/ /_  ____  ____ ____  ____
#     / /_/ / __ `/ __/ __ \/ __ \/ __ `/ _ \/ __ \
#    / ____/ /_/ / /_/ / / / /_/ / /_/ /  __/ / / /
#   /_/    \__,_/\__/_/ /_/\____/\__, /\___/_/ /_/
#                               /____/
#
#   MASH-MACRO
#   MACRO: PfLOME Pedigree Class
#   MASH-MACRO Team
#   September 14, 2017
#
###############################################################################


###############################################################################
# Class Definition
###############################################################################

#' PfLOME Pedigree Class Definition
#'
#' write me.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: write me
#'
#' @section **Methods**:
#'  * get_something: write me
#'  * set_something: write me
#'
#' @section **Fields**:
#'  * field: write me
#'
#'
#'
#'
#'
#'
#'
#'
#' @export
PfLOME_Pedigree <- R6::R6Class(classname = "PfLOME_Pedigree",
                    portable = TRUE,
                    cloneable = FALSE,
                    lock_class = FALSE,
                    lock_objects = FALSE,

                    # public members
                    public = list(

                      #################################################
                      # Constructor
                      #################################################

                      initialize = function(N = 100L){

                        private$Pedigree = MASHcpp::HashMap$new(N = N)

                      }

                    ),

                    # private members
                    private = list(

                      # fields
                      Pedigree = NULL


                    )
)


###############################################################################
# Class Methods
###############################################################################

#' PfLOME_Pedigree: Get Pedigree
#'
#' Return \code{private$Pedigree}
#'
#'
get_Pedigree_PfLOME <- function(){
  return(private$Pedigree)
}

PfLOME_Pedigree$set(which = "public",name = "get_Pedigree",
  value = get_Pedigree_PfLOME, overwrite = TRUE
)
