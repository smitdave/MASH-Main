###############################################################################
#       ___
#      /   | ____ ___  ______ _
#     / /| |/ __ `/ / / / __ `/
#    / ___ / /_/ / /_/ / /_/ /
#   /_/  |_\__, /\__,_/\__,_/
#            /_/
#
#   MASH-MACRO
#   AquaPop Emerge Class Definition
#   MASH Team
#   November 2017
#
###############################################################################


#' Aquatic Population Emerge Base Class Definition
#'
#' This class inherits form \code{\link{AquaPop_Base}}, this documentation only records
#' overridden or new methods & fields, please see the base class documentation for more details.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * lambda: numeric vector of mean adult female emergence of length 365
#'
#' @section **Methods**:
#'  * oneDay_popDynamics: see \code{\link{oneDay_popDynamics_AquaPop_Emerge}}
#'
#' @section **Fields**:
#'  * lambda: numeric vector of mean adult female emergence of length 365
#'
#' @md
#' @export
AquaPop_Emerge <- R6::R6Class(classname = "AquaPop_Emerge",
                 inherit = AquaPop_Base,
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   #################################################
                   # Constructor
                   #################################################

                   initialize = function(lambda){
                     if(length(lambda)!=365){stop("AquaPop_Emerge must be initialized with length 365 lambda vector")}
                     private$ImagoQ = MASHcpp::ImagoQ()
                     private$lambda = lambda
                   }

                  ),

                  # private methods & fields
                  private = list(

                    # seasonal lambda
                    lambda                = numeric(365)

                  )

) #end class definition


#' Run Emerge Daily Population Dynamics
#'
#' Calculate daily emerging adults with Poisson dispersion around mean value of lambda for this day and add them to the \code{\link[MASHcpp]{ImagoQ}}
#'
#'  * This method is bound to \code{AquaPop_Emerge$oneDay_popDynamics}
#'
oneDay_popDynamics_AquaPop_Emerge <- function(){
  tNow = private$PatchPointer$get_TilePointer()$get_tNow()
  lambda = private$lambda[floor(tNow)%%365+1]
  adults = rpois(n=1,lambda=lambda)
  private$ImagoQ$add_ImagoQ(N_new=adults,tEmerge_new=tNow,genotype_new=-1L)
}

AquaPop_Emerge$set(which = "public",name = "oneDay_popDynamics",
  value = oneDay_popDynamics_AquaPop_Emerge, overwrite = TRUE
)


#' Reset the Aquatic Population
#'
#' Reset this aquatic population between simulation runs
#'
#'  * This method is bound to \code{AquaPop_Emerge$reset}
#'
reset_AquaPop_Emerge <- function(){
  private$ImagoQ$clear_ImagoQ()
}

AquaPop_Emerge$set(which = "public",name = "reset",
  value = reset_AquaPop_Emerge, overwrite = TRUE
)
