###############################################################################
#
#       __  ____                _____ _ __
#      /  |/  (_)_____________ / ___/(_) /____
#     / /|_/ / / ___/ ___/ __ \\__ \/ / __/ _ \
#    / /  / / / /__/ /  / /_/ /__/ / / /_/  __/
#   /_/  /_/_/\___/_/   \____/____/_/\__/\___/
#
#   MICRO
#   Peri-domestic Breeding Site
#   MASH Team
#   January 2018
#
###############################################################################

###############################################################################
# Peri-Domestic Breeding Site Class Declaration
###############################################################################

#' Peri-Domestic Breeding Site Class Definition
#'
#' A peri-domestic breeding site is an aquatic habitat that is considered to be
#' part of an enclosing \code{\link{FeedingSite}}, for example a backyard pool or water container.
#' If a mosquito is at a feeding site with a peri-domestic breeding site, she does not need to enter a full
#' search bout to oviposit.
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
#' @export
periDomestic_AquaticSite <- R6::R6Class(classname = "periDomestic_AquaticSite",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   initialize = function(module, lambda = NULL, numGenotypes = 1){

                     # shared Aquatic Ecology fields
                     private$ImagoQ = MASHcpp::ImagoQ()

                     # Aquatic Ecology Emerge module fields
                     switch(module,
                       emerge = {
                         private$lambda = lambda
                        },
                        EL4P = {
                          private$numGenotypes = numGenotypes
                          private$EL4P = EL4P$new(N_genotypes = numGenotypes)
                          private$EggQ = MASHcpp::EggQ()
                          # private$EL4P = MASHcpp::EL4P(numGenotypes=numGenotypes,psi_new=0,alpha_new=0,p_new=0)
                        },
                        {stop("unrecognized Aquatic Ecology module")}
                      )

                   }

                 ),

                 # private members
                 private = list(

                   # periDomestic_AquaticSite fields
                   ImagoQ = NULL,
                   EggQ = NULL,
                   EL4P = NULL,
                   lambda = NULL,
                   numGenotypes = integer(1),

                   # Pointers
                   LandscapePointer = NULL,

                   # Vector Control
                   aerialSpray=NULL,
                   areaRepellent=NULL,
                   biologicalControl=NULL,
                   larviciding=NULL,
                   ovitrap=NULL
                 )
)


###############################################################################
# Class Methods
###############################################################################

#' Clear EggQ
#'
#' Clear the egg queue at this site, an object of class \code{\link[MASHcpp]{EggQ}}
#'  * This method is bound to \code{periDomestic_AquaticSite$clear_EggQ}
#'
clear_EggQ_periDomestic_AquaticSite <- function(){
  private$EggQ$clear_EggQ()
}

periDomestic_AquaticSite$set(which = "public",name = "clear_EggQ",
  value = clear_EggQ_periDomestic_AquaticSite, overwrite = TRUE
)

#' Clear ImagoQ
#'
#' Clear the imago queue at this site, an object of class \code{\link[MASHcpp]{ImagoQ}}
#'  * This method is bound to \code{periDomestic_AquaticSite$clear_ImagoQ}
#'
clear_ImagoQ_periDomestic_AquaticSite <- function(){
  private$ImagoQ$clear_ImagoQ()
}

periDomestic_AquaticSite$set(which = "public",name = "clear_ImagoQ",
  value = clear_ImagoQ_periDomestic_AquaticSite, overwrite = TRUE
)

#' Get ImagoQ
#'
#' Get mosquito imago queue of emerging adults, object of class \code{\link[MASHcpp]{ImagoQ}}
#'  * This method is bound to \code{periDomestic_AquaticSite$get_ImagoQ}
#'
get_ImagoQ_periDomestic_AquaticSite <- function(){return(private$ImagoQ)}

periDomestic_AquaticSite$set(which = "public",name = "get_ImagoQ",
  value = get_ImagoQ_periDomestic_AquaticSite, overwrite = TRUE
)

#' Set ImagoQ
#'
#' Set mosquito imago queue of emerging adults, object of class \code{\link[MASHcpp]{ImagoQ}}
#'  * This method is bound to \code{periDomestic_AquaticSite$set_ImagoQ}
#'
#' @param ImagoQ object of class \code{\link[MASHcpp]{ImagoQ}}
#'
set_ImagoQ_periDomestic_AquaticSite <- function(ImagoQ){private$ImagoQ = ImagoQ}

periDomestic_AquaticSite$set(which = "public",name = "set_ImagoQ",
  value = set_ImagoQ_periDomestic_AquaticSite, overwrite = TRUE
)

#' Get EggQ
#'
#' Get mosquito egg batch queue, object of class \code{\link[MASHcpp]{EggQ}}
#'  * This method is bound to \code{periDomestic_AquaticSite$get_EggQ}
#'
get_EggQ_periDomestic_AquaticSite <- function(){return(private$EggQ)}

periDomestic_AquaticSite$set(which = "public",name = "get_EggQ",
  value = get_EggQ_periDomestic_AquaticSite, overwrite = TRUE
)

#' Set EggQ
#'
#' Set mosquito egg batch queue, object of class \code{\link[MASHcpp]{EggQ}}
#'  * This method is bound to \code{periDomestic_AquaticSite$set_EggQ}
#'
#' @param EggQ object of class \code{\link[MASHcpp]{EggQ}}
#'
set_EggQ_periDomestic_AquaticSite <- function(EggQ){private$EggQ = EggQ}

periDomestic_AquaticSite$set(which = "public",name = "set_EggQ",
  value = set_EggQ_periDomestic_AquaticSite, overwrite = TRUE
)

#' Get EL4P
#'
#' Get aquatic populations, object of class \code{\link[MASHcpp]{EL4P}}
#'  * This method is bound to \code{periDomestic_AquaticSite$get_EL4P}
#'
get_EL4P_periDomestic_AquaticSite <- function(){return(private$EL4P)}

periDomestic_AquaticSite$set(which = "public",name = "get_EL4P",
  value = get_EL4P_periDomestic_AquaticSite, overwrite = TRUE
)

#' Set EL4P
#'
#' Set aquatic populations, object of class \code{\link[MASHcpp]{EL4P}}
#'  * This method is bound to \code{periDomestic_AquaticSite$set_EL4P}
#'
#' @param EL4P object of class \code{\link[MASHcpp]{EL4P}}
#'
set_EL4P_periDomestic_AquaticSite <- function(EL4P){private$EL4P = EL4P}

periDomestic_AquaticSite$set(which = "public",name = "set_EL4P",
  value = set_EL4P_periDomestic_AquaticSite, overwrite = TRUE
)

#' Get lambda
#'
#' Get vector of lambda of mean daily emergence at this site
#'  * This method is bound to \code{periDomestic_AquaticSite$get_lambda}
#'
get_lambda_periDomestic_AquaticSite <- function(){return(private$lambda)}

periDomestic_AquaticSite$set(which = "public",name = "get_lambda",
  value = get_lambda_periDomestic_AquaticSite, overwrite = TRUE
)

#' Set lambda
#'
#' Set vector of lambda of mean daily emergence at this site
#'  * This method is bound to \code{periDomestic_AquaticSite$set_lambda}
#'
#' @param lambda numeric(365)
#'
set_lambda_periDomestic_AquaticSite <- function(lambda){private$lambda = lambda}

periDomestic_AquaticSite$set(which = "public",name = "set_lambda",
  value = set_lambda_periDomestic_AquaticSite, overwrite = TRUE
)

#' Get numGenotypes
#'
#' Get number of genotypes.
#'  * This method is bound to \code{periDomestic_AquaticSite$get_numGenotypes}
#'
get_numGenotypes_periDomestic_AquaticSite <- function(){return(private$numGenotypes)}

periDomestic_AquaticSite$set(which = "public",name = "get_numGenotypes",
  value = get_numGenotypes_periDomestic_AquaticSite, overwrite = TRUE
)

#' Get \code{\link{Landscape}} Pointer
#'
#' Return \code{Landscape$self} enclosing this site.
#'  * This method is bound to \code{periDomestic_AquaticSite$get_LandscapePointer}
#'
get_LandscapePointer_periDomestic_AquaticSite <- function(){return(private$LandscapePointer)}

periDomestic_AquaticSite$set(which = "public",name = "get_LandscapePointer",
  value = get_LandscapePointer_periDomestic_AquaticSite, overwrite = TRUE
)

#' Set \code{\link{Landscape}} Pointer
#'
#' Set the pointer \code{Landscape$self} enclosing this site.
#'  * This method is bound to \code{periDomestic_AquaticSite$set_LandscapePointer}
#'
#' @param LandscapePointer an environment
#'
set_LandscapePointer_periDomestic_AquaticSite <- function(LandscapePointer){private$LandscapePointer = LandscapePointer}

periDomestic_AquaticSite$set(which = "public",name = "set_LandscapePointer",
  value = set_LandscapePointer_periDomestic_AquaticSite, overwrite = TRUE
)
