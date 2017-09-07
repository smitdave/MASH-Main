###############################################################################
#
#       __  ____                _____ _ __
#      /  |/  (_)_____________ / ___/(_) /____
#     / /|_/ / / ___/ ___/ __ \\__ \/ / __/ _ \
#    / /  / / / /__/ /  / /_/ /__/ / / /_/  __/
#   /_/  /_/_/\___/_/   \____/____/_/\__/\___/
#
#   MASH-MICRO
#   MICRO: Site Class Methods
#   MASH-MICRO Team
#   September 6, 2017
#
###############################################################################

###############################################################################
# Blood Feeding Site
###############################################################################

#' Get ix
#'
#' Return site index \code{ix}
#'  * This method is bound to \code{FeedingSite$get_ix}
#'
get_ix_FeedingSite <- function(){return(private$ix)}

FeedingSite$set(which = "public",name = "get_ix",
  value = get_ix_FeedingSite, overwrite = TRUE
)

#' Set ix
#'
#' Set site index \code{ix}
#'  * This method is bound to \code{FeedingSite$set_ix}
#'
#' @param ix integer
#'
set_ix_FeedingSite <- function(ix){private$ix = ix}

FeedingSite$set(which = "public",name = "get_ix",
  value = set_ix_FeedingSite, overwrite = TRUE
)


#' Get siteXY
#'
#' Get site coordinates \code{siteXY}
#'  * This method is bound to \code{FeedingSite$get_siteXY}
#'
get_siteXY_FeedingSite <- function(){return(private$siteXY)}

FeedingSite$set(which = "public",name = "get_siteXY",
  value = get_siteXY_FeedingSite, overwrite = TRUE
)

#' Set siteXY
#'
#' Set site coordinates \code{siteXY}
#'  * This method is bound to \code{FeedingSite$set_siteXY}
#'
#' @param ix numeric(2)
#'
set_siteXY_FeedingSite <- function(newSiteXY){private$siteXY = set_siteXY}

FeedingSite$set(which = "public",name = "get_siteXY",
  value = set_siteXY_FeedingSite, overwrite = TRUE
)

#' Get searchWt
#'
#' Get site search weight \code{searchWt}
#'  * This method is bound to \code{FeedingSite$get_searchWt}
#'
get_searchWt_FeedingSite <- function(){return(private$get_searchWt)}

FeedingSite$set(which = "public",name = "get_siteXY",
  value = get_searchWt_FeedingSite, overwrite = TRUE
)

#' Set searchWt
#'
#' Set site search weight \code{searchWt}
#'  * This method is bound to \code{FeedingSite$set_searchWt}
#'
#' @param ix numeric
#'
set_searchWt_FeedingSite <- function(searchWt){private$searchWt = searchWt}

FeedingSite$set(which = "public",name = "set_searchWt",
  value = set_searchWt_FeedingSite, overwrite = TRUE
)


#' Get siteType
#'
#' Get site type \code{siteType}; 1 is peri-domestic, 0 is non peri-domestic.
#'  * This method is bound to \code{FeedingSite$get_siteType}
#'
get_siteType_FeedingSite <- function(){return(private$siteType)}

FeedingSite$set(which = "public",name = "get_siteType",
  value = get_siteType_FeedingSite, overwrite = TRUE
)

#' Set siteType
#'
#' Set site type \code{siteType}; 1 is peri-domestic, 0 is non peri-domestic.
#'  * This method is bound to \code{FeedingSite$set_siteType}
#'
#' @param siteType integer
#'
set_siteType_FeedingSite <- function(siteType){priate$siteType = siteType}

FeedingSite$set(which = "public",name = "set_siteType",
  value = set_siteType_FeedingSite, overwrite = TRUE
)


#' Get hazV
#'
#' Get landing hazard associated with vegetation at this site \code{hazV}
#'  * This method is bound to \code{FeedingSite$get_hazV}
#'
get_hazV_FeedingSite <- function(){return(private$hazV)}

FeedingSite$set(which = "public",name = "get_hazV",
  value = get_hazV_FeedingSite, overwrite = TRUE
)

#' Set hazV
#'
#' Set landing hazard associated with vegetation at this site \code{hazV}
#'  * This method is bound to \code{FeedingSite$set_hazV}
#'
#' @param hazV numeric
#'
set_hazV_FeedingSite <- function(hazV){private$hazV = hazV}

FeedingSite$set(which = "public",name = "set_hazV",
  value = set_hazV_FeedingSite, overwrite = TRUE
)


#' Get hazW
#'
#' Get landing hazard associated with outside wall at this site \code{get_hazW}
#'  * This method is bound to \code{FeedingSite$get_hazW}
#'
get_hazW_FeedingSite <- function(){return(private$hazW)}

FeedingSite$set(which = "public",name = "get_hazW",
  value = get_hazW_FeedingSite, overwrite = TRUE
)

#' Set hazW
#'
#' Set landing hazard associated with outside wall at this site \code{hazW}
#'  * This method is bound to \code{FeedingSite$set_hazW}
#'
#' @param hazW numeric
#'
set_hazW_FeedingSite <- function(hazW){private$hazW = hazW}

FeedingSite$set(which = "public",name = "set_hazW",
  value = set_hazW_FeedingSite, overwrite = TRUE
)

#' Get hazI
#'
#' Get landing hazard associated with indoor wall at this site \code{hazI}
#'  * This method is bound to \code{FeedingSite$get_hazI}
#'
get_hazI_FeedingSite <- function(){return(private$hazI)}

FeedingSite$set(which = "public",name = "get_hazI",
  value = get_hazI_FeedingSite, overwrite = TRUE
)

#' Set hazI
#'
#' Set landing hazard associated with indoor wall at this site \code{hazI}
#'  * This method is bound to \code{FeedingSite$set_hazI}
#'
#' @param hazI numeric
#'
set_hazI_FeedingSite <- function(hazI){private$hazI = hazI}

FeedingSite$set(which = "public",name = "set_hazI",
  value = set_hazI_FeedingSite, overwrite = TRUE
)

#' Get Landing Hazards
#'
#' Get landing hazard associated with one of the spots a mosquito may land at for this site.
#'  * This method is bound to \code{FeedingSite$get_hazLspot}
#'
get_hazLspot_FeedingSite <- function(lspot){
  switch(lspot,
     i = {return(private$hazI)},
     w = {return(private$hazW)},
     v = {return(private$hazV)},
     r = {return(0)},
     l = {return(0)}
   )
}

FeedingSite$set(which = "public",name = "get_hazLspot",
  value = get_hazLspot_FeedingSite, overwrite = TRUE
)

#' Get enterP
#'
#' Get house entry probability at this site \code{enterP}
#'  * This method is bound to \code{FeedingSite$get_enterP}
#'
get_enterP_FeedingSite <- function(){return(private$enterP)}

FeedingSite$set(which = "public",name = "get_enterP",
  value = get_enterP_FeedingSite, overwrite = TRUE
)

#' Set enterP
#'
#' Set landing hazard associated with indoor wall at this site \code{enterP}
#'  * This method is bound to \code{FeedingSite$set_enterP}
#'
#' @param enterP numeric
#'
set_enterP_FeedingSite <- function(enterP){private$enterP = enterP}

FeedingSite$set(which = "public",name = "set_enterP",
  value = set_enterP_FeedingSite, overwrite = TRUE
)


#' Get RiskQ
#'
#' Get the host risk queue at this site, an object of class \code{\link[MASHcpp]{RiskQ}}
#'  * This method is bound to \code{FeedingSite$get_RiskQ}
#'
get_RiskQ_FeedingSite <- function(){return(private$RiskQ)}

FeedingSite$set(which = "public",name = "get_RiskQ",
  value = get_RiskQ_FeedingSite, overwrite = TRUE
)

#' Clear RiskQ
#'
#' Clear the human risk queue at this site, an object of class \code{\link[MASHcpp]{RiskQ}}
#'  * This method is bound to \code{FeedingSite$clear_RiskQ}
#'
clear_RiskQ_FeedingSite <- function(){
  private$RiskQ$clear_HumanHost()
}

FeedingSite$set(which = "public",name = "clear_RiskQ",
  value = clear_RiskQ_FeedingSite, overwrite = TRUE
)


#' Get \code{\link{Landscape}} Pointer
#'
#' Return \code{Landscape$self} enclosing this site.
#'  * This method is bound to \code{FeedingSite$get_LandscapePointer}
#'
get_LandscapePointer_FeedingSite <- function(){return(private$LandscapePointer)}

FeedingSite$set(which = "public",name = "get_LandscapePointer",
  value = get_LandscapePointer_FeedingSite, overwrite = TRUE
)


#' Set \code{\link{Landscape}} Pointer
#'
#' Set the pointer \code{Landscape$self} enclosing this site.
#'  * This method is bound to \code{FeedingSite$set_LandscapePointer}
#'
#' @param LandscapePointer an environment
#'
set_LandscapePointer_FeedingSite <- function(LandscapePointer){private$LandscapePointer = LandscapePointer}

FeedingSite$set(which = "public",name = "set_LandscapePointer",
  value = set_LandscapePointer_FeedingSite, overwrite = TRUE
)



###############################################################################
# Aquatic Habitat Site
###############################################################################

# site index
get_ix <- function(){return(private$ix)}
set_ix <- function(ix){private$ix = ix}

# site coordinates
get_siteXY <- function(){return(private$siteXY)}
set_siteXY <- function(siteXY){private$siteXY = siteXY}

# search weight
get_searchWt <- function(){return(private$searchWt)}
set_searchWt <- function(searchWt){private$searchWt = searchWt}

# site type (1 is domestic, 0 is not peri-domestic)
get_siteType <- function(){return(private$siteType)}
set_siteType <- function(siteType){priate$siteType = siteType}

# AquaticSite

get_haz <- function(){return(private$haz)}
set_haz <- function(haz){private$haz = haz}

get_ImagoQ <- function(){return(private$ImagoQ)}
set_ImagoQ <- function(ImagoQ){private$ImagoQ = ImagoQ}

get_EggQ <- function(){return(private$EggQ)}
set_EggQ <- function(EggQ){private$EggQ = EggQ}

get_EL4P <- function(){return(private$EL4P)}
set_EL4P <- function(EL4P){private$EL4P = EL4P}

get_lambda <- function(){return(private$lambda)}
set_lambda <- function(lambda){private$lambda = lambda}

get_numGenotypes <- function(){return(private$numGenotypes)}
set_numGenotypes <- function(numGenotypes){private$numGenotypes = numGenotypes}

# landscape pointer
get_LandscapePointer <- function(){return(private$LandscapePointer)}
set_LandscapePointer <- function(LandscapePointer){private$LandscapePointer = LandscapePointer}
