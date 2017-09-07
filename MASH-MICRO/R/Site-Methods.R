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
set_ix <- function(ix){private$ix = ix}

FeedingSite$set(which = "public",name = "get_ix",
  value = get_ix_FeedingSite, overwrite = TRUE
)


#' Get siteXY
#'
#' Get site coordinates \code{siteXY}
#'  * This method is bound to \code{FeedingSite$get_siteXY}
#'
get_siteXY <- function(){return(private$siteXY)}

#' Set siteXY
#'
#' Set site coordinates \code{siteXY}
#'  * This method is bound to \code{FeedingSite$set_siteXY}
#'
#' @param ix numeric(2)
#'
set_siteXY <- function(newSiteXY){private$siteXY = siteXY}

#' Get searchWt
#'
#' Get site search weight \code{searchWt}
#'  * This method is bound to \code{FeedingSite$get_searchWt}
#'
get_searchWt <- function(){return(private$searchWt)}

#' Set searchWt
#'
#' Set site search weight \code{searchWt}
#'  * This method is bound to \code{FeedingSite$set_searchWt}
#'
#' @param ix numeric
#'
set_searchWt <- function(searchWt){private$searchWt = searchWt}

#' Get siteType
#'
#' Get site type \code{siteType}; 1 is peri-domestic, 0 is non peri-domestic.
#'  * This method is bound to \code{FeedingSite$get_siteType}
#'
get_siteType <- function(){return(private$siteType)}

#' Set siteType
#'
#' Set site type \code{siteType}; 1 is peri-domestic, 0 is non peri-domestic.
#'  * This method is bound to \code{FeedingSite$set_siteType}
#'
#' @param siteType integer
#'
set_siteType <- function(siteType){priate$siteType = siteType}


# vegetation hazards
get_hazV <- function(){return(private$hazV)}
set_hazV <- function(hazV){private$hazV = hazV}

# outside wall hazards
get_hazW <- function(){return(private$hazW)}
set_hazW <- function(hazW){private$hazW = hazW}

# inside wall hazards
get_hazI <- function(){return(private$hazI)}
set_hazI <- function(hazI){private$hazI = hazI}

# get hazard based on lspot
get_hazLspot <- function(lspot){
  switch(lspot,
     i = {return(private$hazI)},
     w = {return(private$hazW)},
     v = {return(private$hazV)},
     r = {return(0)},
     l = {return(0)}
   )
}

# house entry probability
get_enterP <- function(){return(private$enterP)}
set_enterP <- function(enterP){private$enterP = enterP}

# host risk queue
get_RiskQ <- function(){return(private$RiskQ)}
set_RiskQ <- function(RiskQ){private$RiskQ = RiskQ}

clear_RiskQ <- function(){
  private$RiskQ$clear_HumanHost()
}


# landscape pointer
get_LandscapePointer <- function(){return(private$LandscapePointer)}
set_LandscapePointer <- function(LandscapePointer){private$LandscapePointer = LandscapePointer}


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
