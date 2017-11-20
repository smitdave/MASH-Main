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

FeedingSite$set(which = "public",name = "set_ix",
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
set_siteXY_FeedingSite <- function(newSiteXY){private$siteXY = newSiteXY}

FeedingSite$set(which = "public",name = "set_siteXY",
  value = set_siteXY_FeedingSite, overwrite = TRUE
)

#' Get searchWt
#'
#' Get site search weight \code{searchWt}
#'  * This method is bound to \code{FeedingSite$get_searchWt}
#'
get_searchWt_FeedingSite <- function(){return(private$searchWt)}

FeedingSite$set(which = "public",name = "get_searchWt",
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

#' Get ix
#'
#' Return site index \code{ix}
#'  * This method is bound to \code{AquaticSite$get_ix}
#'
get_ix_AquaticSite <- function(){return(private$ix)}

AquaticSite$set(which = "public",name = "get_ix",
  value = get_ix_AquaticSite, overwrite = TRUE
)

#' Set ix
#'
#' Set site index \code{ix}
#'  * This method is bound to \code{AquaticSite$set_ix}
#'
#' @param ix integer
#'
set_ix_AquaticSite <- function(ix){private$ix = ix}

AquaticSite$set(which = "public",name = "set_ix",
  value = set_ix_AquaticSite, overwrite = TRUE
)

#' Get siteXY
#'
#' Get site coordinates \code{siteXY}
#'  * This method is bound to \code{AquaticSite$get_siteXY}
#'
get_siteXY_AquaticSite <- function(){return(private$siteXY)}

AquaticSite$set(which = "public",name = "get_siteXY",
  value = get_siteXY_AquaticSite, overwrite = TRUE
)

#' Set siteXY
#'
#' Set site coordinates \code{siteXY}
#'  * This method is bound to \code{AquaticSite$set_siteXY}
#'
#' @param ix numeric(2)
#'
set_siteXY_AquaticSite <- function(newSiteXY){private$siteXY = newSiteXY}

AquaticSite$set(which = "public",name = "set_siteXY",
  value = set_siteXY_AquaticSite, overwrite = TRUE
)

#' Get searchWt
#'
#' Get site search weight \code{searchWt}
#'  * This method is bound to \code{AquaticSite$get_searchWt}
#'
get_searchWt_AquaticSite <- function(){return(private$searchWt)}

AquaticSite$set(which = "public",name = "get_searchWt",
  value = get_searchWt_AquaticSite, overwrite = TRUE
)

#' Set searchWt
#'
#' Set site search weight \code{searchWt}
#'  * This method is bound to \code{AquaticSite$set_searchWt}
#'
#' @param ix numeric
#'
set_searchWt_AquaticSite <- function(searchWt){private$searchWt = searchWt}

AquaticSite$set(which = "public",name = "set_searchWt",
  value = set_searchWt_AquaticSite, overwrite = TRUE
)


#' Get siteType
#'
#' Get site type \code{siteType}; 1 is peri-domestic, 0 is non peri-domestic.
#'  * This method is bound to \code{AquaticSite$get_siteType}
#'
get_siteType_AquaticSite <- function(){return(private$siteType)}

AquaticSite$set(which = "public",name = "get_siteType",
  value = get_siteType_AquaticSite, overwrite = TRUE
)

#' Set siteType
#'
#' Set site type \code{siteType}; 1 is peri-domestic, 0 is non peri-domestic.
#'  * This method is bound to \code{AquaticSite$set_siteType}
#'
#' @param siteType integer
#'
set_siteType_AquaticSite <- function(siteType){priate$siteType = siteType}

AquaticSite$set(which = "public",name = "set_siteType",
  value = set_siteType_AquaticSite, overwrite = TRUE
)


#' Get haz
#'
#' Get landing hazard associated with outside wall at this site \code{haz}
#'  * This method is bound to \code{AquaticSite$get_haz}
#'
get_haz_AquaticSite <- function(){return(private$haz)}

AquaticSite$set(which = "public",name = "get_haz",
  value = get_haz_AquaticSite, overwrite = TRUE
)

#' Set haz
#'
#' Set landing hazard associated with outside wall at this site \code{haz}
#'  * This method is bound to \code{AquaticSite$set_haz}
#'
#' @param haz numeric
#'
set_haz_AquaticSite <- function(haz){private$haz = haz}

AquaticSite$set(which = "public",name = "set_haz",
  value = set_haz_AquaticSite, overwrite = TRUE
)


#' Get ImagoQ
#'
#' Get mosquito imago queue of emerging adults, object of class \code{\link[MASHcpp]{ImagoQ}}
#'  * This method is bound to \code{AquaticSite$get_ImagoQ}
#'
get_ImagoQ_AquaticSite <- function(){return(private$ImagoQ)}

AquaticSite$set(which = "public",name = "get_ImagoQ",
  value = get_ImagoQ_AquaticSite, overwrite = TRUE
)

#' Set ImagoQ
#'
#' Set mosquito imago queue of emerging adults, object of class \code{\link[MASHcpp]{ImagoQ}}
#'  * This method is bound to \code{AquaticSite$set_ImagoQ}
#'
#' @param ImagoQ object of class \code{\link[MASHcpp]{ImagoQ}}
#'
set_ImagoQ_AquaticSite <- function(ImagoQ){private$ImagoQ = ImagoQ}

AquaticSite$set(which = "public",name = "set_ImagoQ",
  value = set_ImagoQ_AquaticSite, overwrite = TRUE
)

#' Get EggQ
#'
#' Get mosquito egg batch queue, object of class \code{\link[MASHcpp]{EggQ}}
#'  * This method is bound to \code{AquaticSite$get_EggQ}
#'
get_EggQ_AquaticSite <- function(){return(private$EggQ)}

AquaticSite$set(which = "public",name = "get_EggQ",
  value = get_EggQ_AquaticSite, overwrite = TRUE
)

#' Set EggQ
#'
#' Set mosquito egg batch queue, object of class \code{\link[MASHcpp]{EggQ}}
#'  * This method is bound to \code{AquaticSite$set_EggQ}
#'
#' @param EggQ object of class \code{\link[MASHcpp]{EggQ}}
#'
set_EggQ_AquaticSite <- function(EggQ){private$EggQ = EggQ}

AquaticSite$set(which = "public",name = "set_EggQ",
  value = set_EggQ_AquaticSite, overwrite = TRUE
)

#' Get EL4P
#'
#' Get aquatic populations, object of class \code{\link[MASHcpp]{EL4P}}
#'  * This method is bound to \code{AquaticSite$get_EL4P}
#'
get_EL4P_AquaticSite <- function(){return(private$EL4P)}

AquaticSite$set(which = "public",name = "get_EL4P",
  value = get_EL4P_AquaticSite, overwrite = TRUE
)

#' Set EL4P
#'
#' Set aquatic populations, object of class \code{\link[MASHcpp]{EL4P}}
#'  * This method is bound to \code{AquaticSite$set_EL4P}
#'
#' @param EL4P object of class \code{\link[MASHcpp]{EL4P}}
#'
set_EL4P_AquaticSite <- function(EL4P){private$EL4P = EL4P}

AquaticSite$set(which = "public",name = "set_EL4P",
  value = set_EL4P_AquaticSite, overwrite = TRUE
)

#' Get lambda
#'
#' Get vector of lambda of mean daily emergence at this site
#'  * This method is bound to \code{AquaticSite$get_lambda}
#'
get_lambda_AquaticSite <- function(){return(private$lambda)}

AquaticSite$set(which = "public",name = "get_lambda",
  value = get_lambda_AquaticSite, overwrite = TRUE
)

#' Set lambda
#'
#' Set vector of lambda of mean daily emergence at this site
#'  * This method is bound to \code{AquaticSite$set_lambda}
#'
#' @param lambda numeric(365)
#'
set_lambda_AquaticSite <- function(lambda){private$lambda = lambda}

AquaticSite$set(which = "public",name = "set_lambda",
  value = set_lambda_AquaticSite, overwrite = TRUE
)

#' Get numGenotypes
#'
#' Get number of genotypes.
#'  * This method is bound to \code{AquaticSite$get_numGenotypes}
#'
get_numGenotypes_AquaticSite <- function(){return(private$numGenotypes)}

AquaticSite$set(which = "public",name = "get_numGenotypes",
  value = get_numGenotypes_AquaticSite, overwrite = TRUE
)

#' Set numGenotypes
#'
#' Set number of genotypes.
#'  * This method is bound to \code{AquaticSite$set_numGenotypes}
#'
#' @param numGenotypes numeric(365)
#'
set_numGenotypes_AquaticSite <- function(numGenotypes){private$numGenotypes = numGenotypes}

AquaticSite$set(which = "public",name = "set_numGenotypes",
  value = set_numGenotypes_AquaticSite, overwrite = TRUE
)


#' Get \code{\link{Landscape}} Pointer
#'
#' Return \code{Landscape$self} enclosing this site.
#'  * This method is bound to \code{AquaticSite$get_LandscapePointer}
#'
get_LandscapePointer_AquaticSite <- function(){return(private$LandscapePointer)}

AquaticSite$set(which = "public",name = "get_LandscapePointer",
  value = get_LandscapePointer_AquaticSite, overwrite = TRUE
)


#' Set \code{\link{Landscape}} Pointer
#'
#' Set the pointer \code{Landscape$self} enclosing this site.
#'  * This method is bound to \code{AquaticSite$set_LandscapePointer}
#'
#' @param LandscapePointer an environment
#'
set_LandscapePointer_AquaticSite <- function(LandscapePointer){private$LandscapePointer = LandscapePointer}

AquaticSite$set(which = "public",name = "set_LandscapePointer",
  value = set_LandscapePointer_AquaticSite, overwrite = TRUE
)


###############################################################################
# Mating Site
###############################################################################

#' Get ix
#'
#' Return site index \code{ix}
#'  * This method is bound to \code{MatingSite$get_ix}
#'
get_ix_MatingSite <- function(){return(private$ix)}

MatingSite$set(which = "public",name = "get_ix",
  value = get_ix_MatingSite, overwrite = TRUE
)

#' Set ix
#'
#' Set site index \code{ix}
#'  * This method is bound to \code{MatingSite$set_ix}
#'
#' @param ix integer
#'
set_ix_MatingSite <- function(ix){private$ix = ix}

MatingSite$set(which = "public",name = "set_ix",
  value = set_ix_MatingSite, overwrite = TRUE
)

#' Get siteXY
#'
#' Get site coordinates \code{siteXY}
#'  * This method is bound to \code{MatingSite$get_siteXY}
#'
get_siteXY_MatingSite <- function(){return(private$siteXY)}

MatingSite$set(which = "public",name = "get_siteXY",
  value = get_siteXY_MatingSite, overwrite = TRUE
)

#' Set siteXY
#'
#' Set site coordinates \code{siteXY}
#'  * This method is bound to \code{MatingSite$set_siteXY}
#'
#' @param ix numeric(2)
#'
set_siteXY_MatingSite <- function(newSiteXY){private$siteXY = newSiteXY}

MatingSite$set(which = "public",name = "set_siteXY",
  value = set_siteXY_MatingSite, overwrite = TRUE
)

#' Get searchWt
#'
#' Get site search weight \code{searchWt}
#'  * This method is bound to \code{MatingSite$get_searchWt}
#'
get_searchWt_MatingSite <- function(){return(private$searchWt)}

MatingSite$set(which = "public",name = "get_searchWt",
  value = get_searchWt_MatingSite, overwrite = TRUE
)

#' Set searchWt
#'
#' Set site search weight \code{searchWt}
#'  * This method is bound to \code{MatingSite$set_searchWt}
#'
#' @param ix numeric
#'
set_searchWt_MatingSite <- function(searchWt){private$searchWt = searchWt}

MatingSite$set(which = "public",name = "set_searchWt",
  value = set_searchWt_MatingSite, overwrite = TRUE
)


#' Get siteType
#'
#' Get site type \code{siteType}; 1 is peri-domestic, 0 is non peri-domestic.
#'  * This method is bound to \code{MatingSite$get_siteType}
#'
get_siteType_MatingSite <- function(){return(private$siteType)}

MatingSite$set(which = "public",name = "get_siteType",
  value = get_siteType_MatingSite, overwrite = TRUE
)

#' Set siteType
#'
#' Set site type \code{siteType}; 1 is peri-domestic, 0 is non peri-domestic.
#'  * This method is bound to \code{MatingSite$set_siteType}
#'
#' @param siteType integer
#'
set_siteType_MatingSite <- function(siteType){priate$siteType = siteType}

MatingSite$set(which = "public",name = "set_siteType",
  value = set_siteType_MatingSite, overwrite = TRUE
)


#' Get haz
#'
#' Get landing hazard associated with outside wall at this site \code{haz}
#'  * This method is bound to \code{MatingSite$get_haz}
#'
get_haz_MatingSite <- function(){return(private$haz)}

MatingSite$set(which = "public",name = "get_haz",
  value = get_haz_MatingSite, overwrite = TRUE
)

#' Set haz
#'
#' Set landing hazard associated with outside wall at this site \code{haz}
#'  * This method is bound to \code{MatingSite$set_haz}
#'
#' @param haz numeric
#'
set_haz_MatingSite <- function(haz){private$haz = haz}

MatingSite$set(which = "public",name = "set_haz",
  value = set_haz_MatingSite, overwrite = TRUE
)

#' Get MatingQ
#'
#' Get the mating queue object at this site; see \code{\link[MASHcpp]{MatingQ}} for details.
#'  * This method is bound to \code{MatingSite$get_MatingQ}
#'
get_MatingQ_MatingSite <- function(){return(private$MatingQ)}

MatingSite$set(which = "public",name = "get_MatingQ",
  value = get_MatingQ_MatingSite, overwrite = TRUE
)

#' Clear MatingQ
#'
#' Clear the mating queue object at this site; see \code{\link[MASHcpp]{MatingQ}} for details.
#'  * This method is bound to \code{MatingSite$clear_MatingQ}
#'
clear_MatingQ_MatingSite <- function(){
  private$MatingQ$clear_MatingQ()
}

MatingSite$set(which = "public",name = "clear_MatingQ",
  value = clear_MatingQ_MatingSite, overwrite = TRUE
)


#' Get \code{\link{Landscape}} Pointer
#'
#' Return \code{Landscape$self} enclosing this site.
#'  * This method is bound to \code{MatingSite$get_LandscapePointer}
#'
get_LandscapePointer_MatingSite <- function(){return(private$LandscapePointer)}

MatingSite$set(which = "public",name = "get_LandscapePointer",
  value = get_LandscapePointer_MatingSite, overwrite = TRUE
)

#' Set \code{\link{Landscape}} Pointer
#'
#' Set the pointer \code{Landscape$self} enclosing this site.
#'  * This method is bound to \code{MatingSite$set_LandscapePointer}
#'
#' @param LandscapePointer an environment
#'
set_LandscapePointer_MatingSite <- function(LandscapePointer){private$LandscapePointer = LandscapePointer}

MatingSite$set(which = "public",name = "set_LandscapePointer",
  value = set_LandscapePointer_MatingSite, overwrite = TRUE
)


###############################################################################
# Sugar Feeding Site
###############################################################################

#' Get ix
#'
#' Return site index \code{ix}
#'  * This method is bound to \code{SugarSite$get_ix}
#'
get_ix_SugarSite <- function(){return(private$ix)}

SugarSite$set(which = "public",name = "get_ix",
  value = get_ix_SugarSite, overwrite = TRUE
)

#' Set ix
#'
#' Set site index \code{ix}
#'  * This method is bound to \code{SugarSite$set_ix}
#'
#' @param ix integer
#'
set_ix_SugarSite <- function(ix){private$ix = ix}

SugarSite$set(which = "public",name = "set_ix",
  value = set_ix_SugarSite, overwrite = TRUE
)

#' Get siteXY
#'
#' Get site coordinates \code{siteXY}
#'  * This method is bound to \code{SugarSite$get_siteXY}
#'
get_siteXY_SugarSite <- function(){return(private$siteXY)}

SugarSite$set(which = "public",name = "get_siteXY",
  value = get_siteXY_SugarSite, overwrite = TRUE
)

#' Set siteXY
#'
#' Set site coordinates \code{siteXY}
#'  * This method is bound to \code{SugarSite$set_siteXY}
#'
#' @param ix numeric(2)
#'
set_siteXY_SugarSite <- function(newSiteXY){private$siteXY = newSiteXY}

SugarSite$set(which = "public",name = "set_siteXY",
  value = set_siteXY_SugarSite, overwrite = TRUE
)

#' Get searchWt
#'
#' Get site search weight \code{searchWt}
#'  * This method is bound to \code{SugarSite$get_searchWt}
#'
get_searchWt_SugarSite <- function(){return(private$searchWt)}

SugarSite$set(which = "public",name = "get_searchWt",
  value = get_searchWt_SugarSite, overwrite = TRUE
)

#' Set searchWt
#'
#' Set site search weight \code{searchWt}
#'  * This method is bound to \code{SugarSite$set_searchWt}
#'
#' @param ix numeric
#'
set_searchWt_SugarSite <- function(searchWt){private$searchWt = searchWt}

SugarSite$set(which = "public",name = "set_searchWt",
  value = set_searchWt_SugarSite, overwrite = TRUE
)


#' Get siteType
#'
#' Get site type \code{siteType}; 1 is peri-domestic, 0 is non peri-domestic.
#'  * This method is bound to \code{SugarSite$get_siteType}
#'
get_siteType_SugarSite <- function(){return(private$siteType)}

SugarSite$set(which = "public",name = "get_siteType",
  value = get_siteType_SugarSite, overwrite = TRUE
)

#' Set siteType
#'
#' Set site type \code{siteType}; 1 is peri-domestic, 0 is non peri-domestic.
#'  * This method is bound to \code{SugarSite$set_siteType}
#'
#' @param siteType integer
#'
set_siteType_SugarSite <- function(siteType){priate$siteType = siteType}

SugarSite$set(which = "public",name = "set_siteType",
  value = set_siteType_SugarSite, overwrite = TRUE
)


#' Get haz
#'
#' Get landing hazard associated with outside wall at this site \code{haz}
#'  * This method is bound to \code{SugarSite$get_haz}
#'
get_haz_SugarSite <- function(){return(private$haz)}

SugarSite$set(which = "public",name = "get_haz",
  value = get_haz_SugarSite, overwrite = TRUE
)

#' Set haz
#'
#' Set landing hazard associated with outside wall at this site \code{haz}
#'  * This method is bound to \code{SugarSite$set_haz}
#'
#' @param haz numeric
#'
set_haz_SugarSite <- function(haz){private$haz = haz}

SugarSite$set(which = "public",name = "set_haz",
  value = set_haz_SugarSite, overwrite = TRUE
)

#' Get \code{\link{Landscape}} Pointer
#'
#' Return \code{Landscape$self} enclosing this site.
#'  * This method is bound to \code{SugarSite$get_LandscapePointer}
#'
get_LandscapePointer_SugarSite <- function(){return(private$LandscapePointer)}

SugarSite$set(which = "public",name = "get_LandscapePointer",
  value = get_LandscapePointer_SugarSite, overwrite = TRUE
)

#' Set \code{\link{Landscape}} Pointer
#'
#' Set the pointer \code{Landscape$self} enclosing this site.
#'  * This method is bound to \code{SugarSite$set_LandscapePointer}
#'
#' @param LandscapePointer an environment
#'
set_LandscapePointer_SugarSite <- function(LandscapePointer){private$LandscapePointer = LandscapePointer}

SugarSite$set(which = "public",name = "set_LandscapePointer",
  value = set_LandscapePointer_SugarSite, overwrite = TRUE
)
