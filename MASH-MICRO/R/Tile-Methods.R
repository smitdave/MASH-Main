###############################################################################
#
#       __  ____               _______ __
#      /  |/  (_)_____________/_  __(_) /__
#     / /|_/ / / ___/ ___/ __ \/ / / / / _ \
#    / /  / / / /__/ /  / /_/ / / / / /  __/
#   /_/  /_/_/\___/_/   \____/_/ /_/_/\___/
#
#   MASH-MICRO
#   MICRO: Tile Class Methods
#   MASH-MICRO Team
#   September 6, 2017
#
###############################################################################


###############################################################################
# Getters & Setters
###############################################################################

#' Get tNow
#'
#' Return current simulation time. All agents run events until their next scheduled event overruns the current simulation time, upon which they
#' hold in the current behavioral state until the next time step. At the end of all activity on that iteration, \code{tNow} increases and the loop iterates.
#'  * This method is bound to \code{Tile$get_tNow}
#'
get_tNow_Tile <- function(){return(private$tNow)}

Tile$set(which = "public",name = "get_tNow",
  value = get_tNow_Tile, overwrite = TRUE
)

#' Set tNow
#'
#' Set current simulation time
#'  * This method is bound to \code{Tile$set_tNow}
#'
#' @param tNow numeric
#'
set_tNow_Tile <- function(tNow){private$tNow = tNow}

Tile$set(which = "public",name = "set_tNow",
  value = set_tNow_Tile, overwrite = TRUE
)

#' Get Humans
#'
#' Return embedded human population object \code{\link[MASHmacro]{HumanPop}}
#'  * This method is bound to \code{Tile$get_HumanPop}
#'
get_HumanPop_Tile <- function(){return(private$HumanPop)}

Tile$set(which = "public",name = "get_HumanPop",
  value = get_HumanPop_Tile, overwrite = TRUE
)

#' Get Landscape
#'
#' Return embedded landscape object \code{\link{Landscape}}
#'  * This method is bound to \code{Tile$get_Landscape}
#'
get_Landscape_Tile <- function(){return(private$Landscape)}

Tile$set(which = "public",name = "get_Landscape",
  value = get_Landscape_Tile, overwrite = TRUE
)

#' Get Female Mosquitoes
#'
#' Return embedded female mosquito population object \code{\link{MosquitoPopFemale}}
#'  * This method is bound to \code{Tile$get_FemalePop}
#'
get_FemalePop_Tile <- function(){return(private$FemalePop)}

Tile$set(which = "public",name = "get_FemalePop",
  value = get_FemalePop_Tile, overwrite = TRUE
)

#' Get Male Mosquitoes
#'
#' Return embedded male mosquito population object \code{\link{MosquitoPopMale}}
#'  * This method is bound to \code{Tile$get_MalePop}
#'
get_MalePop_Tile <- function(){return(private$MalePop)}

Tile$set(which = "public",name = "get_MalePop",
  value = get_MalePop_Tile, overwrite = TRUE
)


#' Get Directory
#'
#' Return the directory (character) containing output for this tile
#'  * This method is bound to \code{Tile$get_directory}
#'
get_directory_Tile <- function(){return(private$directory)}

Tile$set(which = "public",name = "get_directory",
  value = get_directory_Tile, overwrite = TRUE
)

#' Set Directory
#'
#' Set the directory (character) containing output for this tile
#'  * This method is bound to \code{Tile$set_directory}
#'
#' @param directory character
#'
set_directory_Tile <- function(directory){private$directory = directory}

Tile$set(which = "public",name = "set_directory",
  value = set_directory_Tile, overwrite = TRUE
)

#' Get Directory for Mosquito Output
#'
#' Return the directory (character) containing mosquito output for this tile
#'  * This method is bound to \code{Tile$get_MosquitoDirectory}
#'
get_MosquitoDirectory_Tile <- function(){return(private$MosquitoDirectory)}

Tile$set(which = "public",name = "get_MosquitoDirectory",
  value = get_MosquitoDirectory_Tile, overwrite = TRUE
)

#' Get Directory for Human Output
#'
#' Return the directory (character) containing human output for this tile
#'  * This method is bound to \code{Tile$get_HumanDirectory}
#'
get_HumanDirectory_Tile <- function(){return(private$HumanDirectory)}

Tile$set(which = "public",name = "get_HumanDirectory",
  value = get_HumanDirectory_Tile, overwrite = TRUE
)

#' Get Run ID
#'
#' Return current simulation run ID that will be used to identify multiple simulation output from the same tile object.
#'  * This method is bound to \code{Tile$get_runID}
#'
get_runID_Tile <- function(){return(private$runID)}

Tile$set(which = "public",name = "get_runID",
  value = get_runID_Tile, overwrite = TRUE
)

#' Set Run ID
#'
#' Set current simulation run ID that will be used to identify multiple simulation output from the same tile object.
#'  * This method is bound to \code{Tile$set_runID}
#'
#' @param runID integer
#'
set_runID_Tile <- function(runID){private$runID = runID}

Tile$set(which = "public",name = "set_runID",
  value = set_runID_Tile, overwrite = TRUE
)


###############################################################################
# Getters & Setters for Output Connections
###############################################################################

# FemaleCSVCon

#' Get Female CSV Connection
#'
#' Return the text connection used to write population count data to CSV.
#'  * This method is bound to \code{Tile$get_FemaleCSVCon}
#'
get_FemaleCSVCon_Tile <- function(){return(private$FemaleCSVCon)}

Tile$set(which = "public",name = "get_FemaleCSVCon",
  value = get_FemaleCSVCon_Tile, overwrite = TRUE
)

#' Set Female CSV Connection
#'
#' Set the text connection used to write population count data to CSV.
#'  * This method is bound to \code{Tile$set_FemaleCSVCon}
#'
#' @param FemaleCSVCon object of class \code{\link[base]{connection}}, usually opened with \code{file}
#'
set_FemaleCSVCon_Tile <- function(FemaleCSVCon){private$FemaleCSVCon = FemaleCSVCon}

Tile$set(which = "public",name = "set_FemaleCSVCon",
  value = set_FemaleCSVCon_Tile, overwrite = TRUE
)

#' Close Female CSV Connection
#'
#' Close the text connection used to write population count data to CSV.
#'  * This method is bound to \code{Tile$close_FemaleCSVCon}
#'
close_FemaleCSVCon_Tile <- function(){close(private$FemaleCSVCon)}

Tile$set(which = "public",name = "close_FemaleCSVCon",
  value = close_FemaleCSVCon_Tile, overwrite = TRUE
)

# MaleCSVCon

#' Get Male CSV Connection
#'
#' Return the text connection used to write population count data to CSV.
#'  * This method is bound to \code{Tile$get_MaleCSVCon}
#'
get_MaleCSVCon_Tile <- function(){return(private$MaleCSVCon)}

Tile$set(which = "public",name = "get_MaleCSVCon",
  value = get_MaleCSVCon_Tile, overwrite = TRUE
)

#' Set Male CSV Connection
#'
#' Set the text connection used to write population count data to CSV.
#'  * This method is bound to \code{Tile$set_MaleCSVCon}
#'
#' @param MaleCSVCon object of class \code{\link[base]{connection}}, usually opened with \code{file}
#'
set_MaleCSVCon_Tile <- function(MaleCSVCon){private$MaleCSVCon = MaleCSVCon}

Tile$set(which = "public",name = "set_MaleCSVCon",
  value = set_MaleCSVCon_Tile, overwrite = TRUE
)

#' Close Male CSV Connection
#'
#' Close the text connection used to write population count data to CSV.
#'  * This method is bound to \code{Tile$close_MaleCSVCon}
#'
close_MaleCSVCon_Tile <- function(){close(private$FemaleCSVCon)}

Tile$set(which = "public",name = "close_MaleCSVCon",
  value = close_MaleCSVCon_Tile, overwrite = TRUE
)

# FemaleHistoryCon

#' Get Female JSON Connection
#'
#' Return the text connection used to write female histories to JSON.
#'  * This method is bound to \code{Tile$get_FemaleHistoryCon}
#'
get_FemaleHistoryCon_Tile <- function(){return(private$FemaleHistoryCon)}

Tile$set(which = "public",name = "get_FemaleHistoryCon",
  value = get_FemaleHistoryCon_Tile, overwrite = TRUE
)

#' Set Female JSON Connection
#'
#' Set the text connection used to write female histories to JSON.
#'  * This method is bound to \code{Tile$set_FemaleHistoryCon}
#'
#' @param FemaleHistoryCon object of class \code{\link[base]{connection}}, usually opened with \code{file}
#'
set_FemaleHistoryCon_Tile <- function(FemaleHistoryCon){private$FemaleHistoryCon = FemaleHistoryCon}

Tile$set(which = "public",name = "set_FemaleHistoryCon",
  value = set_FemaleHistoryCon_Tile, overwrite = TRUE
)

#' Close Male JSON Connection
#'
#' Close the text connection used to write female histories to JSON.
#'  * This method is bound to \code{Tile$close_FemaleHistoryCon}
#'
close_FemaleHistoryCon_Tile <- function(){close(private$FemaleHistoryCon)}

Tile$set(which = "public",name = "close_FemaleHistoryCon",
  value = close_FemaleHistoryCon_Tile, overwrite = TRUE
)

# MaleHistoryCon

#' Get Male JSON Connection
#'
#' Return the text connection used to write male histories to JSON.
#'  * This method is bound to \code{Tile$get_MaleHistoryCon}
#'
get_MaleHistoryCon_Tile <- function(){return(private$MaleHistoryCon)}

Tile$set(which = "public",name = "get_MaleHistoryCon",
  value = get_MaleHistoryCon_Tile, overwrite = TRUE
)

#' Set Male JSON Connection
#'
#' Set the text connection used to write male histories to JSON.
#'  * This method is bound to \code{Tile$set_MaleHistoryCon}
#'
#' @param MaleHistoryCon object of class \code{\link[base]{connection}}, usually opened with \code{file}
#'
set_MaleHistoryCon_Tile <- function(MaleHistoryCon){private$MaleHistoryCon = MaleHistoryCon}

Tile$set(which = "public",name = "set_MaleHistoryCon",
  value = set_MaleHistoryCon_Tile, overwrite = TRUE
)

#' Close Male JSON Connection
#'
#' Close the text connection used to write male histories to JSON.
#'  * This method is bound to \code{Tile$close_MaleHistoryCon}
#'
close_MaleHistoryCon_Tile <- function(){close(private$MaleHistoryCon)}

Tile$set(which = "public",name = "close_MaleHistoryCon",
  value = close_MaleHistoryCon_Tile, overwrite = TRUE
)

# MosquitoPathogenCon

#' Get Mosquito Pathogen JSON Connection
#'
#' Return the text connection used to write mosquito-stage pathogen data to JSON
#'  * This method is bound to \code{Tile$get_MosquitoPathogenCon}
#'
get_MosquitoPathogenCon_Tile <- function(){return(private$MosquitoPathogenCon)}

Tile$set(which = "public",name = "get_MosquitoPathogenCon",
  value = get_MosquitoPathogenCon_Tile, overwrite = TRUE
)

#' Set Mosquito Pathogen JSON Connection
#'
#' Set the text connection used to write mosquito-stage pathogen data to JSON
#'  * This method is bound to \code{Tile$set_MosquitoPathogenCon}
#'
#' @param MosquitoPathogenCon object of class \code{\link[base]{connection}}, usually opened with \code{file}
#'
set_MosquitoPathogenCon_Tile <- function(MosquitoPathogenCon){private$MosquitoPathogenCon = MosquitoPathogenCon}

Tile$set(which = "public",name = "set_MosquitoPathogenCon",
  value = set_MosquitoPathogenCon_Tile, overwrite = TRUE
)

#' Close Mosquito Pathogen JSON Connection
#'
#' Close the text connection used to write mosquito-stage pathogen data to JSON
#'  * This method is bound to \code{Tile$close_MosquitoPathogenCon}
#'
close_MosquitoPathogenCon_Tile <- function(){close(private$MosquitoPathogenCon)}

Tile$set(which = "public",name = "close_MosquitoPathogenCon",
  value = close_MosquitoPathogenCon_Tile, overwrite = TRUE
)

# HumanPathogenCon

#' Get Human Pathogen JSON Connection
#'
#' Return the text connection used to write human-stage pathogen data to JSON
#'  * This method is bound to \code{Tile$get_HumanPathogenCon}
#'
get_HumanPathogenCon_Tile <- function(){return(private$HumanPathogenCon)}

Tile$set(which = "public",name = "get_HumanPathogenCon",
  value = get_HumanPathogenCon_Tile, overwrite = TRUE
)

#' Set Human Pathogen JSON Connection
#'
#' Set the text connection used to write human-stage pathogen data to JSON
#'  * This method is bound to \code{Tile$set_HumanPathogenCon}
#'
#' @param HumanPathogenCon object of class \code{\link[base]{connection}}, usually opened with \code{file}
#'
set_HumanPathogenCon_Tile <- function(HumanPathogenCon){private$HumanPathogenCon = HumanPathogenCon}

Tile$set(which = "public",name = "set_HumanPathogenCon",
  value = set_HumanPathogenCon_Tile, overwrite = TRUE
)

#' Close Human Pathogen JSON Connection
#'
#' Close the text connection used to write human-stage pathogen data to JSON
#'  * This method is bound to \code{Tile$close_HumanPathogenCon}
#'
close_HumanPathogenCon_Tile <- function(){close(private$HumanPathogenCon)}

Tile$set(which = "public",name = "close_HumanPathogenCon",
  value = close_HumanPathogenCon_Tile, overwrite = TRUE
)
