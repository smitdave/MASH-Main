###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     null human object
#     Sean Wu
#     June 2019
#
###############################################################################

# the null human is an object that just gets bitten and never moves.
make_null_human <- function(id,w,siteID){

  human <- list()

  # basic parameters
  human$id <- id
  human$w <- w

  # location fields
  human$siteID <- siteID

  # biting history
  human$UNBITTEN <- TRUE, # have i been bitten yet?
  human$mosquito_id <- integer(1) # vector of mosquitoes that have bitten me
  human$mosquito_t <- numeric(1) # vector of times i was bitten
  human$bloodFeed <- logical(1) # F for probing-only events, T for blood feeding events

  list2env(human,hash=TRUE)
}

# write out the human
exit_human_null <- function(human){
  cat(jsonlite::toJSON(x = list(
          id = human$id,
          site = human$siteID,
          bite_id = human$mosquito_id,
          bite_times = human$mosquito_t,
          bloodfeed_bool = human$bloodFeed
      ), pretty = TRUE),",\n",sep="",file=get("globals")$human_out)
}


###############################################################################
# Interface with Tile-Simulation
###############################################################################

# the 'activity space' just keeps me home.
activity_space_human_null <- function(human){
  get("landscape")[[human$siteID]]$riskQ$add2Q(human$id,human$w,1)
}

# the eventQ doesn't do anything
eventQ_human_null <- function(human){}


###############################################################################
# Interface with Pathogen-NULL
###############################################################################

# the probe
pushProbe_human_null <- function(human,m_id,t){
  if(human$UNBITTEN){
    human$mosquito_id = m_id
    human$mosquito_t = t
    human$bloodFeed = FALSE
    human$UNBITTEN = FALSE
  } else {
    human$mosquito_id = append(human$mosquito_id,m_id)
    human$mosquito_t = append(human$mosquito_t,t)
    human$bloodFeed = append(human$bloodFeed,FALSE)
  }
}

# the blood meal
pushFeed_human_null <- function(human){
  human$bloodFeed[length(human$bloodFeed)] = TRUE
}
