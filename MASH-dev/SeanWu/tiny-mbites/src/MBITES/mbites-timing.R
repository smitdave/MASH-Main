# main timing function
timing <- function(mosy){

  switch(mosy$state,
    F = {
      r <- get("parameters",.GlobalEnv)$time_BFSB
      mosy$tnext <- mosy$tnow + rexp(n=1,rate=r)
    },
    B = {
      r <- get("parameters",.GlobalEnv)$time_BFAB
      mosy$tnext <- mosy$tnow + rexp(n=1,rate=r)
    },
    L = {
      r <- get("parameters",.GlobalEnv)$time_ELSB
      mosy$tnext <- mosy$tnow + rexp(n=1,rate=r)
    },
    O = {
      r <- get("parameters",.GlobalEnv)$time_ELAB
      mosy$tnext <- mosy$tnow + rexp(n=1,rate=r)
    },
    # M = {self$activity_MAB()},
    # S = {self$activity_SFAB()},
    {stop("mosquito ",mosy$id," calling illegal behavioral state: ",mosy$state,"\n")}

  )

}

# PPR timing function
# the reason we update tnow here (normally very wrong) is becuase this is a small
# pseudo-state (such that we can imagine actually setting tnext, then as soon as
# we leave this function we instantly set tnow = tnext)
timing_ppr <- function(mosy){
  r <- get("parameters",.GlobalEnv)$time_ppr
  mosy$tnow <- mosy$tnow + rexp(n=1,rate=r)
}
