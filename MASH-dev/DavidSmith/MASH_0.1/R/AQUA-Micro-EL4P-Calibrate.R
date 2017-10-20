#################################################################
#
#       ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MICRO Aquatic Ecology: EL4P
#   EL4P fitting
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 7, 2017
#
#################################################################

#################################################################
# EL4P Parameters
#################################################################

#' Calculate Lambda for EL4P Aquatic Ecology Module
#'
#' Calculate parameter \code{lambda} (daily adult female emergence over entire landscape) required to sustain \emph{Plasmodium falciparum} transmission at given value of \emph{R0} based on
#' classical Ross-MacDonald assumptions. This is used in EL4P fitting routines and called in \code{\link{EL4P.Parameters}} such that mean emergence over the landscape will be fitted to this equilibrium value.
#'  * Detailed derivations found in \url{https://doi.org/10.1186/1475-2875-3-13}
#'
#' @param R0 desired intensity of pathogen transmission
#' @param EIP length of the entomological incubation period
#' @param lifespan average lifespan of mosquito
#' @param S stability index; number of bites on humans over the average mosquito lifespan
#' @param nH number of humans on landscape
#' @param b mosquito to human transmission efficiency
#' @param c human to mosquito transmission efficiency
#' @param r rate of recovery in humans (1/average length of infection)
#' @return equilibrium lambda
#' @md
#' @export
calcLambda_MicroEL4P <- function(R0, EIP, lifespan, S, nH, b = 0.55, c = 0.15, r = 1/38){

  P = exp(-EIP/lifespan)

  return(
    (R0*nH*r) / ((S^2)*b*c*P)
  )
}

#' Initialize EL4P Aquatic Ecoogy Module Parameters
#'
#' Generate named list of EL4P parameters, and calculates lambda (see \code{\link{calcLambda_MicroEL4P}} for details).
#'
#' @param nAqua number of aquatic habitats on landscape
#' @param nHumans number of humans on landscape
#' @param R0 desired pathogen transmission level at equilibrium
#' @param eqAqua vector of probability of oviposition at each aquatic habitat
#' @param EIP length of the entomological incubation period
#' @param lifespan average lifespan of mosquito
#' @param G mean total lifetime egg production of adult female
#' @param nu mean egg batch size for single oviposition
#' @param S stability index; number of bites on humans over the average mosquito lifespan
#' @param alpha_a density independent survival parameter \code{alpha} from pupae to adult \deqn{-log(alpha_a^{\frac{1-p}{5}})}
#' @param alpha_sd standard deviation of density independent survival parameter \code{alpha}
#' @param p expected fraction of cohort that advances to next life stage (1/p is expected time spent in each stage L1,L2,L3,L4,P)
#' @param K_a shape parameter for gamma distributed weights on K
#' @param K_b scale parameter for gamma distributed weights on K
#' @param b mosquito to human transmission efficiency
#' @param c human to mosquito transmission efficiency
#' @param r rate of recovery in humans (1/average length of infection)
#' @md
#' @export
EL4P.Parameters <- function(
    nAqua,
    nHumans,
    R0,
    eqAqua,
    EIP,
    lifespan,
    G,
    nu,
    S,
    alpha_a = 0.8,
    alpha_sd = 0.004,
    p = 0.9,
    K_a = 1,
    K_b = 1,
    b = 0.55,
    c = 0.15,
    r = 1/38
  ){

    out = list(
      nAqua = nAqua,
      nHumans = nHumans,
      R0 = R0,
      eqAqua = eqAqua,
      EIP = EIP,
      lifespan = lifespan,
      G = G,
      nu = nu,
      S = S,
      alpha_a = alpha_a,
      alpha_sd = alpha_sd,
      p = p,
      K_a = K_a,
      K_b = K_b
    )
    out$lambda = calcLambda_MicroEL4P(R0,EIP,lifespan,S,nHumans,b,c,r)
    out$M = out$lambda / (1/lifespan)

    return(out)
}


#################################################################
# Main EL4P Fitting Functions
#################################################################

#' Fit EL4P Aquatic Ecology Module to \code{\link{Landscape}}
#'
#' Fit the EL4P Aquatic Ecology module on the exact LANDSCAPE to match a desired level of daily emergence at equilibrium.
#' This means that the site-specific density dependent mortality parameter \code{psi} will be fit
#' to K for each aquatic habitat on the landscape, and then each site will be run to equilibrium. Compare with \code{\link{setupAquaPop_EL4PsamplePoints}} which will
#' fit \code{psi} based on a sampling grid of K.
#'
#' @md
#' @export
EL4P.Landscape.Fit <- function(){
  stop("write me please")
}

#' Fit EL4P Aquatic Ecology Module to Mesh of K
#'
#' Fit the EL4P Aquatic Ecology module on a sampling grid of K values to match a desired level of daily emergence at equilibrium.
#' This means that the site-specific density dependent mortality parameter \code{psi} will be fit
#' to K based on a sampling grid of values for K in log-space. If \code{plot = TRUE}, the linear regression of \code{psi} against logged values of K
#' should show exact linear dependence, indicating fitted \code{psi} will produce desired level of lambda at equilibrium. Compare with \code{\link{setupAquaPop_EL4Pexact}} which will
#' fit \code{psi} based on an exact LANDSCAPE. This will also return the coefficients of a linear regression of K on psi (see \code{\link{psi2K_cf}}) to give the functional relationship between K and psi.
#'
#' @param mesh_N number of sample points of K (density of mesh of K values used to fit psi)
#' @param EL4P_PAR parameters from \code{\link{EL4P.Parameters}}
#' @param var_tol target minimum variance in lambda for emergence for equilibrium to be assumed
#' @param plot produce diagnostic plots of fitting algorithm
#'
#' @return named list
#'  * equilibriumPops: equilibrium aquatic populations
#'  * psi: fitted values of psi
#'  * alpha: sampled values of alpha
#'  * K: sampled values of K
#'  * meshK: sampled values of K
#'  * psi2K_cf: slope coefficient between 1/psi and K
#' @md
#' @export
EL4P.Mesh.Fit <- function(mesh_N, EL4P_PAR, var_tol = 0.1, plot = FALSE){

  with(EL4P_PAR,{

    # sample initial values
    K_w = rgamma(n = mesh_N, shape = K_a, scale = K_b) # weights on K
    K = (lambda * K_w) / sum(K_w) # K for each pool (aquatic habitat)
    alpha_m = -log(alpha_a^((1-p)/5)) # mean of alpha mortality parameter
    alpha = abs(rnorm(n = mesh_N, mean = alpha_m, sd = alpha_sd)) # density-independent mortality
    psi_init = alpha/K # initial value of psi (density-dependent mortality)

    # plot relationship between K and psi prior to fitting
    if(plot){
      par(mfrow=c(1,2))
      invisible(psi2K_cf_MicroEL4P(K,psi_init,TRUE,label="Before Fitting EL4P"))
    }

    # generate aquatic populations
    AquaPops = vector(mode="list",length=mesh_N)
    for(i in 1:mesh_N){
      AquaPops[[i]] = MASH::EL4P(numGenotypes=1,psi_new=psi_init[i],alpha_new=alpha[i],p_new=p)
    }

    # fit psi on a mesh of values for K
    rangeK = range(K)
    meshK_out = meshK_MicroEL4P(K_l=rangeK[1],K_u=rangeK[2],AquaPops=AquaPops,EL4P_PAR=EL4P_PAR,var_tol=var_tol)

    # regression of 1/psi on K
    cf = psi2K_cf_MicroEL4P(K=meshK_out$meshK,psi=unlist(meshK_out$psi_hat),plot=plot,label="After Fitting EL4P")
    if(plot){
      # psi2K_plot_MicroEL4P(psi2K=lm(1/K2psi_MicroEL4P(meshK_out$meshK,cf)~K+0),K=meshK_out$meshK,psi=K2psi_MicroEL4P(meshK_out$meshK,cf),label="After Regression")
      par(mfrow=c(1,1))
    }

    # use fitted relationship from regression to generate psi values for landscape K values
    psi_fit = K2psi_MicroEL4P(K,cf)

    # run all EL4P pool aquatic populations to equilibrium with fitted psi
    for(i in 1:mesh_N){
      AquaPops[[i]]$set_pop(meshK_out$eq_pops[[i]])
    }
    eq_pops = parallel::mcmapply(FUN = run2eq_MicroEL4P,psi = psi_fit, EL4P = AquaPops, eqAqua = eqAqua,
      MoreArgs = list(M=M,G=G,lifespan=lifespan,var_tol=var_tol),SIMPLIFY=FALSE,USE.NAMES=FALSE,mc.cores=parallel::detectCores()-2L)

    # remove external pointers to C++ objects and manually garbage collect memory
    rm(AquaPops)
    invisible(gc())

    # return results
    return(
      list(equilibriumPops=eq_pops,psi=psi_fit,alpha=alpha,K=K,meshK=meshK_out$meshK,psi2K_cf=cf)
    )

  })

}


#################################################################
# EL4P Fitting Auxiliary Functions
#################################################################

#' Fit Psi on Mesh of K Values
#'
#' Generate a mesh of K values in log space and fit psi to mesh via numerical optization, then run input EL4P populations to equilibrium.
#' This sets values of psi so that lambda \eqn{\lambda=K} at given parameter values.
#'
#' @param K_l lower bound of K mesh
#' @param K_u upper bound of K mesh
#' @param AquaPops list of EL4P objects (see \code{\link{EL4P}})
#' @param EL4P_PAR parameters from \code{\link{EL4P.Parameters}}
#' @param psi_min lower bound for fitting psi via one dimensional optimization
#' @param psi_max upper bound for fitting psi via one dimensional optimization
#' @param tMax maximum time to to run aquatic populations to equilibrium
#' @param var_tol target minimum variance in lambda for aquatic populations equilibrium
#' @return named list
#' * eq_pops: EL4P pool populations at equilibrium values
#' * psi_hat: fitted values of psi
#' * meshK: sampled values of K
#' @md
#' @export
meshK_MicroEL4P <- function(K_l, K_u, AquaPops, EL4P_PAR, psi_min = 0, psi_max = 10, tMax = 500, var_tol = 0.1){

  with(EL4P_PAR,{

    # sample K on mesh in log space; transform to linear space
    meshK = exp(seq(log(K_l),log(K_u),length.out=length(AquaPops)))

    # fit EL4P; set values of psi so lambda = K at given parameter values
    print(paste0("fitting psi for all sample values of K"))
    psi_hat = parallel::mcmapply(FUN = function(EL4P, M, eqAqua, K, G, lifespan, psi_min, psi_max){
      psi_optim_out = stats::optimize(f = psiFit_MicroEL4P,interval = c(psi_min,psi_max),EL4P = EL4P,M = M,eqAqua = eqAqua,K = K,G = G,lifespan = lifespan)
      return(psi_optim_out$minimum)
    },EL4P = AquaPops, eqAqua = eqAqua, K = meshK,
    MoreArgs = list(M=M,G=G,lifespan=lifespan,psi_min=psi_min,psi_max=psi_max),SIMPLIFY = FALSE,USE.NAMES = FALSE,mc.cores = parallel::detectCores()-2L)

    # run all EL4P pools to equilibrium values
    print(paste0("run EL4P pools to equilibrium values"))
    eq_pops = parallel::mcmapply(FUN = run2eq_MicroEL4P,psi = psi_hat, EL4P = AquaPops, eqAqua = eqAqua,
      MoreArgs = list(M=M,G=G,lifespan=lifespan,tMax=tMax,var_tol=var_tol),SIMPLIFY=FALSE,USE.NAMES=FALSE,mc.cores=parallel::detectCores()-2L)

    return(list(
        eq_pops = eq_pops, # EL4P pool populations at equilibrium values
        psi_hat = psi_hat, # fitted values of psi
        meshK = meshK # sampling mesh of K
      ))
  })

}

#' Objective Function for Fitting Psi
#'
#' Given a input psi, \code{x}, run a single aquatic population and output objective function; squared error of lambda around the given value of K \eqn{\left ( \lambda-K \right )^{2}}.
#' This will typically be called by \code{\link{meshK_MicroEL4P}} or \code{optimize}; generally it is the objective function that will be passed to \code{optimize(...)}
#'
#' @param x value of psi from \code{optimize(...)}
#' @param EL4P an EL4P pool (see \code{\link{EL4P}})
#' @param M equilibrium mosquito density at this aquatic habitat
#' @param eqAqua vector of probability of oviposition at each aquatic habitat
#' @param K carrying capacity at this aquatic habitat
#' @param G mean total lifetime egg production of adult female
#' @param lifespan average lifespan of mosquito
#' @param tMax time to run EL4P pool before returning squared error of empirical lambda around value of K
#' @return value of objective function at \code{x}
#' @export
psiFit_MicroEL4P <- function(x, EL4P,  M, eqAqua, K, G, lifespan, tMax = 150){

  psi_iter = abs(x) # value of psi at this iteration of optimization
  EL4P$set_psi(psi_iter) # set psi in EL4P pool

  # initial burn-in of EL4P pool with psi.iter
  for(i in 1:30){
    EL4P$oneStep_GEL4P(M = M,eqAqua = eqAqua,G = G,lifespan = lifespan)
  }

  # run the EL4P pool to tMax
  for(i in 1:tMax){
    EL4P$oneStep_GEL4P(M = M,eqAqua = eqAqua,G = G,lifespan = lifespan)
    M = ((exp(-1/lifespan))*M) + EL4P$get_totalLambda() # simulate adult population dynamics from pool
  }

  return((EL4P$get_totalLambda() - K)^2)
}

#' Run a Single Aquatic Population to Equilibrium
#'
#' Run a single aquatic population to equilibrium (where variance in emergence, lambda is less than \col{tol}).
#' This function first runs the population through a burnin period (see \code{\link{burnin_GEL4P}}) then runs aquatic dynamics with simulated adult
#' dynamics from derived Ross-MacDonald parameters (see \code{\link{G2K_GEL4P}}). Then the dynamics are run while variance in lambda is above \code{tol}
#' (see \code{\link{checkDX_GEL4P}).
#'
#' @param psi value of density-dependent mortality psi parameter for this EL4P pool (output of \code{optimize} called on \code{\link{psiFit_MicroEL4P}})
#' @param EL4P
#' @param M
#' @param eqAqua
#' @param G
#' @param lifespan
#' @param tMax
#' @param tMax_checkDX
#' @param var_tol
#' @return return the EL4P pool populations at equilibrium values
#' @export
run2eq_MicroEL4P <- function(psi, EL4P, M, eqAqua, G, lifespan, tMax = 800, var_tol = 0.1){

  EL4P$set_psi(psi) # set new value of psi
  EL4P$burnIn_GEL4P(M,eqAqua,G,lifespan,tMax) # run aquatic stages burn-in
  EL4P$G2K_GEL4P(eqAqua,G,lifespan,tMax) # run with simulated adult dynamics
  pop_out = EL4P$get_allGenotypes() # store pop after running G2K_GEL4P
  tMax_checkDX = 100
  lambda_dx = EL4P$checkDX_GEL4P(eqAqua,G,lifespan,tMax_checkDX) # output lambda history

  # run until variance in lambda stabilizes
  while(var(lambda_dx) > var_tol){
    EL4P$set_pop(pop_out)
    EL4P$G2K_GEL4P(eqAqua,G,lifespan,tMax_checkDX)
    pop_out = EL4P$get_allGenotypes()
    lambda_dx = EL4P$checkDX_GEL4P(eqAqua,G,lifespan,tMax_checkDX)
    tMax_checkDX = tMax_checkDX + 100
  }

  return(pop_out)
}


#################################################################
# Psi vs. K
#################################################################

#' Regress K on Psi and Return Coefficients
#'
#' Run a linear regression of K on iverse of psi and extract coefficients; optionally plot the regression. Psi and K should follow a linear relationship, and
#' the coefficients of the linear regression will give the parameter of the 1-dimensional response surface that relates the two parameters.
#'
#' @param K vector of K values
#' @param psi vector of fitted psi values from \code{\link{psiFit_MicroEL4P}}
#' @param plot produce plot
#' @param ... additional named parameters passed to \code{\link{psi2K_plot_MicroEL4P}}
#' @return regression coefficients
#' @export
psi2K_cf_MicroEL4P <- function(K, psi, plot, ...){
  psi_inv = 1/psi
  psi2K = lm(psi_inv~K+0)
  cf = coef(psi2K)
  if(plot){
    psi2K_plot_MicroEL4P(psi2K,K,psi,...)
  }
  return(cf)
}

#' Convert K and Regression Coefficient into Psi
#'
#' Return Psi from K and slope coefficient of linear relationship.
#'
#' @param K vector of K values
#' @param cf regression coefficients from \code{\link{psi2K_cf_MicroEL4P}}
#' @return psi
#' @export
K2psi_MicroEL4P <- function(K, cf){
  1/(cf*K)
}

#' Plot Regression of K on Psi
#'
#' Write docs.
#'
#' @param psi2K regression of K on psi via \code{lm}; (see \code{\link{psi2K_cf_MicroEL4P}})
#' @param K vector of K values
#' @param psi vector of psi values
#' @param label title of plot
#' @export
psi2K_plot_MicroEL4P <- function(psi2K,K,psi,label = NULL){
  col = ggCol_utility(n=1,alpha=0.8)
  plot(K,1/psi,type="p",pch=16,cex=1.15,col=col, ylab = expression(paste(1/psi," (density-dependent mortality)")), xlab = "K (carrying capacities)",main=label)
  legend(x = "topleft",legend = expression(paste("Regression of ",1/psi," on K")),bty = "n")
  grid()
  abline(psi2K)
  points(K, coef(psi2K)*K, col = "red", pch = 3)
  points(K, 1/K2psi_MicroEL4P(K = K,cf = coef(psi2K)),col = "purple")
}






















# placeholder
