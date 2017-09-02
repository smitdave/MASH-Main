###############################################################################
#
#      _____ _           ______           __
#     / ___/(_)___ ___  / ____/________  / /
#     \__ \/ / __ `__ \/ __/ / ___/ __ \/ /
#    ___/ / / / / / / / /___/ /__/ /_/ / /
#   /____/_/_/ /_/ /_/_____/\___/\____/_/
#
#   MASH-MACRO
#   SimEcol: Deterministc Difference Equations for Mosy Populations
#   MASH Team
#   September 1, 2017
#
###############################################################################


# MosquitoRM <- R6::R6Class(classname="MosquitoRM",
#                      portable = TRUE,
#                      cloneable = FALSE,
#                      lock_class = FALSE,
#                      lock_objects = FALSE,
#
#                      #public members
#                      public = list(
#
#                        #################################################
#                        # Constructor
#                        #################################################
#
#                        initialize = function(patchID, M, Y = 0, Z = 0, par){
#
#                          # patchID
#                          private$patchID = patchID
#
#                          # set population state variables
#                          private$M = M
#                          private$Y = Y
#                          private$Z = Z
#                          private$ZZ = matrix(data = rep(0,times=par$maxEIP),ncol=1)
#
#                          private$M_out = 0
#                          private$Y_out = 0
#                          private$Y0_out = 0
#                          private$Z_out = 0
#
#                          # set par
#                          private$par = par
#
#                        }
#
#                      ),
#
#                      #private members
#                      private = list(
#
#                        # fields
#                        patchID = NULL,
#
#                        M = NULL,
#                        Y = NULL,
#                        Y0 = NULL,
#                        Z = NULL,
#                        ZZ = NULL,
#
#                        M_out = NULL,
#                        Y_out = NULL,
#                        Y0_out = NULL,
#                        Z_out = NULL,
#
#                        par = NULL,
#
#                        # pointers
#                        PatchPointer = NULL,
#                        HumanPopPointer = NULL
#
#                      )
#
# ) #end class definition
