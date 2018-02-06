library(MASH)

# test ImagoQ

# sizeQ = 25L
# N = sample(x = 50L,size = sizeQ, replace = TRUE)
# tEmerge = sort(runif(n = sizeQ, min = 0, max = 8))
# genotype = rep(x = -1L, times = sizeQ)
# damID = rep(x = -1L, times = sizeQ)
# sireID = rep(x = -1L, times = sizeQ)
#
# ImagoQ = MASH::ImagoQ()
#
# for(i in 1:sizeQ){
#   ImagoQ$add_ImagoQ(N[i],tEmerge[i],genotype[i],damID[i],sireID[i])
# }
#
# ImagoQ$get_ImagoQ()
# ImagoQ$get_ImagoQTime(tNow = 1,clear = FALSE)
# ImagoQ$get_N()
#
# ImagoQ$get_ImagoQTime(tNow = 1,clear = TRUE)
# ImagoQ$get_N()
# ImagoQ$get_ImagoQ()
#
# ImagoQ$track_ImagoQ(time = 1)
# ImagoQ$get_N()
#
# ImagoQ$add_ImagoQ(N_new = 5,tEmerge_new = 0.1,genotype_new = -1L,damID_new = -1L,sireID_new = -1L)

# test LANDSCAPE
MICRO.Emerge.Setup()

Landscape_PAR = Landscape.Parameters(nFeed = 10,nAqua = 10,pointGen = "poisson",
                                     module = "emerge",modulePars = list(N = 10, lambda = 10))

AquaEmergeLambdaPlot_utility(Landscape_PAR$AquaticSite_PAR$lambda)

testLandscape = Landscape$new(Landscape_PAR = Landscape_PAR)

MicroLandscapePlot_utility(testLandscape)
