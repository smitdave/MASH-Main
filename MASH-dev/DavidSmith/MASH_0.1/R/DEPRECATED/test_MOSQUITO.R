library(MASH)

testMosy <- new.env()

testMosy$makeData <- function(){
  testMosy$state = sample(x = c("F","B"),size = 1)
  testMosy$tNow = runif(n = 1,min = 0,max = 10)
  testMosy$ix = sample(x = 10,size = 1)
  testMosy$inPointSet = sample(x = c("f","l"),size = 1)
}

testMosy$makeFeedData <- function(){
  testMosy$hostID = sample(x = c(1,-1),size = 1,prob = c(0.5,0.5))
  testMosy$bmSize = rlnorm(n = 1)
  testMosy$batch = rpois(n = 1,lambda = 5)
}
mosyHistory <- MASH::MosquitoFemaleHistory()

testMosy$makeData()
mosyHistory$historyTrack(privateEnv = testMosy,alive = TRUE)

testMosy$makeFeedData()
mosyHistory$historyFeed(privateEnv = testMosy)

testMosy$makeData()
mosyHistory$historyTrack(privateEnv = testMosy,alive = TRUE)

testMosy$makeData()
mosyHistory$historyTrack(privateEnv = testMosy,alive = TRUE)

testMosy$makeFeedData()
mosyHistory$historyFeed(privateEnv = testMosy)

testMosy$makeFeedData()
mosyHistory$historyFeed(privateEnv = testMosy)

testMosy$makeFeedData()
mosyHistory$historyFeed(privateEnv = testMosy)

mosyHistory$exportHistory()

testMosy$stateNew = "D"
testMosy$tNext = 1000

mosyHistory$historyTrack(privateEnv = testMosy,alive = FALSE)
mosyHistory$exportHistory()
# mosyHistory$calcBionomics()
mosyHistory$exportBionomics()


# test hash table efficiency
library(hash)
library(microbenchmark)

makeBigHash <- function(N){
  xx = hash::hash()
  for(i in 1:N){
    hash::.set(hash = xx, keys = i, values = i)
  }
  return(xx)
}

makeBigList <- function(N){
  xx = vector(mode="list",length = N)
  for(i in 1:N){
    xx[[i]] = i
  }
  return(xx)
}

microbenchmark::microbenchmark(
  makeBigHash(N = 1e4),
  makeBigList(N = 1e4)
)
