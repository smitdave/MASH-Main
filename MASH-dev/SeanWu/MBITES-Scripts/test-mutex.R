library(synchronicity)

# example: https://github.com/adamryczkowski/clustertools/blob/master/possible_bug_mutexes.R
spawn_fn<-function(tag, prev_mutex) {
  next_mutex <- synchronicity::boost.mutex(mut_name<-synchronicity::uuid())
  synchronicity::lock(next_mutex) #We start the thread locked

  env<-new.env()
  env$a<-tag
  env$m_next_descr <- synchronicity::describe(next_mutex)
  env$m_previous_descr <- synchronicity::describe(prev_mutex)

  j<-eval(quote(parallel::mcparallel({
    m_next<-synchronicity::attach.mutex(m_next_descr)
    m_previous<-synchronicity::attach.mutex(m_previous_descr)

    synchronicity::lock(m_previous)
    synchronicity::unlock(m_previous)

    Sys.sleep(1)
    b<-list(value=a, walltime=as.numeric(Sys.time()))
    synchronicity::unlock(m_next)
    b
  })), envir = env)
  return(list(job=j, value=tag, next_m=next_mutex))
}

test_fn<-function(item) {
  ans<-parallel::mccollect(jobs = item$job)[[1]]
  if(length(ans)==0) {
    browser()
  }
  if(item$value!=ans$value) {
    stop("Error")
  }
  return(ans)
}

prev_mutex<-synchronicity::boost.mutex(mut_name<-synchronicity::uuid())
for(i in seq(100)) {
  cat('\n')
  job_nr<-sample.int(length(jobs), 1)
  if(length(jobs[[job_nr]])!=0){
    j<-jobs[[job_nr]]
    cat(paste0("Testing job ", job_nr, "\nExpected value: ", j$value, "\nCurrent walltime: ", as.numeric(Sys.time()),"\n"))
    ans<-test_fn(j)
    jobs[[job_nr]]<-list()
    cat(paste0("Execution walltime: ",ans$walltime))

    cat('\n')
  }
  r<-runif(1)
  cat(paste0("Spawning job ", job_nr, "\nExpected value: ", r, "\nCurrent walltime: ", as.numeric(Sys.time()),"\n"))
  jobs[[job_nr]]<-spawn_fn(r, prev_mutex)
  prev_mutex<-jobs[[job_nr]]$next_m
}

a<-spawn_fn(1, prev_mutex)
test_fn(a)

jobs<-rep(x = list(list()),20)


# @ https://stackoverflow.com/questions/31575585/shared-memory-in-parallel-foreach-in-r/37964291#37964291
#
# usage: repeat this script several times

### inc
library(parallel)
library(bigmemory)

library(synchronicity) #for locks

### par
num_jobs <- 20

### shared object
shared <- big.matrix(nrow = 1, ncol = 1, type = 'double')
shared_sync <- big.matrix(nrow = 1, ncol = 1, type = 'double')

shared[1] <- 0
shared_sync[1]  <- 0

### semaphor
m <- boost.mutex()

### increment functions
inc_race <- function()
{
  shared[1] <- shared[1] + 1
}

inc_sync <- function()
{
  lock(m)
  shared_sync[1] <- shared_sync[1] + 1
  unlock(m)
}


### race
for(i in seq(1, num_jobs)) job <- mcparallel(inc_race())
out <- mccollect()

for(i in seq(1, num_jobs)) job <- mcparallel(inc_sync())
out <- mccollect()

cat(" - number of expected increments (number of jobs):", num_jobs, "\n")
cat(" - number of realized unsync. increments:", shared[1], "\n")
cat(" - number of realized sync. increments:", shared_sync[1], "\n")
