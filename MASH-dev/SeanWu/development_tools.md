# Parallel Computing

## R

* Futures: Parallel & Distributed computing
  * https://cran.r-project.org/web/packages/future/index.html
  * https://github.com/HenrikBengtsson/future

* promises (similar to above??) https://appsilondatascience.com/blog/rstats/2017/11/01/r-promises-hands-on.html

* asychronous HTTP requests: https://github.com/r-lib/async (dunno how useful)

### more asynchronous computing (why's it so popular in R? maybe for Shiny requests?)

* https://github.com/r-lib/later
* https://github.com/r-lib/callr

### R - bigmemory ecosystem (seems to only work with matrices, unfortunately)

 * Rdsm https://cran.r-project.org/web/packages/Rdsm/index.html
 * synchronicity (mutexes for big.matrix objects) https://github.com/kaneplusplus/synchronicity
  * https://github.com/adamryczkowski/clustertools/blob/87c1e0464bd9dc8475bf05ac137892c466848933/possible_bug_mutexes.R
  * https://stackoverflow.com/questions/31575585/shared-memory-in-parallel-foreach-in-r/37964291#37964291

# Package Development

to compile .Rd docs to PDF (and also Dirk's answer on SO: https://stackoverflow.com/questions/23502380/pdf-documentation-of-local-r-package)
```
pack <- "mypackage"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(path),"--force"))
```
