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

# Package Development

to compile .Rd docs to PDF
```
pack <- "mypackage"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(path)))
```
