% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PATHOGEN-PfSI-Methods.R
\name{add2Q_pewanePfSI}
\alias{add2Q_pewanePfSI}
\title{PfSI \code{Human} Event: Add PfSI PE Waning Protection Event to Event Queue}
\usage{
add2Q_pewanePfSI(tEvent, PAR = NULL)
}
\arguments{
\item{tEvent}{time of vaccination}

\item{PAR}{\code{NULL}}
}
\description{
Add PfSI PE waning protection event to the event queue.
This method is called from \code{\link{pevaccinatePfSI}}
This method adds event \code{\link{event_pewanePfSI}} to the event queue.
\itemize{
\item This method is bound to \code{Human$add2Q_pewanePfSI()}
}
}
