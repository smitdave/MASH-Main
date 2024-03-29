% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HUMAN-HumanPop-Parameters.R
\name{sitePops_HumanPop}
\alias{sitePops_HumanPop}
\title{Generate Site Sizes for \code{HumanPop}}
\usage{
sitePops_HumanPop(N, siteSize = 10, siteMin = 2, aa = 1.7, bb = 4)
}
\arguments{
\item{N}{number of sites (may refer to household or a \code{\link{Patch}} patch)}

\item{siteSize}{average size of population at sites}

\item{siteMin}{minimum size of population at sites}
}
\value{
list:
\itemize{
\item nHumans: total human population size
\item sitePops: vector of population size at each site
\item siteHumanID: list of human IDs at each site
\item homeHumanID: vector of home site ID for each human
\item siteAges: list of human ages at each site; calculated from \code{\link{siteAges_HumanPop}}
\item siteBiteWeight: list of human biting weights at each site; calculated from \code{\link{bitingWeight_HumanPop}}
}
}
\description{
Site may be defined as a MICRO \code{\link[MASHmicro]{FeedingSite}} or a MACRO \code{\link{Patch}}.
}
\examples{
sitePops(N = 10)
}
