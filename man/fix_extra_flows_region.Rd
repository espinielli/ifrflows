% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixes.r
\name{fix_extra_flows_region}
\alias{fix_extra_flows_region}
\title{Fix region value for various countries with respect to extra flows.}
\usage{
fix_extra_flows_region(flows)
}
\arguments{
\item{flows}{a data frame with origin/destination flows}
}
\value{
a data frame with corrected region
}
\description{
Fix region value for various countries with respect to extra flows.
}
\examples{
\dontrun{
flows \%>\% fix_extra_flows_region()
}
}
