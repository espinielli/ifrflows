% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.r
\name{retrieve_country_flows}
\alias{retrieve_country_flows}
\title{retrieve the yearly number of flights from countries for a period of years.}
\usage{
retrieve_country_flows(wef, til)
}
\arguments{
\item{wef}{the beginning year of the period (included).}

\item{til}{the end year of the period (not included).}
}
\value{
a dataframe with year, # of flights, departing country code and
arrival country code.
}
\description{
retrieve the yearly number of flights from countries for a period of years.
}
\examples{
\dontrun{
retrieve_country_flows('2001', '2002')
# A tibble: 4,743 x 4
YEAR  NB_OF_FLIGHT DEP_ISO_COUNTRY_CODE DES_ISO_COUNTRY_CODE
<chr>        <dbl> <chr>                <chr>
  1 2001          269. CN                   IT
2 2001         1499. BR                   FR
3 2001         1211. JP                   NL
4 2001          484. JO                   TR
5 2001        44710. ES                   FR
6 2001         5684. AE                   GB
7 2001          940. IL                   BE
8 2001         5863. EG                   DE
9 2001         3350. BE                   PT
10 2001         1750. RU                   AT
# ... with 4,733 more rows
}

}
