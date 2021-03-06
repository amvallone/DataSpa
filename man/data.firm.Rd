% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.firm.R
\name{data.firm}
\alias{data.firm}
\title{Collects information of firms at a municipality level}
\usage{
data.firm(provincia)
}
\arguments{
\item{provincia}{one of the 52 Spanish provinces. See \link{getbase.pob} for details.}
}
\value{
A data frame containing the following variables for each company: location (province, municipality, address, geographic coordinates), company characteristics (name, birth, legal form, social object), main figures (number of employees, social capital, sales), economic activity codes and firm URL
}
\description{
Generate a data frame of firm information of a particular municipality
}
\details{
It is an interactive function, which requires the selection of a particular municipality.
}
\examples{
\dontrun{data.firm("Ceuta")}

}
\seealso{
Other firm functions: \code{\link{data.firm.a}},
  \code{\link{num.firm.a}}, \code{\link{num.firm}}
}
\concept{firm functions}
