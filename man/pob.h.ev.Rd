% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pob.h.ev.R
\name{pob.h.ev}
\alias{pob.h.ev}
\title{Panel of the male population at the municipality level for a period of time}
\usage{
pob.h.ev(inicio, fin, provincia, print = FALSE)
}
\arguments{
\item{inicio}{starting year of the panel, which must be higher than 1996.}

\item{fin}{last year of the panel.}

\item{provincia}{one of the 52 Spanish provinces.}

\item{print}{logical variable ‘do you need to print output file with the results? being FALSE the default value.}
}
\value{
It is a data frame
}
\description{
pob.h.ev. creates a data frame containing the panel of the male population of Spain at the municipality level for a period of time from the years \code{inicio} to \code{fin}.
}
\details{
If \code{print} is set to \code{TRUE}, an \code{xlsx} file containing the data frame is saved in the folder \code{Outputs} called \code{pob_male_ev_provincia_inicio-fin.xlsx}
}
\examples{
pob.h.ev(2005,2007,"Avila")

}
\seealso{
Other Manipulate functions: \code{\link{pob.e.ev}},
  \code{\link{pob.ev}}, \code{\link{pob.fen.ev}},
  \code{\link{pob.ind.p}}, \code{\link{pob.ind}},
  \code{\link{pob.m.ev}}, \code{\link{pob.n.ev}}
}
\concept{Manipulate functions}
