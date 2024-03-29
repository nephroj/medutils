% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fac.R
\name{fac}
\alias{fac}
\title{Make variables to factor}
\usage{
fac(data, vars, ...)
}
\arguments{
\item{data}{a dataframe to be used}

\item{vars}{Variables to be transformed to factor}

\item{...}{...}
}
\value{
Converted dataframe
}
\description{
This function transforms variables to the factor class
}
\examples{
fac(mtcars, am, cyl)

}
\keyword{fac}
