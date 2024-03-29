% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logistic_adj.R
\name{logistic_adj}
\alias{logistic_adj}
\title{Adjusted logistic regression results with various variable combination}
\usage{
logistic_adj(
  data,
  outcome,
  predictor,
  vars_list,
  nsmall = 2,
  digits = 1,
  p.round = 3
)
}
\arguments{
\item{data}{a dataframe to be used}

\item{outcome}{binary outcome variable}

\item{predictor}{predictor variable}

\item{vars_list}{list of variable combination}

\item{nsmall}{nsmall}

\item{digits}{digits}

\item{p.round}{P-value round}
}
\value{
data.frame object
}
\description{
Adjusted logistic regression results with various variable combination
}
\keyword{logistic_adj}
