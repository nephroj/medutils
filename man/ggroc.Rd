% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggroc.R
\name{ggroc}
\alias{ggroc}
\title{Draw ROC plot with cut-off values}
\usage{
ggroc(
  roc,
  showAUC = TRUE,
  interval = 0.2,
  breaks = seq(0, 1, interval),
  round = 2,
  show.num = 3,
  print.result = F,
  print.num = 5,
  font.size = 3.5
)
}
\arguments{
\item{roc}{roc object made by pROC package}

\item{showAUC}{show AUC value}

\item{interval}{interval}

\item{breaks}{breaks}

\item{round}{round value}

\item{show.num}{number of cut-off values}

\item{print.result}{print analysis results}

\item{print.num}{number of cut-off values to be printed}

\item{font.size}{font size}
}
\value{
ggplot object
}
\description{
Draw ROC plot with cut-off values
}
\keyword{ggroc}
