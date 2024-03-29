% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_survplot.R
\name{custom_survplot}
\alias{custom_survplot}
\title{Plot customized survival graphs}
\usage{
custom_survplot(
  survFit,
  filename = NULL,
  title = NULL,
  xlab = "Overall survival (years)",
  ylab = "Survival probability",
  xmax = NULL,
  timescale = "years",
  trend = FALSE,
  custom_palette = NULL,
  custom_legends = NULL,
  risk.table = FALSE,
  mar = c(12, 9, 3, 2),
  width = 2,
  height = 2.2
)
}
\arguments{
\item{survFit}{survfit object}

\item{filename}{character or \code{NULL}; filename for export or no export}

\item{title}{character; plot title}

\item{xlab}{character; x-axis label}

\item{ylab}{character; y-axis label}

\item{xmax}{numeric; x-axis limit}

\item{timescale}{character; adjust x axis scale to "years" or "days"}

\item{trend}{logical; toggle test for trend in logrank test}

\item{custom_palette}{character; color palette for groups}

\item{custom_legends}{character; overwrite legend labels (see warning below)}

\item{risk.table}{logical; toggle generation of number at risk table below survival plot}

\item{mar}{numeric vector; plotting margins}

\item{width}{numeric; width of pdf export in cm}

\item{height}{numeric; height of pdf export in cm}
}
\description{
Creates a customized survival plot and optionally exports it as a pdf.
The risk.table argument of this function is based on the 'plotSurvival2' function developed
by Yuanyuan Xiao and Dorothee Nickles as part of the 'Imvigor210CoreBiologies'
package (http://research-pub.gene.com/IMvigor210CoreBiologies/).
}
\author{
Benjamin N. Ostendorf
}
