% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ckd.R
\name{GFR_staging}
\alias{GFR_staging}
\alias{GFR_staging.data.frame}
\alias{GFR_staging.units}
\alias{GFR_staging.numeric}
\title{GFR Staging}
\usage{
GFR_staging(...)

\method{GFR_staging}{data.frame}(.data, GFR, ...)

\method{GFR_staging}{units}(GFR, ...)

\method{GFR_staging}{numeric}(GFR, ...)
}
\arguments{
\item{...}{Further optional arguments}

\item{.data}{(data.frame) A data.frame, optional}

\item{GFR}{Glomerular filtration rate
column name, or vector of units or numeric if \code{.data} is not provided}
}
\value{
GFR category
}
\description{
Using KDIGO 2012 Clinical Practice Guideline for
the Evaluation and Management of Chronic Kidney Disease
Volume 3 | Issue 1 | January 2013
}
\details{
\itemize{
\item{G1: Normal or high GFR, \eqn{\ge}90}
\item{G2: Mildly decreased, 60-89}
\item{G3a: Mildly to moderately decreased, 45-59}
\item{G3b: Moderately  to severely decreased, 30-44}
\item{G4: Severely decreased, 15-29}
\item{G5: Kidney failure, <15}
}

See \url{https://kdigo.org/guidelines/ckd-evaluation-and-management/} for more details
}
\examples{
df <- tibble::tibble(
  eGFR = units::set_units(c(-1, NA, 100, 70, 50, 35, 20, 10), "mL/min/1.73m2")
)

GFR_staging(df, "eGFR")

df \%>\%
  dplyr::mutate(GFR_level = GFR_staging(eGFR))
}
