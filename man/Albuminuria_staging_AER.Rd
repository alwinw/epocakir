% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ckd.R
\name{Albuminuria_staging_AER}
\alias{Albuminuria_staging_AER}
\alias{Albuminuria_staging_AER.data.frame}
\alias{Albuminuria_staging_AER.units}
\alias{Albuminuria_staging_AER.numeric}
\title{Albuminuria Staging based on AER}
\usage{
Albuminuria_staging_AER(...)

\method{Albuminuria_staging_AER}{data.frame}(.data, AER, ...)

\method{Albuminuria_staging_AER}{units}(AER, ...)

\method{Albuminuria_staging_AER}{numeric}(AER, ...)
}
\arguments{
\item{...}{Further optional arguments}

\item{.data}{(data.frame) A data.frame, optional}

\item{AER}{Albumin excretion rate
column name, or vector of units or numeric if \code{.data} is not provided}
}
\value{
Albuminuria category
}
\description{
Using KDIGO 2012 Clinical Practice Guideline for
the Evaluation and Management of Chronic Kidney Disease
Volume 3 | Issue 1 | January 2013
}
\details{
\itemize{
\item{A1: Normal to mildly increased}
\item{A2: Moderately increased}
\item{A3: Severely increased}
}

See \url{https://kdigo.org/guidelines/ckd-evaluation-and-management/} for more details
}
\examples{
df <- tibble::tibble(
  AER = units::set_units(c(-1, NA, 15, 100, 500), "mg/day")
)

Albuminuria_staging_AER(df, "AER")

df \%>\%
  dplyr::mutate(GFR_level = Albuminuria_staging_AER(AER))
}
