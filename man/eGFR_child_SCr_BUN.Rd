% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ckd.R
\name{eGFR_child_SCr_BUN}
\alias{eGFR_child_SCr_BUN}
\alias{eGFR_child_SCr_BUN.data.frame}
\alias{eGFR_child_SCr_BUN.units}
\alias{eGFR_child_SCr_BUN.numeric}
\title{eGFR Pediatric SCr, Height and BUN}
\usage{
eGFR_child_SCr_BUN(...)

\method{eGFR_child_SCr_BUN}{data.frame}(.data, SCr, height, BUN, ...)

\method{eGFR_child_SCr_BUN}{units}(SCr, height, BUN, ...)

\method{eGFR_child_SCr_BUN}{numeric}(SCr, height, BUN, ...)
}
\arguments{
\item{...}{Further optional arguments}

\item{.data}{(data.frame) A data.frame, optional}

\item{SCr}{Serum creatinine
column name, or vector of units or numeric if \code{.data} is not provided}

\item{height}{Height of patient
column name, or vector of units or numeric if \code{.data} is not provided}

\item{BUN}{Blood urea nitrogen
column name, or vector of units or numeric if \code{.data} is not provided}
}
\value{
Estimated GFR
of the same type provided (numeric or units)
}
\description{
Using KDIGO 2012 Clinical Practice Guideline for
the Evaluation and Management of Chronic Kidney Disease
Volume 3 | Issue 1 | January 2013
}
\details{
See \url{https://kdigo.org/guidelines/ckd-evaluation-and-management/} for more details
}
\examples{
eGFR_child_SCr_BUN(eGFR_pt_data,
  SCr = "SCr_", height = "height_", BUN = "BUN_",
)

eGFR_pt_data \%>\%
  dplyr::mutate(eGFR = eGFR_child_SCr_BUN(
    SCr = SCr_, height = height_, BUN = BUN_,
  ))
}
