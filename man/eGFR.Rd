% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ckd.R
\name{eGFR}
\alias{eGFR}
\alias{eGFR.data.frame}
\alias{eGFR.units}
\alias{eGFR.numeric}
\title{GFR Estimation}
\usage{
eGFR(...)

\method{eGFR}{data.frame}(
  .data,
  SCr = NULL,
  SCysC = NULL,
  Age = NULL,
  height = NULL,
  BUN = NULL,
  male = NULL,
  black = NULL,
  pediatric = NULL,
  ...
)

\method{eGFR}{units}(
  SCr = NULL,
  SCysC = NULL,
  Age = NULL,
  height = NULL,
  BUN = NULL,
  male = NULL,
  black = NULL,
  pediatric = NULL,
  ...
)

\method{eGFR}{numeric}(
  SCr = NULL,
  SCysC = NULL,
  Age = NULL,
  height = NULL,
  BUN = NULL,
  male = NULL,
  black = NULL,
  pediatric = NULL,
  ...
)
}
\arguments{
\item{...}{Further optional arguments}

\item{.data}{(data.frame) A data.frame, optional}

\item{SCr}{Serum creatinine
column name, or vector of units or numeric if \code{.data} is not provided}

\item{SCysC}{Serum Cystatin C
column name, or vector of units or numeric if \code{.data} is not provided}

\item{Age}{Age of patient
column name, or vector of units or numeric if \code{.data} is not provided}

\item{height}{Height of patient
column name, or vector of units or numeric if \code{.data} is not provided}

\item{BUN}{Blood urea nitrogen
column name, or vector of units or numeric if \code{.data} is not provided}

\item{male}{Male or not
column name, or vector of logical (TRUE/FALSE) if \code{.data} is not provided}

\item{black}{Black race or not
column name, or vector of logical (TRUE/FALSE) if \code{.data} is not provided}

\item{pediatric}{(logical) Pediatric or not
column name, or vector of logical (TRUE/FALSE) if \code{.data} is not provided}
}
\value{
(units) Estimated glomerular filtration rate (eGFR)
of the same type provided (numeric or units in ml/min/1.73m2)
}
\description{
Using KDIGO 2012 Clinical Practice Guideline for
the Evaluation and Management of Chronic Kidney Disease
Volume 3 | Issue 1 | January 2013
}
\details{
Automatic selection of equation to estimation the
Glomerular Filtration Rate (eGFR), based on input data

\itemize{
\item{\code{\link[=eGFR_adult_SCr]{eGFR_adult_SCr()}}: 2009 CKD-EPI creatinine equation}
\item{\code{\link[=eGFR_adult_SCysC]{eGFR_adult_SCysC()}}: 2012 CKD-EPI cystatin C equation}
\item{\code{\link[=eGFR_adult_SCr_SCysC]{eGFR_adult_SCr_SCysC()}}: 2012 CKD-EPI creatinine-cystatin C equation}
\item{\code{\link[=eGFR_child_SCr]{eGFR_child_SCr()}}: Pediatric creatinine-based equation}
\item{\code{\link[=eGFR_child_SCr_BUN]{eGFR_child_SCr_BUN()}}: Pediatric creatinine-BUN equation}
\item{\code{\link[=eGFR_child_SCysC]{eGFR_child_SCysC()}}: Pediatric cystatin C-based equation}
}

See \url{https://kdigo.org/guidelines/ckd-evaluation-and-management/} for more details
}
\examples{
eGFR(eGFR_pt_data,
  SCr = "SCr_", SCysC = "SCysC_",
  Age = "Age_", height = "height_", BUN = "BUN_",
  male = "male_", black = "black_", pediatric = "pediatric_"
)

eGFR_pt_data \%>\%
  dplyr::mutate(eGFR = eGFR(
    SCr = SCr_, SCysC = SCysC_,
    Age = Age_, height = height_, BUN = BUN_,
    male = male_, black = black_, pediatric = pediatric_
  ))
}
