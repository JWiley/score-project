% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/MetSSS.R
\name{MetSSS}
\alias{MetSSS}
\title{Score the MetSSS}
\usage{
MetSSS(input)
}
\arguments{
\item{input}{data passed on as \code{newdata} to \code{\link{predictCS}}}
}
\description{
Function requires systolic and diastolic blood pressure,
  triglycerides, waist circumference, HDL cholesterol,
  blood glucose, and sex.
}
\examples{
mydata <- data.frame(
  sbp = c(122, 102.5),
  dbp = c(76.5, 64),
  trigs = c(1.47, 1.27),
  hdl = c(2.22, 1.59),
  waist = c(71, 91),
  glucose = c(5.16, 5.82),
  sex = c("Female", "Male"))

MetSSS(mydata)
}

