% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-ages-over-90.R
\name{check_ages_over_90}
\alias{check_ages_over_90}
\title{Check for ages over 90}
\usage{
check_ages_over_90(
  data,
  col = "ageDeath",
  strict = FALSE,
  success_msg = "No ages over 90 detected",
  fail_msg = "Ages over 90 detected in the data"
)
}
\arguments{
\item{data}{Data to check}

\item{col}{Name of age column(s). Defaults to \code{ageDeath}.}

\item{strict}{If \code{FALSE}, return a \code{"check_warn"} object; if \code{TRUE}, return a
\code{"check_fail"} object}

\item{success_msg}{Message indicating the check succeeded.}

\item{fail_msg}{Message indicating the check failed.}
}
\value{
A condition object indicating whether the data contains ages over
ninety.
}
\description{
Checks metadata for ages over ninety that should be censored.
}
\examples{
dat <- data.frame(ageDeath = c(65, 80, 95))
check_ages_over_90(dat)

# Can check multiple columns
dat <- data.frame(age1 = c(50, 55), age2 = c(90, 95))
check_ages_over_90(dat)
}
