\name{pbo_performance_cases}
\alias{pbo_performance_cases}
\title{Draws a scatter XY plot showing selected study by CSCV case, with out-of-sample rank
exceeding a given threshold.}
\usage{
  pbo_performance_cases(data, osr_threshold = 50,
    xlab = "CSCV Case", ylab = "Selected Study (N)",
    show_rug = TRUE, show_config = TRUE, pch = c(0, 3),
    col = c(4, 6), ...)
}
\arguments{
  \item{data}{a list as returned by pbo()}

  \item{osr_threshold}{out-of-sample rank inclusion
  threshold, default 50}

  \item{xlab}{an x-axis label string}

  \item{ylab}{a y-axis label string}

  \item{show_rug}{whether to show the rug plot along the
  axes, default TRUE}

  \item{show_config}{whether to show the study dimension
  annotations, default TRUE}

  \item{pch}{array of plotting characters, default c(0,3)}

  \item{col}{array of plotting colors, default c(4,6)}

  \item{...}{other parameters as passed to xyplot()}
}
\description{
  Draws a scatter XY plot showing selected study by CSCV
  case, with out-of-sample rank exceeding a given
  threshold.
}

