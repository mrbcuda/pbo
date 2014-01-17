\name{pbo_performance_ranks}
\alias{pbo_performance_ranks}
\title{Draws a scatter XY plot showing OOS performance rank by CSCV case, with out-of-sample rank
exceeding a given threshold.}
\usage{
  pbo_performance_ranks(data, osr_threshold = 0,
    xlab = "Selected IS Study (N)", ylab = "OOS Rank",
    show_rug = TRUE, show_config = TRUE, show_grid = FALSE,
    ...)
}
\arguments{
  \item{data}{a list as returned by pbo()}

  \item{osr_threshold}{out-of-sample rank inclusion
  threshold, default 0}

  \item{xlab}{an x-axis label string}

  \item{ylab}{a y-axis label string}

  \item{show_rug}{whether to show the rug plot along the
  axes, default TRUE}

  \item{show_config}{whether to show the study dimension
  annotations, default TRUE}

  \item{show_grid}{whether to show the background grid}

  \item{...}{other parameters as passed to xyplot()}
}
\description{
  Draws a scatter XY plot showing OOS performance rank by
  CSCV case, with out-of-sample rank exceeding a given
  threshold.
}

