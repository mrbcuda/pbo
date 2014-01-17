\name{pbo_performance_pairs}
\alias{pbo_performance_pairs}
\title{Draws a scatter XY plot of study selection pairs with OOS performance
exceeding a given threshold.}
\usage{
  pbo_performance_pairs(data, osr_threshold = 0,
    xlab = "IS Selection (N)",
    ylab = "OOS Counterpart Selection (N)",
    show_rug = TRUE, show_config = TRUE, show_grid = FALSE,
    pch = c(0, 3), col = c(4, 6), ...)
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

  \item{pch}{array of plotting characters, default c(5,6)}

  \item{col}{array of plotting colors, default c(4,6)}

  \item{...}{other parameters as passed to xyplot()}
}
\description{
  Draws a scatter XY plot of study selection pairs with OOS
  performance exceeding a given threshold.
}

