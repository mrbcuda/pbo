\name{pbo_performance_xy}
\alias{pbo_performance_xy}
\title{Draws an annotated XY plot of study selection sorted by selection frequency.}
\usage{
  pbo_performance_xy(data,
    main = "IS Study Selection (Non-Zero Cases)",
    xlab = "Sorted Study Number (N)",
    ylab = "IS Selection Frequency", show_config = TRUE,
    show_grid = FALSE, ...)
}
\arguments{
  \item{data}{a list as returned by pbo()}

  \item{main}{a title string}

  \item{xlab}{an x-axis label string}

  \item{ylab}{a y-axis label string}

  \item{show_config}{whether to show the study dimension
  annotations, default TRUE}

  \item{show_grid}{whether to show the background grid}

  \item{...}{other parameters as passed to xyplot()}
}
\description{
  Draws an annotated XY plot of study selection sorted by
  selection frequency.
}

