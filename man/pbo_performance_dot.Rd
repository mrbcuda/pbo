\name{pbo_performance_dot}
\alias{pbo_performance_dot}
\title{Draws an annotated dot plot of study selection sorted by selection frequency.}
\usage{
  pbo_performance_dot(data,
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

  \item{...}{other parameters as passed to dotplot()}
}
\description{
  Draws an annotated dot plot of study selection sorted by
  selection frequency.
}

