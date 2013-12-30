\name{pbo_degradation}
\alias{pbo_degradation}
\title{Draws an annotated plot of performance degradation and probability of loss.}
\usage{
  pbo_degradation(data,
    main = "OOS Performance Degradation", xlab = "R (OOS)",
    ylab = "R (IS)", col_bar = "#cc99cc",
    col_line = "#3366cc", show_eqn = TRUE,
    show_threshold = TRUE, show_config = TRUE,
    show_rug = TRUE, ...)
}
\arguments{
  \item{data}{a list as returned by pbo()}

  \item{main}{plot title, default 'OOS Performance
  Degradation'}

  \item{xlab}{X-axis label}

  \item{ylab}{Y-axis label}

  \item{col_bar}{histogram bar fill color}

  \item{col_line}{density plot line color}

  \item{show_eqn}{whether to show the line equation
  annotation, default TRUE}

  \item{show_threshold}{whether to show the probability of
  loss annotation, default TRUE}

  \item{show_config}{whether to show the study dimension
  annotations, default TRUE}

  \item{show_rug}{whether to show the scatter rug near the
  axes}

  \item{...}{other parameters passed to 'xyplot' or
  'lmline'}
}
\description{
  Draws an annotated plot of performance degradation and
  probability of loss.
}

