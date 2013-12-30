\name{pbo_dominance}
\alias{pbo_dominance}
\title{Draws an annotated plot of stochastic dominance.}
\usage{
  pbo_dominance(data, main = "Stochastic Dominance",
    ylab_left = "Frequency",
    ylab_right = "2nd Ord. Stochastic Dominance",
    col_sd1 = "#3366cc", col_sd2 = "#339999",
    increment = 0.01, show_config = TRUE, show_prob = TRUE,
    ...)
}
\arguments{
  \item{data}{a list as returned by pbo()}

  \item{main}{plot title, default 'Stochastic Dominance'}

  \item{ylab_left}{Y-axis label for 1st order}

  \item{ylab_right}{Y-axis lable for 2nd order}

  \item{col_sd1}{line color for 1st order}

  \item{col_sd2}{line color for 2nd order}

  \item{increment}{step increment value for the cumulative
  distribution estimation}

  \item{show_config}{whether to show the study dimension
  annotations, default TRUE}

  \item{show_prob}{whether to show the probability equation
  annotation, default TRUE}

  \item{...}{other parameters passed to 'xyplot'}
}
\description{
  Draws an annotated plot of stochastic dominance.
}

