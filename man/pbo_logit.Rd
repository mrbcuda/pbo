\name{pbo_logit}
\alias{pbo_logit}
\title{Draws an annotated histogram of rank logits.}
\usage{
  pbo_logit(data, main = "Rank Logits",
    col_bar = "#cc99cc", col_line = "#3366cc",
    hist_type = "density", show_pbo = TRUE,
    show_regions = TRUE, show_config = TRUE, ...)
}
\arguments{
  \item{data}{a list as returned by pbo()}

  \item{main}{plot title, default 'Rank Logits'}

  \item{col_bar}{histogram bar fill color}

  \item{col_line}{density plot line color}

  \item{hist_type}{histogram type as used in 'histogram'
  function}

  \item{show_pbo}{whether to show the PBO value annotation,
  default TRUE}

  \item{show_regions}{whether to show the overfit region
  annotations, default TRUE}

  \item{show_config}{whether to show the study dimension
  annotations, default TRUE}

  \item{...}{other parameters passed to 'histogram' or
  'densityplot'}
}
\description{
  Draws an annotated histogram of rank logits.
}

