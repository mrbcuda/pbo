\docType{package}
\name{pbo-package}
\alias{pbo-package}
\title{Computes the probability of backtest overfitting}
\description{
  Implements algorithms for computing the probability of
  backtest overfitting, performance degradation and
  probability of loss, and first- and second-order
  stochastic dominance, based on the approach specified in
  Bailey et al., September 2013.
}
\details{
  A package to compute probability of backtest overfitting
  measures.
}
\examples{
\dontrun{
library(pbo)
library(PerformanceAnalytics) # for Omega ratio
N <- 100
T <- 1000
S <- 8
sr_base <- 0

# scaling and centering
mu_base <- sr_base/(260.0)
sigma_base <- 1.00/(260.0)**0.5
M <- data.frame(matrix(NA,T,N,byrow=TRUE,dimnames=list(1:T,1:N)),check.names=FALSE)
for ( i in 1:N ) {
  M[,i] <- rnorm(T,mean=0,sd=1)
  M[,i] = M[,i] * sigma_base / sd(M[,i]) # re-scale
  M[,i] = M[,i] + mu_base - mean(M[,i]) # re-center
}

my_pbo <- pbo(M,S,ep=Omega)
pbo_logit(my_pbo)
pbo_degradation(my_pbo)
pbo_dominance(my_pbo)
pbo_performance_dot(my_pbo,show_grid=TRUE)
}
}
\author{
  Matt Barry \email{mrb@softisms.com}
}
\references{
  See Bailey, David H. and Borwein, Jonathan M. and Lopez
  de Prado, Marcos and Zhu, Qiji Jim, The Probability of
  Back-Test Overfitting (September 1, 2013). Available at
  SSRN: \url{http://ssrn.com/abstract=2326253} or
  \url{http://dx.doi.org/10.2139/ssrn.2326253}.
}
\keyword{package}

