require(testthat)
require(pbo)

context("Parallel iteration")

n=40
t=200
s=4
set.seed(852)
m = data.frame(matrix(rnorm(n*t,mean=0,sd=1),
                      nrow=t,ncol=n,byrow=TRUE,
                      dimnames=list(1:t,1:n)))

sharpe <- function(x,rf=0.03/252) {
  sr <- apply(x,2,function(col) {
    er = col - rf
    return(mean(er)/sd(er))
  })
  return(sr)
}

test_that("parallel workers succeed", {
  testthat::skip_on_cran()
  
  nc <- parallel::detectCores()
  if ( !is.na(nc)) {
    cluster <- parallel::makeForkCluster()
    doParallel::registerDoParallel(cluster)
    p_pbo <- pbo(m,s,f=sharpe,threshold=1,allow_parallel=TRUE)
    parallel::stopCluster(cluster)
    expect_true(p_pbo$phi > 0.47 && p_pbo$phi < 0.53)
  }
})

test_that("serial workers succeed", {
  p_pbo <- pbo(m,s,f=sharpe,allow_parallel=FALSE)
  expect_true(p_pbo$phi > 0.47 && p_pbo$phi < 0.53)
})
