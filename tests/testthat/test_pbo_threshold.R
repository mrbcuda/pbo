# unit tests for pbo
require(testthat)
require(pbo)

context("Threshold argument")

n=4
t=20
s=2
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

test_that("threshold of -1 accepted", {
  expect_true(pbo(m,s,sum,threshold=-1)$threshold == -1)
})

test_that("threshold of -1 preserved", {
  expect_false(pbo(m,s,sum,threshold=-1)$threshold == -2)
})

test_that("threshold of 0 default", {
  p = pbo(m,s,sharpe)
  expect_true(p$threshold == 0)
  expect_equal(p$phi, 0.0)
  expect_equal(p$below_threshold, 0.5)
})

test_that("threshold passed as character", {
  expect_true(pbo(m,s,sum,threshold='z')$threshold == 'z')
  expect_equal(pbo(m,s,sum,threshold='z')$phi, 0)
})
