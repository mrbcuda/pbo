# unit tests for pbo 
require(testthat)
require(pbo)

context("Function usage")

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


test_that("missing function stops", {
  expect_error(pbo(m,s))
})

test_that("unknown function stops", {
  expect_error(pbo(m,s,unknown))
})

test_that("sharpe function succeeds",{
  x = pbo(m,s,sharpe)
  expect_true(x$phi > 0.49 && x$phi < 0.51)
  expect_true(x$slope > 0.19 && x$slope < 0.20)
  expect_true(x$ar2 > -0.24 && x$ar2 < -0.22)
  expect_true(x$below_threshold > 0.49 && x$below_threshold < 0.51)  
  expect_equal(x$inf_sub,6)
  expect_equal(x$threshold,0)
  expect_equal(x$func,"sharpe")
  expect_equal(nrow(x$rn_pairs),6)
  expect_equal(ncol(x$rn_pairs),2)
  expect_equal(length(x$lambda),6)
  expect_equal(ncol(x$combos),6)
  expect_equal(nrow(x$combos),2)
  expect_equal(nrow(x$results),6)
})

