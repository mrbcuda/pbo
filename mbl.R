# minimum backtest length

# digamma(1) Euler-Mascheroni
# exp(1) Euler

emax <- function(N) {
  ( ( 1 + digamma(1) ) * qnorm( 1 - 1/N ) ) +  
    (-digamma(1) * qnorm( 1 - (1/N) * exp(-1)))
}

#implement upper bound
upbound <- function(N) {sqrt(2*log(N))}
#make a ugly plot for reasonableness check
curve(upbound, from = 1, to = 1000, col = "red", lty = 2)
curve(emax, from = 1, to = 1000, add=TRUE)
grid()


require(rCharts)
df <- data.frame(
  list(x=c(1,2:1000), y=c(0,emax(2:1000))))
d1 <- dPlot( y ~ x, groups = "x", data = df, type = "line", height = 270, width = 800)
d1$xAxis(type = "addMeasureAxis",orderBy = "x",outputFormat = ",0.0f")
d1$yAxis( outputFormat = ".2f")
d1

### Try next example for Eq. 6
# if y = 5
# so solve for annualized Sharpe of 1
# says no more than 45 N should be tried

# first just do this to make sure I understand
N = 45
y = 5
emax( N ) * y^-0.5  #seems like on the right path


#use emax from earlier for numerator
minBTL <- function( N, eMaxSharpe = 1 ) {
  (emax(N) / eMaxSharpe) ^ 2
}
#then this should equal 5 if correct
minBTL( N = 45, eMaxSharpe = 1 )

curve( minBTL, from = 1, to = 1000)

df <- data.frame(
  list(x=c(1,2:1000), minBTL=c(0,minBTL(2:1000))))
n1 <- nPlot( minBTL ~ x, data = df, type = "lineChart", height = 270, width = 800)
n1$yAxis( tickFormat = "#!d3.format(',.2f')!#")
n1$chart( useInteractiveGuideline = TRUE )
n1


