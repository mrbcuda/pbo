Probability of Backtest Overfitting
===================================

Implements in R some of the ideas found in the Bailey et al. paper identified below.  In particular we use combinatorially symmetric cross validation (CSCV) to implement strategy performance tests evaluated by the Omega ratio. We compute the probability of backtest overfit, performance degradation, probability of loss, and stochastic dominance.  We plot visual representations of these using the `lattice` package.     

The reference authors used the Sharpe ratio as the performance measure.  Other measures are suitable according to the assumptions laid out in the paper.

Example plots attached below.  The first four illustrate a test with low overfitting (T-distribution, N=100, T=1600, S=8). The second four illustrate a test from the reference paper with high overfitting (normal distribution, N=100, T=1000, S=8).

Example test case, low overfitting:

![plot1](figures/plot1.png)
![plot2](figures/plot2.png)
![plot3](figures/plot3.png)
![plot4](figures/plot4.png)

Reference test case 1, high overfitting:

![plot1](figures/tc1_1.png)
![plot2](figures/tc1_2.png)
![plot3](figures/tc1_3.png)
![plot4](figures/tc1_4.png)

Installation
------------
```{r}
require(devtools)
install_github('pbo',username='mrbcuda')
```

Example
-------
```{r}
require(PerformanceAnalytics) # for Omega ratio

N <- 200                 # studies, alternative configurations
T <- 3200                # sample returns
S <- 8                   # partition count

# load the matrix with samples for N alternatives
M <- data.frame(matrix(NA,T,N,byrow=TRUE,dimnames=list(1:T,1:N)),check.names=FALSE)
for ( i in 1:N ) M[,i] <- rt(T,10) / 100

# compute and plot
my_pbo = pbo(M,S,F=Omega,threshold=1)
pbo_logit(my_pbo)
pbo_degradation(my_pbo)
pbo_dominance(my_pbo)
```

Packages
--------
* `utils` for the combinations
* `lattice` for plots
* `latticeExtra` over plot overlays only for the SD2 measure

Reference
---------
Bailey, David H. and Borwein, Jonathan M. and Lopez de Prado, Marcos and Zhu, Qiji Jim, The Probability of Back-Test Overfitting (September 1, 2013). Available at SSRN: http://ssrn.com/abstract=2326253 or http://dx.doi.org/10.2139/ssrn.2326253


