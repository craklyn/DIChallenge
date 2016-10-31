# The problem of N=50, M=150 takes too long to compute on my laptop from 2010 =[
# Instead, let's approximate the answer through Monte Carlo simulation.

options(digits=10)

N = 50
M = 150

productList <- c()

totalRolls <- 0
timesRolledM <- 0
haveShownTable <- TRUE
while(TRUE) {
  totalRolls <- totalRolls + 1
  
  dice <- sample(1:6, size = N, replace = TRUE)
  if (sum(dice) == M) {
    timesRolledM <- timesRolledM + 1
    haveShownTable <- FALSE
    productList <- c(productList, prod(dice))
  }
  
  if(timesRolledM %% 10000 == 0 && !haveShownTable) {
    print(paste("totalRolls:", totalRolls))
    print(paste("timesRolledM:", timesRolledM))
    print("Mean:")
    print(mean(productList))
    print("StdDev:")
    print(sd(productList))
    haveShownTable <- TRUE
  }
}