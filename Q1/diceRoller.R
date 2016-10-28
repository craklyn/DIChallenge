library(partitions)

getRollProbs <- function(N=8, M = 24) {
  
  # Generate list of rolls of N dice that can sum to M
  
  # First, make list of all partitions of eight numbers that sum to M 
  dicePartitions = restrictedparts(M, N, include.zero=FALSE)
  # Since we want partitions that have at most six - corresponding to dice
  # with six sides - remove partitions that have entries larger than six.
  goodRows = apply(dicePartitions, MARGIN = 2, function(x) max(x) <= 6)
  dicePartitions = dicePartitions[,goodRows]
  
  # For any sequence of n elements, there are (n!) ways to arrange it.
  # For a sequence that contains identical subset of elements, the number of unique
  # ways to arrange it is reduced to: (n! / (n1! * n2! * ... nf!))
  
  ncombs = factorial(N)
  ncombs
  
  for (i in 1:nrow(dicePartitions)) {
    frequencyDF = as.data.frame(table(dicePartitions[,i]))
    nCombs = ncombs / factorial(frequencyDF[i,"Freq"])
  }
  nCombs
  
  
  probSum = 0
  sumExpProd = 0
  
  probList = rep(0, ncol(dicePartitions))
  prodList = rep(0, ncol(dicePartitions))
  
  for(i in 1:ncol(dicePartitions)) {
    prob = getEventProb(nrolls = N,
                        ndicePerRoll = 1,
                        nsidesPerDie = 6,
                        eventList = as.list(dicePartitions[,i]),
                        orderMatters = FALSE)
    product = prod(dicePartitions[,i])
    print(prob*98813)
    
    probList[i] = prob
    prodList[i] = product
    
#    print(paste("prob:", prob, ", product:", product)) 
  }
  
  probSum = sum(probList)
  sumExpProd = sum(prodList*probList)
  
#  print(paste("probSum:",probSum))
#  print(paste("sumExpProd:", sumExpProd))
#  print(paste("sumExpProd / probSum:", sumExpProd / probSum))
  
  # Find expectation value.  Expectation value = sum(weight * value) / sum(weight).
  num = 0
  denom = 0
  for(i in 1:length(probList)) {
    num = num + probList[i] * prodList[i]
    denom = denom + probList[i]
  }
  
#  print(paste("Expectation value (num / denom):", num / denom))

  # Calculate the squared value of the standard deviation's numerator
  stdDevNum2 = 0
  for(i in 1:length(probList)) {
    stdDevNum2 = stdDevNum2 + probList[i] * (prodList[i] - sumExpProd)^2
  }
  
  # Calculate the squared value of the standard deviation's denominator
  # Calculation from https://en.wikipedia.org/wiki/Weighted_arithmetic_mean#Reliability_weights
  V1 = sum(probList)
  V2 = sum(probList^2)
  stdDevDenom2 = V1 - (V2/V1)
  
  stdDev2 = stdDevNum2 / stdDevDenom2
  stdDev = sqrt(stdDev2)
  #print(paste("StdDev:",stdDev))
  
  # Return expectation value and stdDev
  c(sumExpProd / probSum, stdDev)
}