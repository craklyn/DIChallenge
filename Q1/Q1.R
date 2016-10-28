setwd("~/Dropbox/Applications/Data Incubator/Challenge Problems/Q1")
options(digits=10)

source("diceRoller.R")

library(dice)
getSumProbs(ndicePerRoll=8, nsidesPerDie=6)$probabilities[17,]
# Result:
#       Sum  Probability Ways to Roll
# ..
# [17,]  24 5.883071e-02        98813

getRollProbs(8, 24)
# Result: (exp value) (std dev)
# [1] 1859.932954 1983.584442

getRollProbs(50, 150)
