# Try to generate sum and product recursively

N = 8
M = 24


# format: list(sum) -> list(product) -> value (multiplicity)

initialMatrix <- list(list(c(1,1)), list(c(2,1)), list(c(3,1)), 
                      list(c(4,1)), list(c(5,1)), list(c(6,1)))

prevMatrix <- initialMatrix

for(counter in 2:N) {
  print(paste("----------------->  counter:",counter))
  
  # New list sum max is 6 greater htan previous, but no need to consider sums greater than M
  newSumMax <- min(length(prevMatrix)+6, M)
  
  newMatrix <- list()
  for(i in 1:newSumMax) {
    newMatrix[[i]] <- list()
  }
  
  # Okay, empty list is created.  Now we need to find the option multiplicity for each roll
  # based on the previous matrix.
  
  for(sum in 1:length(prevMatrix)) {
    curSumList <- prevMatrix[[sum]]
    for(curSum in 1:length(curSumList)) {
      if(length(curSumList) == 0) {
        break
      }
      
#      print(paste("length(curSumList)", length(curSumList)))
#      print(paste("curSum:",curSum))
      curProd <- curSumList[[curSum]][1]
      curMulti <- curSumList[[curSum]][2]
      
#      print(paste("curSum:",curSum,", curProd:",curProd,", multiplicity:",curMulti))
      
      for(roll in 1:6) {
#        print(paste("sum:",sum,"roll:",roll))
        newSum <- sum + roll
        newProd <- curProd * roll
        
        # No need to compute multiplicities if we're already at a sum larger than M
        if(newSum > M)
          break
        
        found <- FALSE
#        print("debug c")
#        print(paste("newSum:",newSum))
        for(i in 1:length(newMatrix[[newSum]])) {
#          print("debug d")
          if(length(newMatrix[[newSum]]) == 0) {
#            print("debug e")
            break
          }
          
          if(newMatrix[[newSum]][[i]][1] == newProd) {
            newMatrix[[newSum]][[i]][2] = newMatrix[[newSum]][[i]][2] + 1
            found <- TRUE
            break
          }
        }
        if(found == FALSE) {
#          print("debug a")
          pos <- length(newMatrix[[newSum]]) + 1
#          print("debug b")
          newMatrix[[newSum]][[pos]] <- c(newProd, 1)
        }
      }
    }
  }

  prevMatrix <- newMatrix 
}