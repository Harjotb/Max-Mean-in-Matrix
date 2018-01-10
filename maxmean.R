findmean <- function(cls,x) {
   rowgrps <- splitIndices(nrow(x),length(cls))
   clusterApply(cls,rowgrps,
      function(onegrp) myx <<- x[onegrp,,drop=FALSE])
   results <- clusterEvalQ(cls,t(apply(myx,1,mean)))  
   a <- (Reduce(c,results))
   print(paste0("Max: ", max(a), " Row Number: ", which.max(a)))
   
}
  




maxmean <- function() {
   library(parallel)
   cls <- makeCluster(2)
   
   #make random matrix
   mat <- matrix(nrow = 10000, ncol = 10)
   for(row in 1:10000){
      tempRow <- sample(1:100,10,replace=TRUE)
      mat[row,] <- tempRow
   }
   #make Random Matrix Done

   findmean(cls,mat)
   
}

maxmean()

#Parallel : Time difference of 0.08844757 secs , 10000 ROWS and 10 COLUMNS

#Serial : Time difference of 0.2756121 secs, 10000 ROWS and 10 COLUMNS
