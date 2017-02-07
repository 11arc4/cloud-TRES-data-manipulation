#Calculating nesting growth rates (grams/day)

AllNestlings <- as.list(globalData$nestlings)

NstgGrowth <- data.frame(nestID = rep(NA,10),
                         year= rep(NA, 10),
                         hatchdate=rep(NA, 10), 
                         hatchsize = rep(NA, 10), 
                         fledgesize = rep(NA, 10)
                         
) 

for (nestling in as.list(globalDataNest$nestlings)){
  growthrate <- nestling$calcGrowthRate()
  if (is.na(growthrate)){
    message("growthrate is NA for nestling ", nestling$nestlingCode)
  }
  nestling$addGrowthRateMass(growthrate)
  #if we were able to calculate growth rates then we want to use it in modeling and the relevent data needs to be pulled out!
  if(!is.na(growthrate)){
    nestID <- nestling$fromNest$m_key
    fromNest <- get(nestID, globalDataNest$nests)
    
    NstgGrowth$year [ length(NstgGrowth$year) + 1] <- fromNest$year
    NstgGrowth$hatchdate [ length(NstgGrowth$hatchdate) + 1] <- fromNest$hatchDate
    NstgGrowth$hatchdsize [ length(NstgGrowth$hatchsize) + 1] <- fromNest$hatchSize
    NstgGrowth$year [ length(NstgGrowth$year) +1 ] <- fromNest$year
    NstgGrowth$fledgesize [ length(NstgGrowth$fledgesize) + 1] <- fromNest$fledgeSize
    
  }
  
}


#NICE THIS WORKS 


#OK to actually use this data I will need to pull the data out and put it into a data frame!



