#Calculating nesting growth rates (grams/day)

AllNestlings <- as.list(globalData$nestlings)

nestID = c()
nestyear= c()
hatchdate=c()
hatchsize = c()
fledgesize = c()
massgrowth =c()
nest = c()


for (nestling in as.list(globalDataNest$nestlings)){
  growthrate <- nestling$calcGrowthRate()
  if (is.na(growthrate)){
    message("growthrate is NA for nestling ", nestling$nestlingCode)
  }
  nestling$addGrowthRateMass(growthrate)
  #if we were able to calculate growth rates then we want to use it in modeling and the relevent data needs to be pulled out!
  if(!is.na(growthrate)){
    message("made it")
    nestID <- nestling$fromNest$m_key
    fromNest <- get(nestID, globalDataNest$nests)
    
    nestyear [ length(nestyear) + 1] <- fromNest$year
    hatchdate [ length(hatchdate) + 1] <- fromNest$hatchDate
    hatchsize [ length(hatchsize) + 1] <- fromNest$hatchSize
    year [ length(year) +1 ] <- fromNest$year
    fledgesize [ length(fledgesize) + 1] <- fromNest$fledgeSize
    massgrowth [ length(massgrowth) + 1] <- growthrate
    nest [length(nest) + 1] <- nestID
  }
  
}

NstgGrowth <- cbind(as.integer(nestyear), 
                    as.character(nest), 
                    as.integer(hatchdate), 
                    as.integer(hatchsize), 
                    as.integer(fledgesize), 
                    as.numeric(massgrowth))
colnames(NstgGrowth) <- c("year", "nest", "hatchdate", "hatchsize", "fledgesize", "growthrate_mass")

#NICE THIS WORKS!


#OK to now I can toss this data out onto a csv file and read it in for analysis!

outputdir <-"~/Amelia TRES data 1975-2016/Extracted Data for Analysis"

write.csv(NstgGrowth, file= paste (outputdir, "Nestling Growth Rate Modeling Data.csv", sep= "/"), row.names = FALSE, na= "")




