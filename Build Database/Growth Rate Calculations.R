#Calculating nesting growth rates (grams/day)


for (nestling in as.list(globalData$nestlings)){
  growthrate <- nestling$calcGrowthRate()
  if (is.na(growthrate)){
    message("growthrate is NA for nestling ", nestling$nestlingCode)
  }
  nestling$addGrowthRateMass(growthrate)
  
}




