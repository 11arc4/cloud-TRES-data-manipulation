#Calculating nesting growth rates (grams/day)

AllNestlings <- as.list(globalData$nestlings)


for (nestling in as.list(globalDataNest$nestlings)){
  growthrate <- nestling$calcGrowthRate()
  if (is.na(growthrate)){
    message("growthrate is NA for nestling ", nestling$nestlingCode)
  }
  nestling$addGrowthRateMass(growthrate)
}



#Issue is that when growth rate is NA, then the growthrate is 