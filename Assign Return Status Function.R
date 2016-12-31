AssignReturnStatus<- function(inputNestDataDir, hash_allbirds, year, band)


nestdata<-read.csv(paste(inputNestDataDir, fname, sep="/"), 
                   as.is = TRUE, na.strings = c("NA", ""))


#make it so it will run through bot sexes 
for (d in 1:length(attributes)) {
  sex = attributes[[d]][1]
  birdIdKey = attributes[[d]][2]
  
  rtnStatusKey = paste(sex, "Return.Status", sep = ".")
  if (! rtnStatusKey %in% colnames(nestdata)) {
    nestdata[[rtnStatusKey]] <- rep(NA, nrow(nestdata))
  }
  b=0
  
  
  for (birdID in as.character(nestdata[[birdIdKey]])) {
    b = b + 1
    if(!is.na(birdID)){
      if(!exists(birdID, hash_allbirds)){
        print(paste("Warning:", sex, birdID, "from",  year, "not found in band record" ))
        next
      }
      if (is.na(nestdata$Year[b])) {
        print(sprintf("Error: 'Year' for entry %d of %s data is not the right format (SKIPPING)",
                      b, fname ))
        next
      }
      bandIdxList <- get(birdID, hash_allbirds) #list of all the places this band ID shows up
      returnIdx = 0
      for(c in bandIdxList){
        returnIdx = returnIdx + 1
        #For the new bird if, I've included a statement that shows that if the year that it showed up in 
        #first is less than the year that is shown in the banding data, then we mark it as a new bird 
        #because it looks like some of the birds in 1975 didn't get added to the banding data
        if (is.na(band$Year[c])) {
          print(sprintf("Error: year for entry %d in band data is NA (SKIPPING)", c))
          next
        }
        
        if (returnIdx == 1) {
          # this is the first appearance of 'birdId' in the band data.
          #   we expect that the first time we see the bird is when we band it in a nest (so the
          #   first appearance here corresponds to that nesting).
          #   However, if there is missing data, then we might find the bird in band data such that
          #   the band year is less than the nest year for the first entry.)
          
          if(band$Age[bandIdxList[returnIdx]]=="L") {
            next
          } else if (band$Year[c]>=nestdata$Year[b]){
            nestdata[[rtnStatusKey]][b] <- "New"
          } else {
            # this is the bird appearance from some previous year ..
            if (length(bandIdxList) == 1) {
              print(sprintf("birdID %s nesting in %d seen previously in %d - not in band data for this year", birdID,
                            nestdata$Year[b], band$Year[c]))
            }
            #print(sprintf("birdID %s came back in year %d seen previously in year %d", birdID,
            #              nestdata$Year[b], band$Year[c]))
            next
          }
          
        } else {
          # not the first entry for this bird in the band data (i.e., we saw this one in some prior year)
          if (band$Year[c] == nestdata$Year[b] & 
              band$Age[bandIdxList[returnIdx-1]]=="L") {
            yearsSinceLastSeen <- band$Year[bandIdxList[returnIdx-1]] - band$Year[c]
            if (1 == yearsSinceLastSeen) {
              nestdata[[rtnStatusKey]][b] <- "Recruit"
            } else {
              nestdata[[rtnStatusKey]][b] <- "Slow Recruit"
            }
          } else { 
            #currently recruit if your previous sighting was as a nestling (ie had age "L"), regardless of how many years ago that was.
            if (band$Year[c] == nestdata$Year[b]) {            
              nestdata[[rtnStatusKey]][b] <- "Return"
            }#close return if
          }#close else
          
        }#close recruit/return else
      }    
    } #close for
    
  } #closer if
}  
  return(nestdata)
  