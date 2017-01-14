# return updated nest data (with return status for male and female updated)

#' Title
#'
#' @param nestdata 
#' @param year 

#' @param hash_globalBirdData 
#'
#' @return
#' @export
#'
#' @examples
AssignReturnStatus_fromnestdata <- 
  function(nestdata, # frame of nest data for year
           year, # integer year of this nest data
           
           hash_globalBirdData = NA # some appearance data for every bird 
           # we have eve seen (to date)
  ) {
    
    attributes = list(c("F", "FemaleID"),
                      c("M", "MaleID"))
    
    #make it so it will run through both sexes 
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
        if (is.na(birdID)) {
          next #THIS IS PROBABLY A GOOD PLACE TO PUT IN CODE THAT WILL UPDATE YOUR NEST DATA BAND IDS 
          #BASED ON BANDING DATA EG FOR THE NORTH EAST SANCTUARY WHERE THERE ARE BIRDS BANDED BUT THEY
          #AREN'T ASSIGNED IN THE NEST RECORDS
        }
        # first part:  look to see if this bird is in the global data or not
        # (should always be there - if not, then there is a bug in the data...)
        # - look at the last entry in the bird's data - to see what we know about
        #   the bird from the last time we saw it (was it a nestling, a male, or a
        #   female, what year was it)
        
        if (is.environment(hash_globalBirdData)) {
          if (! exists(birdID, hash_globalBirdData)) {
            #warning(year, "  found ", birdID, " in nest data - but no entry in global bird DB")
            #The above warning will show up in all new birds because they won't
            #have been added to the global bird hash until AFTER I run this
            #function so it's not really useful
            nestdata[[rtnStatusKey]][b] <- "New"
          } else {
            prev = tail(get(birdID, hash_globalBirdData), n = 1)[[1]]
            if ("N" == prev[[2]]) {
              # nestling...
              if(!is.na(prev[[1]])){
                nestdata[[rtnStatusKey]][b] <- ifelse(prev[[1]] == (year - 1),
                                         "Recruit", 
                                         paste("Slow Recruit from", year-as.integer(prev[[1]]) , "ago", sep=" "))
              } else {
                message("Bird", birdID, 
                        "was a nestling last time they showed up but that was in an unknown year")
              }
            } else {
              nestdata[[rtnStatusKey]][b] <- "Return"
            }
           
            
          }
        }
        
        
      }
    }
    return(nestdata)
  }



      


AssertReturnStatus_fromBandData<-function(
  nestdata,
  hash_allbirds, # all the birds which appear in the band data
  band){ # frame of band data) 
  library(assertthat)
  attributes = list(c("F", "FemaleID"),
                    c("M", "MaleID"))
  for (d in 1:length(attributes)) {
    sex = attributes[[d]][1]
    birdIdKey = attributes[[d]][2]
    # second part of the processing...look in band data for this year...
    
    b=0
    for(birdID in as.character(nestdata[[birdIdKey]])){
      b=b+1
      if(!is.na(birdID)){
        if(!exists(birdID, hash_allbirds)){
          message(sex, birdID, " from ",  year, " not found in band record")
          next
        } 
        if (is.na(nestdata$Year[b])) {
          warning("'Year' for entry ", fname, ":", b, 
                  " is not the right format (SKIPPING)")
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
            warning("year for entry ", c, " in band data is NA (SKIPPING)")
            next
          }
          
          if (returnIdx == 1) {
            # this is the first appearance of 'birdId' in the band data.
            #   we expect that the first time we see the bird is when we band it in a nest (so the
            #   first appearance here corresponds to that nesting).
            #   However, if there is missing data, then we might find the bird in band data such that
            #   the band year is less than the nest year for the first entry.)
            
            if(band$Age[bandIdxList[returnIdx]]=="L") {
              # bird is a nestling this year...maybe should add to global DB here?
              next
            } else {
              if (band$Year[c] >= nestdata$Year[b]) {
              # bird either appears in the band data for the first time this year (==)
              #  or bird doesn't appear this year but DOES appear again in the future (>)
              #  mark it as a new bird this year.
              # That is:  we found a bird in a nest (this year) - but there may not be
              #  an entry in the 'band' data this year.
              assert_that(nestdata[[rtnStatusKey]][b]=="New")
            } else {
              # this is the bird appearance from some previous year ..
              if (length(bandIdxList) == 1) {
                message("bird ", birdID, " nesting in ", nestdata$Year[b], 
                        " seen previously in ", band$Year[c], 
                        " - not in band data for this year")
              }
              #print(sprintf("birdID %s came back in year %d seen previously in year %d", birdID,
              #              nestdata$Year[b], band$Year[c]))
              next
            }
            
          } else {
            # not the first entry for this bird in the band data (i.e., we saw
            # this one in some prior year)
            if (band$Year[c] == nestdata$Year[b] & 
                band$Age[bandIdxList[returnIdx-1]]=="L") {
              yearsSinceLastSeen <- band$Year[bandIdxList[returnIdx-1]] - band$Year[c]
              if (1 == yearsSinceLastSeen) {
                nestdata[[rtnStatusKey]][b] <- "Recruit"
              } else {
                assert_that(nestdata[[rtnStatusKey]][b] <-  paste("Slow Recruit from", yearsSinceLastSeen , "ago", sep=" "))
              }
            } else { 
              
              
              if (band$Year[c] == nestdata$Year[b]) {            
                assert_that(nestdata[[rtnStatusKey]][b] <- "Return")
              }
            }
          }
          
        }
        
      }
    }
  } 
  return (nestdata)
}
