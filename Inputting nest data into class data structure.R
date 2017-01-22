outerdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/4 malaria status added"
listfilenames <- list.files(outerdir)
filename <- paste( outerdir, listfilenames[1], sep="/")
nestdata<- read.csv(filename, as.is=TRUE, na.strings=c("", "NA"))


globalData <- GlobalBirdData()





year<- nestdata$Year[1]
for (i in 1: length(nestdata$Year)){
  nestID <- paste (as.character(year), nestdata$BoxID[i], sep="-")  #This is the unique
  nest <- Nest(year=year, siteID=nestdata$siteID[i] )
  
  #Need to create (or append) sightings of the parents as TreeSwallows
  femaleID <- as.character(nestdata$FemaleID[i])
  if (! is.na(femaleID)) {
    
    # look for this bird is the globalData
    bird <- globalData$findBird(femaleID)
    if (is.na(bird)) {
      # this is the first time we have seen this female...buid a TreeSwallow for it...
      bird <- TreeSwallow(bandID=femaleID, sex="F")
    } else {
      # we saw this one before...check that the sex is the same as the last time - or maybe this was a nestling
      #   and we didn't know the sex
      if ("F" != bird$sex()) {
        message("Thats odd.. female ", femaleID, " was male last time.")
      }
    }
    bird$addNest(nest)
    globalData$insertBird(bird)
    nest$femaleID <- EnvPointer(femaleID, globalData$birds)
  }
 
   maleID <- as.character(nestdata$MaleID[i])
  
  if (! is.na(maleID)) {
    
    # look for this bird is the globalData
    bird <- globalData$findBird(maleID)
    if (is.na(bird)) {
      # this is the first time we have seen this female...buid a TreeSwallow for it...
      bird <- TreeSwallow(bandID=maleID, sex="M")
    } else {
      # we saw this one before...check that the sex is the same as the last time - or maybe this was a nestling
      #   and we didn't know the sex
      if ("M" != bird$sex()) {
        message("Thats odd.. male ", maleID, " was female last time.")
      }
    }
    bird$addNest(nest)
    globalData$insertBird(bird)
    nest$maleID <- EnvPointer(maleID, globalData$birds)
    
  }
  
  # call a method on the nest to populate the rest of the data
  #nest$populateMesaurements(nestdata, rownumber)
  
  #add all the important dates and breeding success measurements to the nest
  nest$addDatesandSuccessNestdata(nest, nestdata, i )
  
  #Need to make all the nestlings, and also need to add them to the data
  for (j in 1:10){
    nestling<- globalData$buildNestling(nestdata=nestdata, chicknumber=j, rownumber=i)
    nest$addNestling(EnvPointer(nestling, globalData$nestlings))
    
    
  }

  
}


#as.list(nest_hash)
#pulls out all the stuff in nest_hash (the actual stuff not just the names)