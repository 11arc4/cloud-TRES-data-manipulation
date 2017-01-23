outerdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/4 malaria status added"
listfilenames <- list.files(outerdir)
filename <- paste( outerdir, listfilenames[15], sep="/")
nestdata<- read.csv(filename, as.is=TRUE, na.strings=c("", "NA"))


#Currently the dates are all in the wrong format and will need to be fixed before they can be used. 
is.date()
nestdata <- CleanUpNestDataObservationDates (nestdata)

globalData <- GlobalBirdData()



#Here I am going though the nest data. I will add all the nests, and all their observations




year<- nestdata$Year[1]
for (i in 1: length(nestdata$Year)){
  nestID <- paste (as.character(year), nestdata$BoxID[i], sep="-")  #This is the unique
  nest <- Nest(year=year, siteID=nestdata$siteID[i] )
  
  #Need to create (or append) sightings of the parents as TreeSwallows
  femaleID <- as.character(nestdata$FemaleID[i])
  if (! is.na(femaleID)) {
    
    # look for this bird is the globalData
    if (!exists(femaleID, globalData$birds)) {
      # this is the first time we have seen this female...buid a TreeSwallow for it...
      bird <- TreeSwallow(bandID=femaleID, sex="F")
    } else {
      bird <- globalData$findBird(femaleID)
      # we saw this one before...check that the sex is the same as the last time - or maybe this was a nestling
      #   and we didn't know the sex
      if ("F" != bird$sex()) {
        message("Thats odd.. female ", femaleID, " was male last time.")
      }
    }
    bird$addNest(nest)
    
    #If this isn't NA, then we have SOME measurements, and need to add them as an observation
    if(!is.na(nestdata$F.Day.measured[i])){
      
      
      bodymetrics <- BodyMeasurements(date=nestdata$F.Day.measured[i], 
                                      bird=bird, 
                                      wingChord = nestdata$F.Wing..mm.[i], 
                                      ninthPrimary = nestdata$F.Nineth.Primary..mm.[i],
                                      mass = nestdata$F.Mass..g.[i], 
                                      tarsus = nestdata$F.Tarsus..mm.[i])
      bird$addObservation(bodymetrics)
    }
    if(!is.na(nestdata$F.Malaria.Status[i])){
      malaria <- MalariaStatus(date=nestdata$F.blooddate[i], 
                               bird=bird, 
                               status=nestdata$F.Malaria.Status[i])
      bird$addObservation(malaria)
    }

    globalData$insertBird(bird)
    nest$femaleID <- EnvPointer(femaleID, globalData$birds)
  }
 
   maleID <- as.character(nestdata$MaleID[i])
  
  if (! is.na(maleID)) {
    
    # look for this bird is the globalData
    if (!exists(maleID, globalData$birds)) {
      # this is the first time we have seen this female...buid a TreeSwallow for it...
      bird <- TreeSwallow(bandID=maleID, sex="M")
    } else {
      bird <- globalData$findBird(maleID)
      
      # we saw this one before...check that the sex is the same as the last time - or maybe this was a nestling
      #   and we didn't know the sex
      if ("M" != bird$sex()) {
        message("Thats odd.. male ", maleID, " was female last time.")
      }
    }
    bird$addNest(nest)
    if(!is.na(nestdata$M.Day.measured[i])){
      
      
      bodymetrics <- BodyMeasurements(date=nestdata$M.Day.measured[i], 
                                      bird=bird, 
                                      wingChord = nestdata$M.Wing..mm.[i], 
                                      ninthPrimary = nestdata$M.Nineth.Primary..mm.[i],
                                      mass = nestdata$M.Mass..g.[i], 
                                      tarsus = nestdata$M.Tarsus..mm.[i])
      bird$addObservation(bodymetrics)
      
    }
    if(!is.na(nestdata$M.Malaria.Status[i])){
      malaria <- MalariaStatus(date=nestdata$M.blooddate[i], 
                               bird=bird, 
                               status=nestdata$M.Malaria.Status[i])
      bird$addObservation(malaria)
    }
    globalData$insertBird(bird)
    nest$maleID <- EnvPointer(maleID, globalData$birds)
    
  }
  
  # call a method on the nest to populate the rest of the data
  #nest$populateMesaurements(nestdata, rownumber)
  
  #add all the important dates and breeding success measurements to the nest
  nest$addDatesandSuccessNestdata(nest, nestdata, i )
  
  #Need to make all the nestlings, and also need to add them to the data
  for (j in 1:10){
    #This creates the Nestling, and creates an associated TreeSwallow if applicable
    nestling<- globalData$buildNestling(nestdata=nestdata, 
                                        chicknumber=j, 
                                        rownumber=i, 
                                        dataSingleton = globalData)
    #add the Nestling to the Nest
    nest$addNestling(EnvPointer(nestling$nestlingCode, globalData$nestlings))
    #add the Nestling to the nestlings hash
    globalData$insertNestling(nestling)
    
  }
  globalData$insertNest(nest)

}


#as.list(globalData$nests)
#pulls out all the stuff in nest_hash (the actual stuff not just the names)