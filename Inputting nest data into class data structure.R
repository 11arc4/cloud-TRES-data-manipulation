outerdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/4 malaria status added"
listfilenames <- list.files(outerdir)
filename <- paste( outerdir, listfilenames[16], sep="/")
nestdata<- read.csv(filename, as.is=TRUE, na.strings=c("", "NA"))


#Dates are all characters now so I'll input them into the file structure as a
#character--that seems to get distorted less anyway


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
      globalData$insertBird(bird)
      
    } else {
      bird <- globalData$findBird(femaleID)
      # we saw this one before...check that the sex is the same as the last time - or maybe this was a nestling
      #   and we didn't know the sex
      if (length(bird$sex)==0){
        bird$sex <- "F"
      } else {
        if ( bird$sex == "M"){
          message("Thats odd.. female ", femaleID, " was male last time.")
          
        }
      }
    }
    
    bird$addNest(nest)
    
    #If this isn't NA, then we have SOME measurements, and need to add them as an observation
    if(!is.na(nestdata$F.Day.measured[i])){
      
      
      bodymetrics <- BodyMeasurements(date=nestdata$F.Day.measured[i], 
                                      bird=bird)
      
      if (!is.na(nestdata$F.Wing..mm.[i])){
        bodymetrics$wingChord=nestdata$F.Wing..mm.[i] 
      }
      if ( !is.na (nestdata$F.Nineth.Primary..mm.[i])){
        bodymetrics$ninthPrimary = nestdata$F.Nineth.Primary..mm.[i]
      }
      if (!is.na(nestdata$F.Mass..g.[i])){
        bodymetrics$mass = nestdata$F.Mass..g.[i]
      }
      if (!is.na( nestdata$F.Tarsus..mm.[i])){
        bodymetrics$tarsus = nestdata$F.Tarsus..mm.[i]
        
      }
      
      bird$addObservation(bodymetrics)
    }
    if(!is.na(nestdata$F.Malaria.Status[i])){
      malaria <- MalariaStatus(date=nestdata$F.blooddate[i], 
                               bird=bird, 
                               status=nestdata$F.Malaria.Status[i])
      bird$addObservation(malaria)
    }
    
    nest$femaleID <- EnvPointer(femaleID, globalData$birds)
  }
  
  maleID <- as.character(nestdata$MaleID[i])
  
  if (! is.na(maleID)) {
    
    # look for this bird is the globalData
    if (!exists(maleID, globalData$birds)) {
      # this is the first time we have seen this female...buid a TreeSwallow for it...
      bird <- TreeSwallow(bandID=maleID, sex="M")
      globalData$insertBird(bird)
      
    } else {
      bird <- globalData$findBird(maleID)
      
      # we saw this one before...check that the sex is the same as the last time - or maybe this was a nestling
      #   and we didn't know the sex
      if (length(bird$sex)==0){
        bird$sex <- "M"
      } else {
        if ( bird$sex == "F"){
          message("Thats odd.. male ", femaleID, " was female last time.")
          
        }
      }
    }
    bird$addNest(nest)
    if(!is.na(nestdata$M.Day.measured[i])){
      
      
      bodymetrics <- BodyMeasurements(date=nestdata$M.Day.measured[i], 
                                      bird=bird)
      
      if (!is.na(nestdata$M.Wing..mm.[i])){
        bodymetrics$wingChord=nestdata$M.Wing..mm.[i] 
      }
      if ( !is.na (nestdata$M.Nineth.Primary..mm.[i])){
        bodymetrics$ninthPrimary = nestdata$M.Nineth.Primary..mm.[i]
      }
      if (!is.na(nestdata$M.Mass..g.[i])){
        bodymetrics$mass = nestdata$M.Mass..g.[i]
      }
      if (!is.na( nestdata$M.Tarsus..mm.[i])){
        bodymetrics$tarsus = nestdata$M.Tarsus..mm.[i]
        
      }
      
      bird$addObservation(bodymetrics)
      
    }
    if(!is.na(nestdata$M.Malaria.Status[i])){
      malaria <- MalariaStatus(date=nestdata$M.blooddate[i], 
                               bird=bird, 
                               status=nestdata$M.Malaria.Status[i])
      bird$addObservation(malaria)
    }
    nest$maleID <- EnvPointer(maleID, globalData$birds)
    
  }
  
  # call a method on the nest to populate the rest of the data
  #nest$populateMesaurements(nestdata, rownumber)
  
  #add all the important dates and breeding success measurements to the nest
  nest$addDatesandSuccessNestdata(nest, nestdata, i )
  
  
  n_measures <- c("mass.d", "tarsus.d", "wing.d")
  
  N_obs <- c( "mass", 
              "tarsus",
              "ninthPrimary", 
              "age", 
              "nestling" 
  )
  #Need to make all the nestlings, and also need to add them to the data
  for (j in 1:10){
    #This creates the Nestling, and creates an associated TreeSwallow if applicable
    nestling<- globalData$buildNestling(nestdata=nestdata, 
                                        chicknumber=j, 
                                        rownumber=i, 
                                        dataSingleton = globalData)
    
    #sometimes nestlings will exist as null nestlings if there aren't columns 1-10
    #in the nest data so we need to double check to make sure that a nestling
    #actually has been created.
    if (!is.null(nestling)){
      
      for (day in 1:18){
        nestlingObs <- NestlingMeasurements(age=day)
        h=0
        for (m in 1: length(n_measures)){
          measurement <- paste( n_measures[m], day, ".", j, sep="")
          if(exists( measurement, nestdata)){
            if (!is.na (nestdata[ i, measurement])){
              h=h+1
              nestlingObs[[N_obs[m]]]<- as.numeric(nestdata[i,  measurement])
            } 
            
          }
        }
        #Want to add all the different measurements from that day together and
        #THEN check to see if there are any measurements made, and add it onto
        #the Nestling$measurements
        if(h>0){
        nestling$addObservation(nestlingObs)
        }
        
      }
      if ( nestling$nestlingTRES$isNull() | 
          length(nestling$measurements)==0) {
        #if the nstling wasn't banded and we know nothing about them
        next
      } else {
        #we know somehting about them and should put them into the nest and the nestlings database
        #add the Nestling to the Nest
        nest$addNestling(EnvPointer(nestling$nestlingCode, globalData$nestlings))
        #add the Nestling to the nestlings hash
        globalData$insertNestling(nestling)
      }
    }
  }
  globalData$insertNest(nest)
}

#as.list(globalData$nests)
ls(all.names=TRUE, globalData$nests)
#pulls out all the stuff in nest_hash (the actual stuff not just the names)