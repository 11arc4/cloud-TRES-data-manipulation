
#' InputNestDatatoClassStructure
#'Runs through the nest data, creating Nests, TreeSwallows (for all nestlings
#'with bands, and all adults), Nestlings (for all nestlings who either have a
#'band number or measurements), and putting them into the globalData object (IE
#'the collections of hash tables to look up each of the elements as needed)
#'
#'NOTE: Dates are all characters, or julian dates in the case of the breeding
#'events dates as they seem to get distorted less anyway
#'
#' @param nestdata The nest data file to be added to the data structures
#' @param globalData The GlobalBirdData object into which we will be entering
#'   all the different Reference Class Objects we create.
#'
#' @return
#' @export
#'
#' @examples
InputNestDatatoClassStructure <- function (nestdata, globalData){
  year<- nestdata$Year[1]
  for (i in 1: length(nestdata$Year)){
    nestID <- paste (as.character(year), nestdata$BoxID[i], sep="-")  #This is the unique
    nest <- Nest(year=year, siteID=as.character(nestdata$siteID[i]) )
    
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
        # we saw this one before...check that the sex is the same as the last
        # time - or maybe this was a nestling
        #   and we didn't know the sex
        if (length(bird$sex)==0 | is.na(bird$sex)){
          bird$sex <- "F"
        } else {
          if ( bird$sex == "M"){
            message("Thats odd.. female ", femaleID, " was male last time.")
            
          }
        }
      }
      
      bird$addNest(nest)
      
      yearentry <- YearsSeen(year= year, #set outside the function when we're going through the nestdata
                             age= as.character (nestdata$F.Age[i]), 
                             returnstatus=NA_character_, 
                             hatchNest=EnvPointer(NA_character_, globalData$nests),
                             nest= list(), #need to put this in a list because the bird might have been involved in multiple nests in a year!
                             observations = list()
      )
      yearentry$addNest (EnvPointer(nestID, globalData$nests))
      
      #If this isn't NA, then we have SOME measurements, and need to add them as an observation
      if(!is.na(nestdata$F.Day.measured[i])){
        
        
        bodymetrics <- BodyMeasurements(date=as.character(nestdata$F.Day.measured[i]), 
                                        bird=bird)
        
        if (!is.na(nestdata$F.Wing..mm.[i])){
          bodymetrics$wingChord= as.numeric(nestdata$F.Wing..mm.[i] )
        }
        if ( !is.na (nestdata$F.Nineth.Primary..mm.[i])){
          bodymetrics$ninthPrimary = as.numeric( nestdata$F.Nineth.Primary..mm.[i])
        }
        if (!is.na(nestdata$F.Mass..g.[i])){
          bodymetrics$mass = as.numeric (nestdata$F.Mass..g.[i])
        }
        if (!is.na( nestdata$F.Tarsus..mm.[i])){
          bodymetrics$tarsus = as.numeric( nestdata$F.Tarsus..mm.[i])
          
        }
        
        bird$addObservation(bodymetrics)
        yearentry$addObservation(bodymetrics)
        
      }
      if(!is.na(nestdata$F.Malaria.Status[i])){
        malaria <- MalariaStatus(date=as.character(nestdata$F.blooddate[i]), 
                                 bird=bird, 
                                 status=nestdata$F.Malaria.Status[i])
        bird$addObservation(malaria)
        yearentry$addObservation (malaria)
        
      }
      
      nest$femaleID <- EnvPointer(femaleID, globalData$birds)
      bird$addYearSeen(yearentry)
      
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
        if (length(bird$sex)==0 | is.na(bird$sex)){
          bird$sex <- "M"
        } else {
          if ( bird$sex == "F"){
            message("Thats odd.. male ", femaleID, " was female last time.")
            
          }
        }
      }
      bird$addNest(nest)
      yearentry <- YearsSeen(year= year, #set outside the function when we're going through the nestdata
                             age=as.character (nestdata$F.Age[i]), 
                             returnstatus=NA_character_, 
                             hatchNest=EnvPointer(NA_character_, globalData$nests),
                             nest= list(), #need to put this in a list because the bird might have been involved in multiple nests in a year!
                             observations = list()
      )
      yearentry$addNest (EnvPointer(nestID, globalData$nests))
      if(!is.na(nestdata$M.Day.measured[i])){
        
        
        bodymetrics <- BodyMeasurements(date=as.character(nestdata$M.Day.measured[i]), 
                                        bird=bird)
        
        if (!is.na(nestdata$M.Wing..mm.[i])){
          bodymetrics$wingChord= as.numeric (nestdata$M.Wing..mm.[i] )
        }
        if ( !is.na (nestdata$M.Nineth.Primary..mm.[i])){
          bodymetrics$ninthPrimary = as.numeric( nestdata$M.Nineth.Primary..mm.[i])
        }
        if (!is.na(nestdata$M.Mass..g.[i])){
          bodymetrics$mass = as.numeric (nestdata$M.Mass..g.[i])
        }
        if (!is.na( nestdata$M.Tarsus..mm.[i])){
          bodymetrics$tarsus = as.numeric (nestdata$M.Tarsus..mm.[i])
          
        }
        
        bird$addObservation(bodymetrics)
        yearentry$addObservation (bodymetrics)
        
        
      }
      if(!is.na(nestdata$M.Malaria.Status[i])){
        malaria <- MalariaStatus(date=as.character(nestdata$M.blooddate[i]), 
                                 bird=bird, 
                                 status=nestdata$M.Malaria.Status[i])
        bird$addObservation(malaria)
        yearentry$addObservation (malaria)
        
      }
      nest$maleID <- EnvPointer(maleID, globalData$birds)
      bird$addYearSeen(yearentry)
      
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
    globalData$insertNest(nestID= nestID, Nest=nest)
  }
}
