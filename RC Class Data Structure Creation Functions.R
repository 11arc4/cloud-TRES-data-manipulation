
#First we need to set up a function that will add all the data from a nest into the 
NestData$methods(
  initialize = function(year, lineNumber, date = NA, male = NA,
                        female=NA, nestlings = list()) {
    #or maybe pass the 'nestData' file, line number, and year - and we figure it out
    .self$date <<- date
    .self$year <<- year
    .self$lineNumber <<- lineNumber
    .self$male <<- male
    .self$female <<- female
    .self$nestlings <<- nestlings
    
    #male$addNesting(.self)
  },
  configureFromNestDataFile = function(nestDataFrame, birdHash, line) {
    lineNumber <<- line
    year <<- nestDataFrame$Year[line]
    
    #Here are the bird parents
    if (! is.na(nestData$femaleID[line]) ) {
     female <<- nestDataFrame$femaleID[line]
      # find the female from its ID and enter it here
    }
    if (!is.na(nestDataFrame$maleID[line])){
      male <<- nestDataFrame$maleID[line]
    }
    
    #Need to add in a list of nestlings--create that before
    nestlings <<- nestlingbandIDlist  #Nestlings needs to be made into a list and entered!
    
    #Here are all the important dates
    if (!is.na(nestDataFrame$First.Egg.Date[line])){
      FirstEggDate <<- nestDataFrame$First.Egg.Date[line]
    }
    if(!is.na(nestDataFrame$Incubation.Date[line])){
      LastEggDate<<-nestDataFrame$Incubation.Date[line]
    }
    
    if(!is.na(nestdataframe$Hatch.Date[line])){
      HatchDate <<- nestdataframe$Hatch.Date[line]
    }
    if(!is.na(nestdataframe$Fledge.Fail.date[line])){
      FledgeDate <<- nestdataframe$Fledge.Fail.date[line]
    }

    #Breeding success measurements
    if(!is.na(nestdataframe$Clutch.Size[line])){
      ClutchSize <<- nestdataframe$Clutch.Size[line]
    }
    if(!is.na(nestdataframe$Hatch.Size[line])){
      HatchSize <<- nestdataframe$Hatch.Size[line]
    }
    if(!is.na(nestdataframe$Fledge.Size[line])){
      FledgeSize <<- nestdataframe$Fledge.Size[line]
    }
    if(!is.na(nestdataframe$Fledge.Size[line])){
      ReasonforFailure <<- nestdataframe$Why.fail.[line]
    }
    
  },
  addNestling = function(n) {
    # should probably assert that the type of 'n' is "TreeSwallow"
    append(nestlings, n)
  }
)







TreeSwallow$methods(
  initialize = function(id, nestlingRecord=NA) {
    bandId <<- id
    nestling <<- nestlingRecord
    nestList <<- list()
    observations <<- list()
  },
  addNesting = function(nest) {
    append(nestList, nest)
  },
  addObservation = function(obs) {
    append(observations, obs)
  }
)
