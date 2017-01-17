library(assertthat)


TreeSwallowPointer$methods(
  initialize = function (bandID, bird_hash){
    .self$bandID <<- bandID
    .self$bird_hash <<- bird_hash
  }, 
  getbird= function() {
    get(bandID, bird_hash )
  }
)



configureFromNestDataFile = function(nestDataFrame, line, bird_hash) {
  #lineNumber <<- line 
  #Could add in lineNumber reference but I don't think it's really necessary
  #since we are now including all the dates and stuff
  assert_that(year == nestDataFrame$Year[line])
  
  #Here are the bird parents
  if (! is.na(nestDataFrame$femaleID[line]) ) {
    
    femaleID <- nestDataFrame$femaleID[line]
    if(exists(femaleID, bird_hash)) {
      female <<- TreeSwallowPointer( femaleID, bird_hash)
    } else {
      bird<- 
        assign( )
    }
    # find the female from its ID and enter it here
  }
  if (!is.na(nestDataFrame$maleID[line])){
    male <<- nestDataFrame$maleID[line]
  }
  
  
  #Here are all the important dates
  if (!is.na(nestDataFrame$First.Egg.Date[line])){
    FirstEggDate <<- nestDataFrame$First.Egg.Date[line]
  }
  if(!is.na(nestDataFrame$Incubation.Date[line])){
    LastEggDate<<-nestDataFrame$Incubation.Date[line]
  }
  
  if(!is.na(nestDataFrame$Hatch.Date[line])){
    HatchDate <<- nestDataFrame$Hatch.Date[line]
  }
  if(!is.na(nestDataFrame$Fledge.Fail.date[line])){
    FledgeDate <<- nestDataFrame$Fledge.Fail.date[line]
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
    .self$bandId <<- id
    .self$nestling <<- nestlingRecord
    .sefl$nestList <<- list()
    .self$observations <<- list()
  },
  addNesting = function(nest) {
    append(nestList, nest)
  },
  addObservation = function(obs) {
    append(observations, obs)
  }
)
