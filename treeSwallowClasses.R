


NestPointer <- setRefClass("NestPointer", 
                           fields= list(
                             nest= "character",    #"year box ID" will be this nest 
                             nest_hash="environment"
                           )
                            )
NestPointer$methods(
  initialize=function(nestID=NA_character_, nest_hash=emptyenv()){
    .self$nest <<- nestID
    .self$nest_hash <<- nest_hash
  getnest = function() {
    get(nest, nest_hash)
  }
    
  }
)

#Use this class to make a method that returns tree swallow based on bandID 

Nest <- setRefClass("Nest",
                        
                        fields = list(
                          
                          year = "numeric",
                          lineNumber = "numeric",
                          siteID= "character",   
                          #unique code for the nestID
                          
                          #we can't do the circular referencing like we'd like
                          #to so we'll put it in as a character and use the
                          #hash_table as an intermediary to find the band IDs
                          maleID = "TreeSwallowPointer",
                          femaleID = "TreeSwallowPointer",
                          nestlings = "list",  
                          #Nestlings needs to be a list of Nestlings, which will
                          #get a TreeSwallow bandID reference if and only if
                          #they turn into an adult tree swallow in our data.
                          
                          #Dates are all of class integer because they're all julian dates
                          firstEggDate = "integer",
                          lastEggDate="integer",
                          hatchDate= "integer",
                          fledgeDate= "integer",
                          
                          clutchSize= "integer",
                          hatchSize= "integer",
                          fledgeSize= "integer",
                          reasonforFailure = "character", 
                          renestStatus ="character")
                        
)


Nest$methods ( 
  initialize = function (year, siteID,  nestlings = list(), firstEggDate=NA_integer_, lastEggDate=NA_integer_,
                         hatchDate=NA_integer_, fledgeDate=NA_integer_, clutchSize=NA_integer_, 
                         hatchSize=NA_integer_, fledgeSize=NA_integer_, reasonforFailure=NA_character_, 
                         renestStatus=NA_character_ ){
    .self$year <<- year
    line <-lineNumber
    .self$siteID <<- siteID
    .self$nestlings <<-nestlings  
    .self$firstEggDate <<- firstEggDate
    .self$lastEggDate <<- lastEggDate
    .self$hatchDate <<- hatchDate
    .self$fledgeDate <<- fledgeDate
    .self$clutchSize <<-clutchSize
    .self$hatchSize <<- hatchSize
    .self$fledgeSize <<-fledgeSize
    .self$reasonforFailure <<- reasonforFailure 
    .self$renestStatus <<- renestStatus
  }, 
  addMale = function (malePointer){
    maleID <<- malepointer
  }, 
  addFemale = function (femalePointer){
    femaleID <<- femalePointer
  }
  )



#########
TreeSwallowPointer <- setRefClass("TreeSwallowPointer", 
fields= list(
  bandID = "character", 
  bird_hash = "environment"
  
))

#make sure that you always set the bird_hash--the function will not do what
#you'd like to do if you use the empty environment default
TreeSwallowPointer$methods(
  initialize = function (bandID=NA_character_, bird_hash=emptyenv()){
    .self$bandID <<- bandID
    .self$bird_hash <<- bird_hash
  }, 
  getbird= function() {
    get(bandID, bird_hash )
  }
)
TreeSwallow <- setRefClass("TreeSwallow",
                         fields = list(
                            bandID = "character",
                            sex = "character",
                            hatchnest = "NestPointer", 
                            # a pointer to the nest record from the nest where I hatched 
                            nestList = "list", #(nestData, nestData)
                            
                            observations = "list") 
                         #all of the things I know about an individual bird based on year
)

TreeSwallow$methods(
  initialize = function(bandID=NA_character_, 
                        sex=NA_character_, nestList=list(), 
                        hatchnest = NestPointer(nest="NA", nest_hash)) {
  
    .self$bandID <<- bandID
    .self$hatchnest <<- hatchnest
    .self$nestList <<- list()
    .self$observations <<- list()
  },
  #need a function to add a new nest to the TreeSwallow record
  addNest = function(nest) {
    append(x=nestList, values=nest)
  },
  #Also need a function to add a new observation of the TreeSwallow
  addObservation = function(obs) {
    append(observations, obs)
  }
)
NestlingPointer <- setRefClass("NestlingPointer", fields= list(
  nestlingCode= "character",
  nestling_hash= "environment"
))

NestlingPointer$methods (
  initialize= function( nestlingCode, nestling_hash) {
    .self$nestlingCode <<- nestlingCode
  .self$nestling_hash <<- nestling_hash
  }
)


Nestling <- setRefClass("Nestling", 
                        fields = list (
                          #measurements = "BodyMeasurements",
                          nestlingCode= "character",
                          nestlingTRES = "TreeSwallowPointer", 
                          fromNest = "NestPointer", 
                          nestID="character"
                        ))
Nestling$methods(
  initialize = function (nestlingCode,  nestlingTRES=TreeSwallowPointer("NA", bird_hash), fromNest,  nestID, measurements=ls()){
    .self$fromNest <<- fromNest
    .self$nestlingCode <<- nestlingCode
    .self$nestID <<-nestID
    #.self$measurements <<- measurements
  }
)
CreateNestlingFromNestData = function(nestdata, chicknumber, rownumber ){
  nestID <- paste (as.character(nestdata$Year[1]), nestdata$BoxID[rownumber], sep="-")  #This is the unique 
  nestlingCode <- paste( nestID, "nestling", as.character(chicknumber) )
  band <- as.character(nestdata[rownumber, bandoptions[chicknumber]])
  chickTRES <- TreeSwallow(bandID = band,  
                           hatchnest= NestPointer(nestID=nestID, nest_hash)
  )
  
  chick <- Nestling(fromNest=NestPointer(nestID=nestID, nest_hash), 
                    nestlingCode=nestlingCode, 
                    nestID=nestID, 
                    nestlingTRES=chickTRES)
  
  return(chick)
}



  ##########

Observation <- setRefClass("Observation",
                           fields = list(
                             date = "Date",
                             type = "character",
                             bird = "TreeSwallow"
                           ))



BodyMeasurements <- setRefClass("BodyMeasurements",
                                contains = "Observation",
                                fields = list (
                                  wingChord = "numeric",
                                  ninthPrimary = "numeric",
                                  mass = "numeric",
                                  tarsus = "numeric"
                                ))

EggMass<-setRefClass("EggMass", 
                     contains="Observation", 
                     fields= list(
                       EggMass = "numeric"
                     ))

MalariaCheck <- setRefClass("MalariaCheck",
                            contains = "Observation",
                            fields = list(
                              status = "character"
                            ))
