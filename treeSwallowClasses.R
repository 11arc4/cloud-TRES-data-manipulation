library(assertthat)




#idea of an expanding list object is modified from JanKanis's function on
#stackoverflow which allows us to properly add in a new list
# http://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time-o1
useableList <-  setRefClass("useableList", 
                            fields= list (
                              buffer = "list", 
                              length = "numeric",  #the length of elements in the structure that you really care about
                              capacity = "numeric"
                            ))

useableList$methods (
  initialize = function (capacity= 15){
    .self$capacity <<- capacity
    .self$buffer <<- vector('list', capacity) #making a list of length "capactity"
    .self$length <<- 0
    
  }, 
  double.size = function() {
    .self$buffer <- c(.self$buffer, vector('list', .self$capacity))
    .self$capacity <- .self$capacity * 2
  }, 
  addElement = function (element){
    if(.self$length == .self$capacity) {
      methods$double.size()
    }
    
    .self$length <- .self$length + 1
    buffer[[.self$length]] <<- element
  }, 
  as.list = function() {
    b <- buffer[0: .self$length]
    return(b)
  }
  
)



# a workaround to enable circular references in RC classes:
#   - ultimately, the reference object will be put into an environment (hash table)
#     so it can be looked up by name
#   - the pointer may be NULL (not point to anything) - because we might not know
#     the female bird ID is a nest, or might not know which nest a particular bird
#     was hatched.
#   - thus, we store the key in this structure.  If the pointer is NULL, then we
#     store NA as the key.
EnvPointer <- setRefClass("EnvPointer",
                          fields = list(
                            m_key = "character",
                            m_hash = "environment"
                          )
)
EnvPointer$methods(
  initialize = function(id=NA_character_, hash=emptyenv()) {
    .self$m_key <<- id
    .self$m_hash <<- hash
    assert_that( is.na(id) | ! identical(hash, emptyenv()))
  },
  isNull = function() {
    is.na(m_key)
  },
  get = function() {
    # return RC object which is pointed to or NA if pointer is NULL
    ifelse(isNull(), NA, get(m_key, m_hash))
  }
)

GlobalBirdData <- setRefClass("GlobablBirdData",
                              fields = list(
                                birds = "environment", # bandID -> TreeSwallow
                                nestlings= "environment", #nestlingCode -> nest ->TreeSwallow
                                nests = "environment", # nestID -> Nest
                                nestsByYear = "environment" # year -> vector of Nest
                              )
)

GlobalBirdData$methods(
  initialize = function() {
    birds <<- new.env(hash=TRUE, parent=emptyenv())  #this hash is only going to contain adult birds!
    nestlings <<- new.env(hash=TRUE, parent=emptyenv())  #this hash is only going to contain nestlings!
    nests <<- new.env(hash=TRUE, parent=emptyenv())
    nestsByYear <<- new.env(hash=TRUE, parent=emptyenv())
  },
  findBird = function(bandID) {
    get0(bandID, envir=birds, ifnotfound = NULL)
  },
  findNest = function(nestID) {
   get0(nestID, envir = birds, ifnotfound = NULL)
 },
  insertBird = function(bird) {
    #assert_that(!exists(bird$bandID, envir= .self$birds)) # this bird should not already be in the hash...
    assign(bird$bandID, bird, .self$birds)
  },
  findNestling= function (nestlingCode){
    get0(nestlingCode, envir=nestlings, ifnotfound=NULL)
  },
  insertNestling = function (nestling){
    #assert_that( !exists(nestling$nestlingCode, envir= .self$nestlings))
    assign(nestling$nestlingCode, nestling, .self$nestlings) 
    
  },
#' insertNest
#'
#' @param nestID unique code for the nest example: "1976-BGA3"
#' @param Nest 

  insertNest =function (nestID, Nest){
    #assert_that( !exists(Nest$siteID, envir= .self$nests))
    assign(nestID, Nest, .self$nests) 
  }, 
  buildNestID = function(year, boxID) {
    paste(as.character(year), boxID, sep="-")
  },
  
  buildNest = function(nestdata, rownumber) {
    for (idx in 1:12) {
      nestlingKey = paste("nestling", as.character(idx), sep = ".")
      if (! is.na(nestdata[[nestlingkey, rownumber]])) {
        nestling = self.buildNestling(nestdata, idx, rownumber)
      }
    }
  }, 
  buildNestling = function(nestdata,
                           chicknumber,
                           rownumber, 
                           dataSingleton) {
    nestID <-
      .self$buildNestID(year = nestdata$Year[1], boxID = nestdata$BoxID[rownumber])  #This is the unique
    nestlingCode <-
      paste(nestID, "nestling", as.character(chicknumber))
    # check the nestdata to see if there is a bandID for this bird...
    bandIdKey <- paste("band", as.character(chicknumber), sep = ".")
    if(exists(bandIdKey, nestdata)){

      band <- as.character(nestdata[[bandIdKey]][rownumber])
    #^this is throwing out null for band.1, rownumber 229. I don't understand why

        if (!is.na(band)) {
          # this nestling has an associated TreeSwallow...build the TreeSwallow structure for it.
          #since it's a nestling you won't already have a TreeSwallow for this bird to go right ahead and make one

          
          newBird = TreeSwallow(bandID = band,
                                hatchnest = EnvPointer(id = nestID, hash = dataSingleton$nests))
          year <- YearsSeen(year=nestdata$Year[rownumber], 
                            hatchNest = EnvPointer(id = nestID, hash = dataSingleton$nests), 
                            sex= "U", 
                            age= "HY")
          newBird$addYearSeen(year)
          
          dataSingleton$insertBird(newBird)
          
          # 'band' is NA if we didn't build a treeSwallow...or is the treeSwallow ID if we did
          birdPtr = EnvPointer(id = band, hash = .self$birds)
          
          
          chick <-
            Nestling(
              fromNest = EnvPointer(id = nestID, hash = .self$nests),
              nestlingCode = nestlingCode,
              nestlingTRES=birdPtr)
          
          return(chick)
        } else {
          chick <-
            Nestling(
              fromNest = EnvPointer(id = nestID, hash = .self$nests),
              nestlingCode = nestlingCode
            )
          
          return(chick) 
        }
      }
    }

  
)


globalData <- GlobalBirdData()

getGlobalSingleton <- function() {
  globalData
}

#Use this class to make a method that returns tree swallow based on bandID

Nest <- setRefClass("Nest",
                    
                    fields = list(
                      year = "numeric",
                      siteID = "character",
                      #unique code for the nestID
                      
                      #we can't do the circular referencing like we'd like
                      #to so we'll put it in as a character and use the
                      #hash_table as an intermediary to find the band IDs
                      maleID = "EnvPointer",
                      # to TreeSwallow
                      femaleID = "EnvPointer", # TreeSwallow
                      nestlings = "useableList",
                      #Nestlings needs to be a list of Nestlings, which will
                      #get a TreeSwallow bandID reference if and only if
                      #they turn into an adult tree swallow in our data.
                      
                      #Dates are all of class integer because they're all julian dates
                      firstEggDate = "integer",
                      lastEggDate = "integer",
                      hatchDate = "integer",
                      fledgeDate = "integer",
                      
                      clutchSize = "integer",
                      hatchSize = "integer",
                      fledgeSize = "integer",
                      reasonforFailure = "character",
                      renestStatus = "character", 
                      
                      eggMass= "useableList"
                      
                    )
                    
)


Nest$methods (
  initialize = function (year, siteID,  nestlings = useableList(8), firstEggDate=NA_integer_, lastEggDate=NA_integer_,
                         hatchDate=NA_integer_, fledgeDate=NA_integer_, clutchSize=NA_integer_,
                         hatchSize=NA_integer_, fledgeSize=NA_integer_, reasonforFailure=NA_character_,
                         renestStatus=NA_character_ , eggMass = useableList(8)){
    .self$year <<- year
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
    .self$eggMass <<- eggMass
  },
  addMale = function (malePointer){
    
    maleID <<- malepointer
  },
  addFemale = function (femalePointer){
    femaleID <<- femalePointer
  }, 
  addNestling= function ( nestlingPointer){
    
    .self$nestlings$addElement(nestlingPointer)
  },
  #' addDatesandSuccessNestdata
  #'Adds important breeding timing events to the a Reference class object if available. 
  #'Also adds measures of breeding success (eg clutch size, fledge size)
  #' @param nest The Nest object that we would like to add these pieces of information to. 
  #' @param nestdata The nest data we are working from
  #' @param i The rownumber within the nest data
  #'
  #' @return
  #' @export
  #'
  #' @examples
  addDatesandSuccessNestdata =function (nest, nestdata, i){  
    #Here are all the important dates
    if (!is.na(nestdata$First.Egg.Date[i])){
      nest$firstEggDate <- nestdata$First.Egg.Date[i]
    }
    if(!is.na(nestdata$Incubation.Date[i])){
      nest$lastEggDate <- nestdata$Incubation.Date[i]
    }
    
    if(!is.na(nestdata$Hatch.Date[i])){
      nest$hatchDate <- nestdata$Hatch.Date[i]
    }
    if(!is.na(nestdata$Fledge.Fail.date[i])){
      nest$fledgeDate <- nestdata$Fledge.Fail.date[i]
    }
    
    #Breeding success measurements
    if(!is.na(nestdata$Clutch.Size[i])){
      nest$clutchSize <- as.integer(nestdata$Clutch.Size[i])
    }
    if(!is.na(nestdata$Hatch.Size[i])){
      nest$hatchSize <- as.integer(nestdata$Hatch.Size[i])
    }
    if(!is.na(nestdata$Fledge.Size[i])){
      nest$fledgeSize <- as.integer(nestdata$Fledge.Size[i])
    }
    if(!is.na(nestdata$Why.fail.[i])){
      nest$reasonforFailure <- as.character(nestdata$Why.fail.[i])
    }
    if(!is.na(nestdata$renest.status[i])){
      nest$renestStatus <- as.character(nestdata$renest.status[i])
    }
  }, 
  addEggMass = function ( mass){
    if(!is.na(mass)){
        .self$eggMass$addElement (mass)
        }
}
  
) # end Nest$methods



YearsSeen <- setRefClass("YearsSeen", 
                         contains= "TreeSwallow", 
                         fields= list(
                           year= "integer", 
                           age= "character",
                           sex = "character", 
                           returnStatus = "character", 
                           hatchNest = "EnvPointer", 
                           nest = "useableList", 
                           observations= "useableList"
                         )
)

YearsSeen$methods (
  initialize= function (year=NA_integer_, 
                        age=NA_character_, 
                        sex= NA_character_,
                        returnstatus=NA_character_, 
                        hatchNest=EnvPointer(NA_character_, globalData$nests),
                        nest= useableList(2), #need to put this in a list because the bird might have been involved in multiple nests in a year!
                        observations = useableList(4)
  ){
    .self$year <<- year
    .self$age <<- age
    .self$sex <<- sex
    .self$returnStatus <<- returnStatus
    .self$hatchNest <<- hatchNest
    .self$nest <<- nest
    .self$observations <<- observations
    
  }, 
  addNest = function (nest){
    .self$nest$addElement(nest)
    
  }, 
  addObservation = function (obs){
    .self$observations$addElement(obs)
    
  }
)

TreeSwallow <- setRefClass("TreeSwallow",
                           fields = list(
                             bandID = "character",
                             sex = "character",
                             hatchnest = "EnvPointer", # to Nest
                             # a pointer to the nest record from the nest where I hatched
                             nestList = "useableList", #(nestData, nestData)
                             
                             observations = "useableList" , 
                             yearsSeen= "useableList"
                             )
                           #all of the things I know about an individual bird based on year
)

TreeSwallow$methods(
  initialize = function(bandID=NA_character_,
                        sex=NA_character_, 
                        hatchnest = EnvPointer(),
                        nestList=useableList(4),
                        observations = useableList (4), 
                        yearSeen =useableList (4)
                        ) {
    
    .self$bandID <<- bandID
    .self$sex <<-sex
    .self$hatchnest <<- hatchnest
    .self$nestList <<- nestList
    .self$observations <<- observations
    .self$yearsSeen <<- yearSeen
  },
  #need a function to add a new nest to the TreeSwallow record
  addNest = function(nest) {
    .self$nestList$addElement(nest )
  },
  #Also need a function to add a new observation of the TreeSwallow
  addObservation = function(obs) {
    .self$observations$addElement(obs) 
  }, 
  addYearSeen = function (yearSeen) {
    .self$yearsSeen$addElement(yearSeen)
  }
)


Nestling <- setRefClass("Nestling",
                        fields = list (
                          measurements = "useableList",
                          nestlingCode= "character",
                          nestlingTRES = "EnvPointer", # to TreeSwallow
                          # can point directly to nest - given the decl order
                          #  or we could use a Pointer - to be consistent with all
                          #  the rest of the classes
                          fromNest = "EnvPointer", 
                          growthRateMass = "numeric"
                        ),
                        methods = list(
                          test = function(nestlingCode, nestlingTRES=EnvPointer(NA_character_, globalData$birds),
                                          fromNest,  nestID, measurements=useablelist()){
                            .self$fromNest <<- fromNest
                            .self$nestlingCode <<- nestlingCode
                            .self$measurements <<- useableList(6)
                          }
                        )
)

Nestling$methods(
  initialize = function (nestlingCode=NA_character_,
                         nestlingTRES=EnvPointer(NA_character_, globalData$birds),
                         fromNest, 
                         growthRateMass = NA_real_, 
                         measurements= useableList(6)) {
    .self$fromNest <<- fromNest
    .self$nestlingCode <<- nestlingCode
    .self$nestlingTRES <<- nestlingTRES
    .self$measurements <<- measurements
    .self$growthRateMass <<- growthRateMass
  }, 
  addObservation = function( nestlingObs){
    .self$measurements$addElement(nestlingObs)
  }, 
  calcGrowthRate =function(){
    
    if(length(.self$measurements) < 2){
      #If there aren't two measurements then growth rate is NA because we haven't got a baseline
      return(NA)
    } else {
      day4 <- NestlingMeasurements(age=0)
      day12 <- NestlingMeasurements (age=20)
      for (obs in .self$measurements) {
        
        if(!is.na(obs$mass)){
          if(abs(obs$age - 4) <= abs (day4$age - 4) ){
            #when the observation's age closer to 4 days, then we want to take this observation
            if(abs(obs$age - 4) <abs (day4$age - 4) ) {
              day4 <- obs
            } else {
              #when the observations's age is equally far away from day 4, but
              #the age is larger, then we also want to take this observation
              #preferentially
              if ( obs$age > day4$age){
                day4 <- obs
              }
            }
          }
          if (abs(obs$age-12) <= abs(day12$age-12)){
            if(abs(obs$age-12)<abs(day12$age-12)){
              day12 <- obs
            } else {
              #IF they're exually far away from 12 days old, then we want to use
              #the younger chick because it will be  within the linear
              #period of growth
              if(obs$age < day12$age){
                day12 <- obs
                
              }
            }
          } 
        }
      }
      Massgrowth <- (day12$mass- day4$mass)/(day12$age - day4$age)
      message ( "growthrate calculated from",day4$age, "to",  day12$age, sep=" ")
    }
    return(Massgrowth)
  }, 
  addGrowthRateMass =function(growthRate ){
    .self$growthRateMass <- growthRate
  }
)


##########

Observation <- setRefClass("Observation",
                           fields = list(
                             date = "character",
                             #Dates are all characters now so I'll input them into the file structure as a
                             #character--that seems to get distorted less anyway
                             type = "character", 
                             bird = "TreeSwallow"
                           )
)
Observation$methods(
  initialize = function(date, bird, type = NA_character_) {
    init(date,  type, bird)
  },
  initBase = function(date, type, bird) {
    .self$date <<- date
    .self$type <<- type
    .self$bird <<- bird
  },
  equal = function(that) {
    return (date == that$date &
              type == that$type &
              identical(bird, that$bird))
  }
  
)



BodyMeasurements <- setRefClass("BodyMeasurements",
                                contains="Observation",
                                fields = list (
                                  wingChord = "numeric",
                                  ninthPrimary = "numeric",
                                  mass = "numeric",
                                  tarsus = "numeric"
                                )
)
BodyMeasurements$methods(
  initialize = function (date, bird, wingChord = NA_real_, ninthPrimary = NA_real_,
                         mass = NA_real_, tarsus = NA_real_) {
    initBase(date,  "bodymeasurement", bird)
    .self$wingChord <<- wingChord
    .self$ninthPrimary <<- ninthPrimary
    .self$mass <<- mass
    .self$tarsus <<- tarsus
  }
)




MalariaStatus <- setRefClass("MalariaStatus",
                             contains = "Observation",
                             fields = list(
                               status = "integer"
                             ))
MalariaStatus$methods (
  initialize=function(date, bird, status){
    initBase(date,  "MalariaStatus", bird)
    .self$status <<- status
  }
)
#NestlingMeasurements point back to nestlings, not attach directly
NestlingMeasurements <- setRefClass( "NestlingMeasurements", 
                                     fields = list (
                                       age = "numeric",
                                       #nestling = "Nestling",
                                       ninthPrimary = "numeric",
                                       mass = "numeric",
                                       tarsus = "numeric"
                                     )
)
NestlingMeasurements$methods (
  initialize = function (age=NA_real_, 
                         #nestling, 
                         ninthPrimary= NA_real_, 
                         mass=NA_real_, 
                         tarsus=NA_real_  
  ){
    .self$age <<- age
    #.self$nestling <<- nestling
    .self$ninthPrimary <<- ninthPrimary
    .self$mass <<- mass
    .self$tarsus <<- tarsus
  }
)

