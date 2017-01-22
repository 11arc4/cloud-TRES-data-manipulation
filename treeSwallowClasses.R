library(assertthat)

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
                                nests = "environment", # nestID -> Nest
                                nestsByYear = "environment" # year -> vector of Nest
                              )
)
GlobalBirdData$methods(
  initialize = function() {
    birds <<- new.env()
    nests <<- new.env()
    nestsByYear <<- new.env()
  },
  findBird = function(bandID) {
    get0(bandId, envir=birds, ifnotfound = NA)
  },
  findNest = function(nestID) {
    get0(nestID, envir = birds, ifnotfound = NA)
  },
  insertBird = function(bird) {
    assert_that(is.na(findBird(bird$ID))) # this bird should not already be in the hash...
    assign(bird$ID, bird, .self$birds)
  },
  buildNestID = function(year, boxId) {
    paste(as.character(year), boxId, sep="-")
  }
)

globalData <- GlobalBirdData$new()

getGlobalSingleton <- function() {
  globalData
}

#Use this class to make a method that returns tree swallow based on bandID

Nest <- setRefClass("Nest",
                    
                    fields = list(
                      year = "numeric",
                      lineNumber = "numeric",
                      siteID = "character",
                      #unique code for the nestID
                      
                      #we can't do the circular referencing like we'd like
                      #to so we'll put it in as a character and use the
                      #hash_table as an intermediary to find the band IDs
                      maleID = "EnvPointer",
                      # to TreeSwallow
                      femaleID = "EnvPointer", # TreeSwallow
                      nestlings = "list",
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
                      renestStatus = "character"
                    )
                    
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
) # end Nest$methods




TreeSwallow <- setRefClass("TreeSwallow",
                           fields = list(
                             bandID = "character",
                             sex = "character",
                             hatchnest = "EnvPointer", # to Nest
                             # a pointer to the nest record from the nest where I hatched
                             nestList = "list", #(nestData, nestData)
                             
                             observations = "list")
                           #all of the things I know about an individual bird based on year
)

TreeSwallow$methods(
  initialize = function(bandID=NA_character_,
                        sex=NA_character_, nestList=list(),
                        hatchnest = EnvPointer()) {
    
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



Nestling <- setRefClass("Nestling",
                        fields = list (
                          #measurements = "BodyMeasurements",
                          nestlingCode= "character",
                          nestlingTRES = "EnvPointer", # to TreeSwallow
                          # can point directly to nest - given the decl order
                          #  or we could use a Pointer - to be consistent with all
                          #  the rest of the classes
                          fromNest = "EnvPointer",
                          nestID="character" # don't need this, as we can get it from the nest
                        ),
                        methods = list(
                          test = function(nestlingCode, nestlingTRES=EnvPointer("NA", bird_hash),
                                          fromNest,  nestID, measurements=ls()){
                            .self$fromNest <<- fromNest
                            .self$nestlingCode <<- nestlingCode
                            .self$nestID <<-nestID
                            #.self$measurements <<- measurements
                          }
                        )
)
Nestling$methods(
  initialize = function (nestlingCode, nestlingTRES=EnvPointer("NA", bird_hash),
                         fromNest,  nestID, measurements=ls()){
    .self$fromNest <<- fromNest
    .self$nestlingCode <<- nestlingCode
    .self$nestID <<-nestID
    #.self$measurements <<- measurements
  }
)

GlobalBirdData$methods(
  buildNestling = function(nestdata,
                           chicknumber,
                           rownumber) {
    nestID <-
      .self$buildNestID(year = nestdata$Year[1], boxID = nestdata$BoxID[rownumber])  #This is the unique
    nestlingCode <-
      paste(nestID, "nestling", as.character(chicknumber))
    # check the nestdata to see if there is a bandID for this bird...
    bandIdKey <- paste("band", as.character(chicknumber), sep = ".")
    band <- nestdata[[bandIdKey, rownumber]]
    if (!is.na(band)) {
      # this nestling has an associated TreeSwallow...build the TreeSwallow structure for it.
      newBird = TreeSwallow(bandID = band,
                            hatchnest = EnvPointer(id = nestID, hash = .self$nests))
      dataSingleton$insertBird(newBird)
    }
    # 'band' is NA if we didn't build a treeSwallow...or is the treeSwallow ID if we did
    birdPtr = EnvPointer(id = band, hash = .self$birds)
    
    
    chick <-
      Nestling(
        fromNest = EnvPointer(nestID = nestID, has = .self$nests),
        nestlingCode = nestlingCode,
        nestID = nestID,
        nestlingTRES=birdPtr)
    
    return(chick)
  },
  buildNest = function(nestdata, rownumber) {
    for (idx in 1:12) {
      nestlingKey = paste("nestling", as.character(idx), sep = ".")
      if (! is.na(nestdata[[nestlingkey, rownumber]])) {
        nestling = self.buildNestling(nestdata, idx, rownumber)
      }
    }
  }
)


##########

Observation <- setRefClass("Observation",
                           fields = list(
                             date = "Date",
                             type = "character",
                             bird = "character" #"TreeSwallow"
                           )
)
Observation$methods(
  initialize = function(date, bird, type = NA_character_) {
    init(date, bird, type)
  },
  initBase = function(date, bird, type) {
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
    initBase(date, bird, "body")
    .self$wingChord <<- wingChord
    .self$ninthPrimary <<- ninthPrimary
    .self$mass <<- mass
    .self$tarsus <<- tarsus
  }
)


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