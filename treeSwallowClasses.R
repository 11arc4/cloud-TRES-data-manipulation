NestData <- setRefClass("NestData",
                        fieds = list(
                          date = "Date",
                          year = "numeric",
                          lineNumber = "numeric",
                          male = "TreeSwallow",
                          female = "TreeSwallow",
                          nestlings = "list")
                        )
NestData$methods(
  initialize = function(year, lineNumber, date = NA, male = NA,
                        female=NA, nestlings = list()) {
    #or maybe pass the 'nestData' file, line number, and year - and we figure it out
    .self$date <<- date
    year <<- year
    lineNumber <<- lineNumber
    male <<- male
    female <<- female
    nestlings <<- nestlings

    #male$addNesting(.self)
  },
  configureFromNestDataFile = function(nestDataFrame, birdHash, line) {
      lineNumber <<- line
      if (! is.na(nestData$femaleID[line]) ) {
        # find the female from its ID and enter it here
      }
      # look for any nestlings - and enter them here as well as create
      #  entries in the birdHash for them...
  },
  addNestling = function(n) {
    # should probably assert that the type of 'n' is "TreeSwallow"
    append(nestlings, n)
  }
)

Observation <- setRefClass("Observation",
                           fields = list(
                             date = "Date",
                             type = "char",
                             bird = "TreeSwallow"
                           ))

WingMeasure <- setRefClass("WingMeasure",
                           contains = c("Observation"),
                           fields = list (
                             wingChord = "numeric",
                             ninthPrimary = "numeric",
                             mass = "numeric",
                             tarsus = "numeric"
                           ))

MalariaCheck <- setRefClass("MalariaCheck",
                            contains = c("Observation"),
                            fields = list(
                              status = "char"
                            ))

TreeSwallow <- setRefClass("TreeSwallow",
                         fields = list(
                            bandId = "char",
                            sex = "char",
                            nestling = "NestData", # my nestling record, if any
                            nestList = "list", #(nestData, nestData)
                            observations = "list")
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
