library(assertthat)

BuildNestlingCallbacks <- setRefClass("BuildNestlingCallbacks",
                                      fields = list(
                                        "id" = "integer", # nestling ID
                                        "bandID" = "character",
                                        "columns" = "vector", # all the data columns which exist for this nestling
                                        "days" = "list" # (dayNumber, vector(col1, col2, ..), vector(measName1, name2, ..))
                                      ))
BuildNestlingCallbacks$methods(
  initialize = function(nestlingId, nestData) {
    id <<- nestlingId
    bandID <<- paste("band", nestlingId, sep = ".")
    columns <<- c(bandID)
    days <<- list()
    callbacks <- list(
      #(dataframe key, observationKey)
      c("mass", "mass"),
      c("tarsus", "tarsus"),
      c("wing", "ninthPrimary")
    )
    colnames <- colnames(nestData)
    for (day in 1:18) {
      d <- sapply(callbacks, function(c) {
        k1 <- paste(c[1], ".d", day, ".", nestlingId, sep="")
        k2 <- paste(c[1], day, nestlingId, sep=".")
        list(c(k1, c[2]),
             c(k2, c[2]))
      })
      keys <- sapply(d, function(x) { x[[1]] })
      #message("keys: ", keys)
      contained <- keys %in% colnames
      if (any(contained)) {
        columns <<- append(columns, keys[contained])
        measNames <- sapply(d, function(x) { x[[2]] })
        # could use column indices rather than names?  Might be faster
        #  indices <= match(keys[contained], colnames)
        days <<- append(days, list(list(day,
                                        keys[contained],
                                        measNames[contained])))
      }
    }
    columns <<- columns[columns %in% colnames]
    #message("columns: ", columns)
  },
  empty = function() {
      return(0 == length(columns))
  },
  band = function() {
    return(bandID)
  },
  dayList = function() {
    return(days)
  }
)


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
InputNestDatatoClassStructure <- function (nestdata, globalData) {

  nestlings <- c()
  for (i in 1:12) {
    n <- BuildNestlingCallbacks(i, nestdata)
    if (! n$empty()) {
      nestlings <- append(nestlings, c(n))
    }
  }

  year <- nestdata$Year[1]
  message("starting year ", year)
  for (i in 1: length(nestdata$Year)){
    #message("  begin nest ", i)
    nestID <- paste (as.character(year), nestdata$BoxID[i], sep="-")  #This is the unique
    nest <- Nest(year=year, siteID=as.character(nestdata$siteID[i]) )

    #Need to create (or append) sightings of the parents as TreeSwallows
    parentAttrib <- list(c("FemaleID", "F"), c("MaleID", "M"))
    for (attrib in parentAttrib) {
      birdID <- as.character(nestdata[[i, attrib[1]]])
      sex <- attrib[2]
      if (! is.na(birdID)) {
        #message("   start ", attrib[1], " ", birdID)

        # look for this bird is the globalData
        if (!exists(birdID, globalData$birds)) {
          # this is the first time we have seen this female...buid a TreeSwallow for it...
          bird <- TreeSwallow(bandID=birdID, sex=sex)
          globalData$insertBird(bird)

        } else {
          bird <- globalData$findBird(birdID)
          # we saw this one before...check that the sex is the same as the last
          # time - or maybe this was a nestling
          #   and we didn't know the sex
          if (length(bird$sex)==0 | is.na(bird$sex)){
            bird$sex <- sex
          } else {
            if ( bird$sex != sex){
              message("Thats odd.. ", ifelse(sex=="F", "female", "male")," ", birdID, " was ",
                      ifelse(sex=="F", "male", "female"), " last time.")
            }
          }
        }

        #bird$addNest(nest)
        age <- as.character(nestdata[[i, paste(sex, "Age", sep=".")]])
        yearentry <- YearsSeen(year= year, #set outside the function when we're going through the nestdata
                               age= age,
                               sex= sex,
                               returnstatus=NA_character_,
                               hatchNest=EnvPointer(NA_character_, globalData$nests)

        )
        yearentry$addNest (EnvPointer(nestID, globalData$nests))

        #If this isn't NA, then we have SOME measurements, and need to add them as an observation
        dayMeasured <-  nestdata[[i, paste(sex, "Day.measured", sep=".")]]
        if(!is.na(dayMeasured)){

          bodymetrics <- BodyMeasurements(date=as.character(dayMeasured)
                                          )
          for (metric in list(c("Wing..mm.", "wingChord"), c("Nineth.Primary..mm.", "ninthPrimary"),
                              c("Mass..g.", "mass"), c("Tarsus..mm.", "tarsus"))) {
            k = paste(sex, metric[1], sep=".")
            if(exists(k, nestdata)){
              m <- nestdata[[i, k]]
              if (!is.na(m)) {
                bodymetrics[[metric[2]]] <- as.numeric(m)
              }
            }
          }
          yearentry$addObservation(bodymetrics)

        }
        k <- paste(sex, "Malaria.Status", sep=".")
        if(exists(k, nestdata)){
          m <- nestdata[[i, k]]
          if(!is.na(m)){
            d <- paste(sex, "blooddate", sep=".")
            malaria <- MalariaStatus(date=as.character(nestdata[[i, d]]),
                                     status=m)
            #bird$addObservation(malaria)
            yearentry$addObservation(malaria)

          }
        }

        nest[[attrib[1]]] <- EnvPointer(birdID, globalData$birds)
        bird$addYearSeen(yearentry)

      } #close if (!is.na(birdID))
    }
    # call a method on the nest to populate the rest of the data
    #nest$populateMesaurements(nestdata, rownumber)

    #add all the important dates and breeding success measurements to the nest
    nest$addDatesandSuccessNestdata(nest, nestdata, i )


    # build the nestlings and nestling observations...
    for (n in nestlings) {
      # first, check if there is any data for this nestling (are all the columns NA?)
      keys <- n$columns
      nestlingData <- nestdata[i, keys]
      if (any(!is.na(nestlingData))) {
        # there is data here...
        #This creates the Nestling, and creates an associated TreeSwallow if applicable
        nestling <- globalData$buildNestling(
          nestdata = nestdata,
          chicknumber = n$id,
          rownumber = i,
          dataSingleton = globalData
        )
        tres <- nestdata[i, n$bandID]
        if (! is.na(tres)) {
          # FIXME:  I guess we add a pointer?
          # this nestling is banded...need to do something about it.
        }
        # now build any observations for the nestling..
        for (d in n$dayList()) {
          day <- d[[1]]
          # are there any values which aren't NA?
          meas <- nestdata[i, d[[2]]]
          if (any(!is.na(meas))) {
            # yep...at least one..go ahead and build.
            nestlingObs <- NestlingMeasurements(age = day)
            mapply(function(measName, data) {
              if (!is.na(data)) {
                nestlingObs[[measName]] <- data
              }
            }, d[[3]], meas)
            nestling$addObservation(nestlingObs)
          }
        }
      }
    }


    # HGC:  I added some counts to see how many things we were building vs. how many
    #  we were keeping...at least for the year 1988, we keep very little.
    #  this can probably be faster, if we filter better (even if the processing for the
    #  stuff we want to keep is more costly)
    #message("   nestlings:", keptNestlings, " of ", builtNestlings,
          #  " obs:", keptObs, " of ", builtObservations)
    globalData$insertNest(nestID= nestID, Nest=nest)
  }
}
