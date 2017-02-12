
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


  buildMassMeasurement <- function(nestlingID, day, mass) {
    # doing nothing for now...need to build and return the measurement
    message("n", nestlingID, " d", day, " mass ", mass)
  }
  buildWingMeasurement <- function(nestlingID, day, wing) {
    # doing nothing for now...need to build and return the measurement
    message("w", nestlingID, " d", day, " wing ", wing)
  }
  buildTarsusMeasurement <- function(nestlingID, day, tarsus) {
    # doing nothing for now...need to build and return the measurement
    message("n", nestlingID, " d", day, " tarsus ", tarsus)
  }
  buildNestling <- function(nestlingID, bandID=NA_character_) {
    # doing nothing for now...need to build and return the measurement
  }


  keysAndCallbacks <- function (data, id) {
    bandID <- paste("band", id, sep = ".")
    callbacks <- list(
      c("mass", buildMassMeasurement),
      c("tarsus", buildTarsusMeasurement),
      c("wing", buildWingMeasurement)
    )
    days <- 1:18
    measurementKeys <- lapply(days, function(d) {
      l <- sapply(callbacks, function(cb) {
        key1 <- paste(cb[1], ".d", d, ".", id, sep = "")
        key2 <- paste(cb[1], d, id, sep = ".")
        f <- function(value) {
          cb[2](id, d, value)
        }
        list(c(key1, f), c(key2, f))
      })

      l
    })

    nestlingCallbacks <- append(measurementKeys, list(c(bandID, function(id) {
        # build nestling with this band ID..
        })), after=0 )
    keys <- unlist(sapply(nestlingCallbacks, function(x) {x[1]}))
    #message(keys)
    present <- keys %in% colnames(data)
    #message(present)
    if (any(present)) {
      callbacks <- unlist(sapply(nestlingCallbacks, function(x) {x[2]}))
      return(list(keys[present], callbacks[present]))
    }
    c()
  }

  nestlingCallbacks <- list()
  for (nestlingID in 1:10) {
    cb <- keysAndCallbacks(nestdata, nestlingID)
    if (0 != length(cb)) {
      nestlingCallbacks <- append(nestlingCallbacks, cb)
    }
  }

  year<- nestdata$Year[1]
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


    n_measures <- c("mass.d", "tarsus.d", "wing.d")

    N_obs <- c( "mass",
                "tarsus",
                "ninthPrimary",
                "age",
                "nestling"
    )
    #Need to make all the nestlings, and also need to add them to the data
    #  I think that it should be faster to have a vector of nestling keys
    #  c("band.1", "band.2", ... ) - and subset the dataframe to extract them all,
    #  filtering out the NAs...then go get all the observations for the non-NA nestlings
    #  the same way (i.e., via a subsetting action rather than a loop)
    #  When we get the list back from the subset, we ought to be able to 'apply' over
    #  the list - to build all the observations as well.
    builtNestlings <- 0
    keptNestlings <- 0
    builtObservations <- 0
    keptObs <- 0
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
        builtNestlings <- builtNestlings + 1
        for (day in 1:18){
          nestlingObs <- NestlingMeasurements(age=day)
          h=0
          for (m in 1: length(n_measures)){
            measurement <- paste( n_measures[m], day, ".", j, sep="")
            if(exists( measurement, nestdata)){
              meas <- nestdata[[i, measurement]]
              if (!is.na (meas)){
                h=h+1
                nestlingObs[[N_obs[m]]]<- as.numeric(meas)
                builtObservations <- builtObservations + 1
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
             length(nestling$measurements$as.list()) == 0 ) {
          #if the nestling wasn't banded and we know nothing about them
          next
        } else {
          #we know somehting about them and should put them into the nest and the nestlings database
          #add the Nestling to the Nest
          nest$addNestling(EnvPointer(nestling$nestlingCode, globalData$nestlings))
          #add the Nestling to the nestlings hash
          globalData$insertNestling(nestling)
          keptNestlings <- keptNestlings + 1
          keptObs <- keptObs + h
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
