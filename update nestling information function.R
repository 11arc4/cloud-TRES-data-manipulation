
#We need to make a hash of all the nestlings available in the banding data

#' breakOnError
#' Quick function to catch and error if R is being silly and won't let you put in a breakpoint. 
#'
#' @param errStr 
#'
#' @return
#' @export
#'
#' @examples
breakOnError <- function(errStr) {
  message("caught an error ", errStr)
}

#' updatenestlings function
#' A function that takes the band data and creates a hash of all the nestlings
#' in the band data based on the nest they were from. Then all of the nests from
#' the nest data (that don't have nestling bands already entered) are cross
#' referenced with this banddata nestling hash to see if there are any nestlings
#' from the given year that match with that nest. If there are, the nestlings'
#' band numbers and body metrics are added to the dataframe.
#' @param banddata The master band data list
#' @param nestdata The nest data file from the year in question
#'
#' @return
#' @export
#'
#' @examples
updatenestlings <- function(banddata, nestdata) {
  hash_nestlings <-
    new.env(
      hash = TRUE,
      parent = emptyenv(),
      size = length(banddata$Band.Number)
    )
  
  banddata$Age <- as.character(banddata$Age)
  banddata$Place <- as.character(banddata$Place)
  
  a = 0
  for (nest in banddata$Place) {
    a = a + 1
    if (!is.na(nest)) {
      if (!is.na(banddata$Age[a])) {
        if (banddata$Age[a] == "L") {
          if (exists(nest, hash_nestlings)) {
            assign(nest, append(get(nest, hash_nestlings), a), hash_nestlings)
          } else {
            assign(nest, a, hash_nestlings)
          }
        }
      }
    }
  }
  
  #ls(hash_nestlings, all.names = TRUE)
  
  #Will need a list of all the things that we will want to update
  chickmetrics <- c("mass.d", "tarsus.d", "wing.d")
  bandmetrics <- c(10, 11, 8)
  
  
  #just got to quickly add some code because the columns didn't exist in some of the files
  if (!exists("band.1", nestdata)) {
    nestdata$band.1 <- c(rep(NA, length(nestdata$BoxID)))
  }
  if (!exists("band.2", nestdata)) {
    nestdata$band.2 <- c(rep(NA, length(nestdata$BoxID)))
  }
  if (!exists("band.3", nestdata)) {
    nestdata$band.3 <- c(rep(NA, length(nestdata$BoxID)))
  }
  if (!exists("band.4", nestdata)) {
    nestdata$band.4 <- c(rep(NA, length(nestdata$BoxID)))
  }
  if (!exists("band.5", nestdata)) {
    nestdata$band.5 <- c(rep(NA, length(nestdata$BoxID)))
  }
  if (!exists("band.6", nestdata)) {
    nestdata$band.6 <- c(rep(NA, length(nestdata$BoxID)))
  }
  if (!exists("band.7", nestdata)) {
    nestdata$band.7 <- c(rep(NA, length(nestdata$BoxID)))
  }
  if (!exists("band.8", nestdata)) {
    nestdata$band.8 <- c(rep(NA, length(nestdata$BoxID)))
  }
  if (!exists("band.9", nestdata)) {
    nestdata$band.9 <- c(rep(NA, length(nestdata$BoxID)))
  }
  if (!exists("band.10", nestdata)) {
    nestdata$band.10 <- c(rep(NA, length(nestdata$BoxID)))
  }
  #Now we need to loop through all of the nest boxes and check to see whether
  #there are nestling band IDs entered. This will be a little tricky because it's
  #possible that there will be no bandIDs entered in some places because the
  #nestling died early
  b = 0
  
  
  for (nestbox in nestdata$siteID) {
    b = b + 1
    #This if is checking to see if there are any entries already for band IDs for that nest box--if there are we don't want to mess with it
    
    if (!is.na(nestdata$band.1[b]) |
        !is.na(nestdata$band.2[b]) |
        !is.na(nestdata$band.3[b]) |
        !is.na(nestdata$band.4[b]) |
        !is.na(nestdata$band.5[b]) |
        !is.na(nestdata$band.6[b]) |
        !is.na(nestdata$band.7[b]) |
        !is.na(nestdata$band.8[b])) {
      next
      
    } else{
      if (exists(nestbox, hash_nestlings)) {
        chicklist <- c()
        c = 0
        
        #print(paste(get(nestbox, hash_nestlings)))
        
        for (nestlingIdx in get(nestbox, hash_nestlings)) {
          # if (nestlingIdx == 6160) {
          #   print("problematic")
          # }
          tryCatch({
            if (banddata$Year[nestlingIdx] == nestdata$Year[b]) {
              c <- c + 1
              chicklist[c] <- nestlingIdx
            }
          }, error = function(err) { 
            print(err) 
            message("hit error with nestling ", nestlingIdx, " at line ", b)
            breakOnError(err)
          }
          )
        } # for
        if (length(chicklist > 1)) {
          for (i in 1:length(chicklist)) {
            column <- paste("band.", i, sep = "")
            nestdata[b, column] <- banddata$Band.Number[chicklist[i]]
            
            if (!is.na(nestdata$Hatch.Date[b])) {
              if (!is.na(banddata$JDate[chicklist[i]])) {
                if (!is.na(nestdata$Fledge.Fail.date[b])) {
                  if ((nestdata$Hatch.Date[b] < banddata$JDate[chicklist[i]]) &
                      (banddata$JDate[chicklist[i]] < nestdata$Fledge.Fail.date[b])) {
                    # doing this because we need to check to make sure that the
                    # band IDs are being assigned to the right nest in the case of
                    # boxes that are reused--not elegent and may be missing some that I could be assigning
                    daysold <-
                      banddata$JDate[chicklist[i]] - nestdata$Hatch.Date[b]
                    
                    for (m in 1:length(chickmetrics)) {
                      nestcol <- paste(chickmetrics[m], daysold, ".", i, sep = "")
                      bandcol <- bandmetrics[m]
                      
                      if (!exists(nestcol, nestdata)) {
                        nestdata[nestcol] <- c(rep(NA, length(nestdata$BoxID)))
                        
                      }
                      
                      nestdata[b, nestcol] <-
                        banddata[chicklist[i], bandcol]
                    }
                  }
                }
              }
            }
          }
          #message("Updated nestling information for ",  nestbox, sep="")
          
        }
      }
    }
  }
  return(nestdata)
}

updateNestings_directory <- function(sourcedir, resultdir, bandata) {
  files <- list.files(sourcedir)
  
  for (filename in files) {
    message("Processing file ", filename)
    nestdata <-
      read.csv(
        paste(sourcedir, filename, sep = "/"),
        as.is = TRUE,
        na.strings = c("", "NA")
      )
    improveddata <-
      updatenestlings(nestdata = nestdata, banddata = banddata)
    message("Processing nest data ", nestdata$Year[1])
    newfilename <-paste("Nest Data", nestdata$Year[1], "updated with nestlings.csv") 
    write.csv(improveddata, file = paste(resultdir, newfilename, sep = "/"),
      na = "", row.names = FALSE)
    message("updated nest data ", nestdata$Year[1])
  }
}
