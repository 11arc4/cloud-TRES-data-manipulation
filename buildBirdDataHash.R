#function to build the global hash from the nest data (ie track birds as they
#show up in the nest data, instead of the band data)

buildBirdDataHash <- function(nestdata, year, hash) {
  
  attributes = list(c("F", "FemaleID"),
                    c("M", "MaleID"))
  for (i in 1:10) {
    attributes <- append(attributes, list(c("N", paste("band", i, sep = "."))))
  }
 
  colnames(attributes) <- c("sex", "key")
  
  for (attr in attributes) {
    sex = attr[1]
    birdIdKey = attr[2]
    
    if (! birdIdKey %in% colnames(nestdata)) {
      next
    }
    b=0
    for (birdID in as.character(nestdata[[birdIdKey]])) {
      b = b + 1
      if (is.na(birdID)) {
        next
      }
      if (exists(birdID, hash)) {
        # add to end...
        current <- get(birdID, hash)
        # could check that this bird isn't already in the list for this year -
        #   right now, can (and does) happen when the bird has 2 nests in the same year -
        #   at present, we add it into this list once for each nest...
        assign(birdID, append(current, list(c(year, sex))), hash)
        # check consistency..
        start <- 0
        for (d in get(birdID, hash)) {
          stopifnot(start < d[[1]])  # check that years are sorted...
          start <= d[[1]]
          # could also check that the sex doesn't change (and/or that it only
          # changes if the first appearancd is as a nestling ("N") and the next
          # appearances are all either M or F...
        }
      } else {
        assign(birdID, list(c(year, sex)), hash)
      }
    } # for each bird in column
  }# for each attribute
  return(hash)
} 
