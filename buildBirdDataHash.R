#function to build the global hash from the nest data (ie track birds as they
#show up in the nest data, instead of the band data)

#' buildBirdDataHash
#' This is a function that builds a bird data hash of bandIDs found in the
#' nestling data, sorted by year of occurance. Each year is also tagged based on
#' the sex of the bird during that year (F, M, or N (for nestling))
#'
#' @param nestdata This is the current year's nest data that we will be adding into the hash key
#' @param year The year of the nest that the bird was found in
#' @param hash The name of the hash we are inputting to be filled in by our nest data. This allows you to link functions to add many years worth of nest data
#'
#' @return hash The name of the output hash. 
#' @export
#'
#' @examples
buildBirdDataHash <- function(nestdata, year, hash) {
  
  # we are looking for band IDs in the nest data columns 'FemaleID', 'MaleID',
  #   and 'band.1' through 'band.10'.
  #   We check that the column exists before doing anything - so there it does not
  #   matter whether the nestling columns exist or not.
  # sex keys are 'F', 'M', and 'N' (for nestling whose sex we don't know yet)
  attributes = list(c("F", "FemaleID"),
                    c("M", "MaleID"))
  for (i in 1:10) {
    attributes <- append(attributes, list(c("N", paste("band", i, sep = "."))))
  }
 

  for (attr in attributes) {
    sex = attr[1]
    birdIdKey = attr[2]
    
    if (! birdIdKey %in% colnames(nestdata)) {
      # column doesn't exist (e.g., no such nestling column...skip)
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
