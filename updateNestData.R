

updateNestData <- function (nest_data, currentYear, bandDataTable) {
  # nesDataCsvFile:  file name (string)
  # currentYear: 2-digit integer
  # bandDataTable:  table of swallow band data
  # returns:  updated nest data..
  # 
  # the basic idea:
  #   - for each element of the 'bandDataTable', check if the band ID corresponds to either
  #     the male or female of one of the 'nest data' entries.
  #   - if it does: then update the corresponding male (or female) nest data.
  # Rules: 
  #  - if there is more than one band data entry for some male or female:  
  #      - take the earliest date (provided that the earliest date is after 1 May)
  #   
  # the algorithm used here:
  #   - put the male and female IDs from the nest data into a hash (so we can quickly look up, to see
  #     if the ID of the 'band data' entry matches a bird in this nest data.
  #   - iterate the 'band data':
  #         - assumes band data is ordered by date and that the date can be treated as a 6-digit
  #           integer "YYMMDD"
  #       - if band date is earlier than this year:  go to next band data entry
  #         if band data is later than this year:  we are done

  substList = list()
  hashList = list()

  for (sex in c("M", "F")) {
    l = list()
    i = 0
    for (d in list(
      c("Wing..mm.",           "Wing.Chord"),
      c("Tarsus..mm.",         "Tarsus"),
      c("Mass..g.",            "Mass"),
      c("Nineth.Primary..mm.", "Ninth.Primary")
    )) {
      i = i + 1
      l[[i]] <-  c(paste(sex, d[1], sep = "."), d[2])
    }
    substList[[sex]] = l
    hashList[[sex]] = new.env(hash=TRUE, parent=emptyenv(), size=length(nest_data$BoxID))
  }
  

  # could run through the frame just created, to see if it contains the
  #  column names we are going to create - and to create them, if they do not already
  #  exist.
  # Similarly, could look through, to check that there are no duplicate column
  #   names, that any columns that we expect to be fully specified, are (i.e., no
  #   NA values, no values out-of-range, etc)

  for (i in 1:length(nest_data$BoxID)){
    
    for (d in list(c("M", "MaleID"),
                   c("F", "FemaleID"))) {
      sex = d[1]
      key = d[2]
      
      id <- nest_data[[key]][i]
      if (! is.na(id)){
        k = as.character(id)
        if (exists(k, hashList[[sex]] )) {
          # append to list...
          append(get(k, hashList[[sex]]), i)
        } else {
          assign(k, i, hashList[[sex]])
        }
      }
    }
  }

  # check that the hash was constructed correctly...
  #ls(hashList[[sex]], all.names = TRUE)

  aprilEnd = (currentYear * 10000) + (4 * 100) + 30
  yearEnd = (currentYear * 10000) + (12 * 100) + 31
  yearBegin = (currentYear * 10000);

  numUpdates = 0
  i <- 0
  for(id in as.character(bandDataTable$Band.Number)){
    i <- i + 1
    # bandDataTable$Date is parsed as a factor rather than an integer...not sure why
    bandDate <- as.integer(bandDataTable$Date[i]) #..so this doesn't work
    #bandDate = as.integer(as.character(bandDataTable$Date[i]))
    #print(bandDate)
    if (is.na(bandDate) | bandDate > yearEnd) {
      # assumes that data is sorted by year - such that every element below this one is a later
      # date...if this one is after the end of the year, then the rest are too.
      break
    } else if (bandDate < yearBegin) {
      next
    }
    
    print(id)
    for (sex in c("M", "F")) {
      if (exists(id, hashList[[sex]])) {
        
        for (nestDataIdx in get(id, hashList[[sex]])) {
        
          mkey <- paste(sex, "Day.measured", sep = ".")
          nestDate <- nest_data[[mkey]][nestDataIdx]
          if (is.na(nestDate) | 
              (nestDate > bandDate &
               bandDate > aprilEnd)) {
            # note that our new measured date is this one...
            nest_data[[mkey]][nestDataIdx] <- bandDate
            # update the measurements..
            for (d in substList[[sex]]) {
              to <- d[1]
              from <- d[2]
              if (! is.na(bandDataTable[[from]][i])) {
                nest_data[[to]][nestDataIdx] <- bandDataTable[[from]][i]
              }
            }
            numUpdates = numUpdates + 1
          }
        }
      }
    }
  }

  if (0 != numUpdates){
    print(sprintf("Updated %d entries", numUpdates))
  }
  return(nest_data)
}
