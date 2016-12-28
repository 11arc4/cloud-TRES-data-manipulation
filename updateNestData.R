

updateNestData <- function (nestDataCsvFile, currentYear, bandDataTable) {
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

  nest_data <- read.csv(nestDataCsvFile, as.is=TRUE, na.strings = c("NA", ""))

  # connstruct the hashes...
  hash_female = new.env(hash=TRUE, parent=emptyenv(), size=length(nest_data))
  hash_male = new.env(hash=TRUE, parent=emptyenv(), size=length(nest_data))

  for (i in 1:length(nest_data)){
    fId <- nest_data$FemaleID[i]
   if (! is.na(fId) ){
      assign(as.character(fId), i, hash_female )
   }
    mId <- nest_data$MaleID[i]
    if (! is.na(mId) ){
      assign(as.character(mId), i, hash_male )
    }
  }

  # check that the hash was constructed correctly...
  #ls(hash_female, all.names = TRUE)

  aprilEnd = (currentYear * 10000) + (4 * 100) + 30
  yearEnd = (currentYear * 10000) + (12 * 100) + 31
  yearBegin = (currentYear * 10000);

  didUpdate = FALSE
  i = 0
  for(id in as.character(bandDataTable$Band.Number)){
   i = i + 1
   # bandDataTable$Date is parsed as a factor rather than an integer...not sure why
   bandDate <- as.integer(bandDataTable$Date[i]) #..so this doesn't work
   #bandDate = as.integer(as.character(bandDataTable$Date[i]))
    #print(bandDate)
   if (is.na(bandDate) || bandDate > yearEnd) {
      # assumes that data is sorted by year - such that every element below this one is a later
      # date...if this one is after the end of the year, then the rest are too.
      break
    } else if (bandDate < yearBegin) {
      next
    }
    #print(id)
    if (exists(id, hash_female)){
      nestDataIdx <- get(id, hash_female)
    
      nestDate <- nest_data$F.Day.measured[nestDataIdx]
      if (is.na(nestDate) | 
          (nestDate > bandDate &
          bandDate > aprilEnd)) {
        # note that our new measured date is this one...
        nest_data$F.Day.measured[nestDataIdx] <- bandDate
        # update the measurements...body....
        if (! is.na(bandDataTable$Wing.Chord[i]) ) {
          nest_data$F.Wing..mm.[nestDataIdx] = bandDataTable$Wing.Chord[i]
        }
        if (! is.na(bandDataTable$Tarsus[i]) ) {
          nest_data$F.Tarsus..mm.[nestDataIdx] = bandDataTable$Tarsus[i]
        }         
        if (! is.na(bandDataTable$Mass[i]) ) {
          nest_data$F.Mass..g.[nestDataIdx] = bandDataTable$Mass[i]
        }
        if (! is.na(bandDataTable$Ninth.Primary[i]) ) {
          nest_data$F.Nineth.Primary..mm.[nestDataIdx] = bandDataTable$Ninth.Primary[i]
        }
        didUpdate = TRUE
      }
      if (exists(id, hash_male)){
        nestDataIdx <- get(id, hash_male)
      
        nestDate <- nest_data$F.Day.measured[nestDataIdx]
        if (is.na(nestDate) | 
            (nestDate > bandDate &
             bandDate > aprilEnd)) {
          # note that our new measured date is this one...
          nest_data$M.Day.measured[nestDataIdx] <- bandDate
          # update the measurements...body....
          if (! is.na(bandDataTable$Wing.Chord[i]) ) {
            nest_data$M.Wing..mm.[nestDataIdx] = bandDataTable$Wing.Chord[i]
          }
          if (! is.na(bandDataTable$Tarsus[i]) ) {
            nest_data$M.Tarsus..mm.[nestDataIdx] = bandDataTable$Tarsus[i]
          }         
          if (! is.na(bandDataTable$Mass[i]) ) {
            nest_data$M.Mass..g.[nestDataIdx] = bandDataTable$Mass[i]
          }
          if (! is.na(bandDataTable$Ninth.Primary[i]) ) {
            nest_data$M.Nineth.Primary..mm.[nestDataIdx] = bandDataTable$Ninth.Primary[i]
          }
        }
        didUpdate = TRUE
      }
    }
  }

  if (didUpdate){
    print("We updated something...")
  }
  rm(hash_male)
  rm(hash_female)
  return(nest_data)
}
