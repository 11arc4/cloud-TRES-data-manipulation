library(assertthat)


#Recruit, Return, Nestling or New Bird? 
band<-read.csv("~/Masters Thesis Project/Tree Swallow Data/TRES data/Data Sets I really need to ADD to my current dataset/TRESBAND_75-01.csv" ,as.is=TRUE, na.strings = c("NA", ""))

#We have to fix the dates on the band data so that it's useful (IE we know what the year is and can pull out a 2 digit year to compare with the year in the nest data)


for (i in 1:length(band$Date)){
  date <- band$Date[i]
  if(!is.na(date)) {
    date = as.integer(as.character(date))
    if (is.na(date)) {
      print(sprintf("entry %d is NA", i))
      next
    }
    if (date > 100) {
      date = date %/% 10000
    }
    if (date < 2000) {
      band$Year[i] <- date + 1900 # turn 78 into 1978
    } else {
      band$Year[i] <- date
    }
  }
}
#now band data is all sorted by year
band <- band[order(band$Year), ]

#First we are going to need to make a hash table of all the birds, with a list inside for all the times they show up

hash_allbirds<-new.env(hash=TRUE, parent=emptyenv(), size=(length(band$Band.Number)))

for (i in 1:length(band$Band.Number)) {
  bandID <- as.character(band$Band.Number[i])
  if (!is.na(bandID)) {
    if (exists(bandID, hash_allbirds)) {
      assign(as.character(bandID),
             append(x = get(bandID, hash_allbirds), values = i),
             hash_allbirds)
    } else {
      assign(as.character(bandID), i, hash_allbirds)
    } #This if else makes sure that if a bird shows up multiple years, where to look it up will show up multiple years as well.
  }
}


#ls(hash_allbirds, all.names = TRUE)
#Yay! Hash successful

# I have this lovely key with all the birds entered by band number, with look ups to the different years they showed up in
#Now I'd like to take a year of data, and check to see whether one of those adults shows up in the hash table
#It should show up--if not then we have an issue with the data and I need to know (PRINT YOURSELF A WARNING)
#When the bird band matches with the hash table then I need to loop through the list to check to be sure that I'm matching with the right year
#Then I need to use and if to see if there was a previous year in the list.
#If yes, then the bird is either a recruit or a return, depending on whether the previousl year was a nestling
#If no, then the bird is a new bird because I'm running through only the adults!




dataDirectoryBase <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics"

#Here make sure that you are importing the right data! You will want the data saved 
# in Malaria and Adult Metrics added so that you are adding to this file properly
inputNestDataDir = paste(dataDirectoryBase, "Malaria and Adult metrics added",
                         sep = "/")
nestDataFileList <- list.files(inputNestDataDir)

# need to process the nest data files in order of year - so we can extract the
#   the appearances of each bird, in order.

 
fileDataList=list()
for (fname in nestDataFileList) {
  if (0 == length(grep("^[^0-9]+([0-9]+)[^0-9]*\\.csv$", fname))) {
    print(sprintf("'%s' does not seem to be an input data file - skipping", fname))
    next
  }
  
  
  fileDataList <- rbind(fileDataList, 
                        c(gsub("^[^0-9]+([0-9]+)[^0-9]*\\.csv$", "\\1", fname, perl=TRUE),
                          fname))
}
colnames(fileDataList) <- c("year", "file")

# now sort in order of year...
fileDataList <- fileDataList[order(as.integer(fileDataList[,1])), ]

#Where do you want the good finished data to go?
destinationDir <- paste(dataDirectoryBase, "Recruit and Return Added", sep = "/")
setwd(destinationDir)

attributes = list(c("F", "FemaleID"),
                  c("M", "MaleID"))

#now lets loop through the files adding a recruitment and a return status for the females
for(a in 1:length(nestDataFileList)){
  fname<-fileDataList[[a, 2]]
  year<-fileDataList[[a,1]]
  nestdata<-read.csv(paste(inputNestDataDir, fname, sep="/"), 
                     as.is = TRUE, na.strings = c("NA", ""))

  
 #make it so it will run through bot sexes 
  for (d in 1:length(attributes)) {
    sex = attributes[[d]][1]
    birdIdKey = attributes[[d]][2]
  
    rtnStatusKey = paste(sex, "Return.Status", sep = ".")
    if (! rtnStatusKey %in% colnames(nestdata)) {
      nestdata[[rtnStatusKey]] <- rep(NA, nrow(nestdata))
    }
    b=0
    
    
    for (birdID in as.character(nestdata[[birdIdKey]])) {
      b = b + 1
      if(!is.na(birdID)){
        if(!exists(birdID, hash_allbirds)){
          print(paste("Warning:", sex, birdID, "from",  year, "not found in band record" ))
          next
        }
        if (is.na(nestdata$Year[b])) {
          print(sprintf("Error: 'Year' for entry %d of %s data is not the right format (SKIPPING)",
                        b, fname ))
          next
        }
        bandIdxList <- get(birdID, hash_allbirds) #list of all the places this band ID shows up
        returnIdx = 0
        for(c in bandIdxList){
          returnIdx = returnIdx + 1
          #For the new bird if, I've included a statement that shows that if the year that it showed up in 
          #first is less than the year that is shown in the banding data, then we mark it as a new bird 
          #because it looks like some of the birds in 1975 didn't get added to the banding data
          if (is.na(band$Year[c])) {
            print(sprintf("Error: year for entry %d in band data is NA (SKIPPING)", c))
            next
          }
          
          if (returnIdx == 1) {
            # this is the first appearance of 'birdId' in the band data.
            #   we expect that the first time we see the bird is when we band it in a nest (so the
            #   first appearance here corresponds to that nesting).
            #   However, if there is missing data, then we might find the bird in band data such that
            #   the band year is less than the nest year for the first entry.)
            
            if(band$Age[bandIdxList[returnIdx]]=="L") {
              next
            } else if (band$Year[c]>=nestdata$Year[b]){
              nestdata[[rtnStatusKey]][b] <- "New"
            } else {
              # this is the bird appearance from some previous year ..
              if (length(bandIdxList) == 1) {
                print(sprintf("birdID %s nesting in %d seen previously in %d - not in band data for this year", birdID,
                              nestdata$Year[b], band$Year[c]))
              }
              #print(sprintf("birdID %s came back in year %d seen previously in year %d", birdID,
              #              nestdata$Year[b], band$Year[c]))
              next
            }
            
          } else {
            # not the first entry for this bird in the band data (i.e., we saw this one in some prior year)
            if (band$Year[c] == nestdata$Year[b] & 
                band$Age[bandIdxList[returnIdx-1]]=="L") {
              yearsSinceLastSeen <- band$Year[bandIdxList[returnIdx-1]] - band$Year[c]
              if (1 == yearsSinceLastSeen) {
                nestdata[[rtnStatusKey]][b] <- "Recruit"
              } else {
                nestdata[[rtnStatusKey]][b] <- "Slow Recruit"
              }
            } else { 
              #currently recruit if your previous sighting was as a nestling (ie had age "L"), regardless of how many years ago that was.
              if (band$Year[c] == nestdata$Year[b]) {            
                nestdata[[rtnStatusKey]][b] <- "Return"
              }#close return if
            }#close else
            
          }#close recruit/return else
        }    
      } #close for
      
    } #closer if
    
  } #close for
  filename<-paste("Nest update w recruit ", nestdata$Year[1], ".csv"  ,sep="")
  write.csv(x=nestdata, file=filename, na="", row.names=FALSE)  
} #close for

