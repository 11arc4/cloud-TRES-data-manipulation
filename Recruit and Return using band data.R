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
#now band data is all sorted by year then by nest location within the year
band <- band[with(band, order(-Year, Nest)), ]

#First we are going to need to make a hash table of all the birds, with a list
#inside for all the times they show up

hash_allbirds <- new.env(hash=TRUE, parent=emptyenv(), size=(length(band$Band.Number)))

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

globalBirdDataHash <- new.env(hash = TRUE, size = 1000)

#ls(hash_allbirds, all.names = TRUE)
#Yay! Hash successful

#I have this lovely key with all the birds entered by band number, with look ups
#to the different years they showed up in Now I'd like to take a year of data,
#and check to see whether one of those adults shows up in the hash table It
#should show up--if not then we have an issue with the data and I need to know
#(PRINT YOURSELF A WARNING) When the bird band matches with the hash table then
#I need to loop through the list to check to be sure that I'm matching with the
#right year Then I need to use and if to see if there was a previous year in the
#list. If yes, then the bird is either a recruit or a return, depending on
#whether the previousl year was a nestling If no, then the bird is a new bird
#because I'm running through only the adults!


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
  matchStr = "^[^0-9]+([0-9]+)[^0-9]*\\.csv$"
  if (0 == length(grep(matchStr, fname))) {
    message("'", fname, "' does not seem to be an input data file - skipping")
    next
  }
  
  fileDataList <- rbind(fileDataList, c(gsub(matchStr, "\\1", fname, perl=TRUE),
                                        fname))
}
colnames(fileDataList) <- c("year", "file")

# now sort in order of year...
fileDataList <- fileDataList[order(as.integer(fileDataList[,1])), ]

#Where do you want the good finished data to go?
destinationDir <- paste(dataDirectoryBase, "Recruit and Return Added", sep = "/")
setwd(destinationDir)

#now lets loop through the files adding a recruitment and a return status for the females
for(a in 1:length(nestDataFileList)){
  fname <- fileDataList[[a, 2]]
  year <- as.integer(fileDataList[[a, 1]])
  nestdata <- read.csv(paste(inputNestDataDir, fname, sep="/"), 
                       as.is = TRUE, na.strings = c("NA", ""))
  
  nestdata <- AssignReturnStatus(nestdata, year, hash_allbirds, band,
                                 globalBirdDataHash)
  
  filename <- paste("Nest update w recruit ", nestdata$Year[1], ".csv"  ,sep="")
  write.csv(x=nestdata, file=filename, na="", row.names=FALSE)
  
  # and update the global bird data...
  globalBirdDataHash <- buildBirdDataHash(nestdata, year, globalBirdDataHash)
} #close for

