

banddata<-read.csv("~/Masters Thesis Project/Tree Swallow Data/TRES data/Data Sets I really need to ADD to my current dataset/TRESBAND_75-01.csv", as.is=TRUE, na.strings = c("", "NA"))
#We have to fix the dates on the band data so that it's useful (IE we know what
#the year is and can pull out a 2 digit year to compare with the year in the
#nest data)
for (i in 1:length(banddata$Date)){
  date <- banddata$Date[i]
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
      banddata$Year[i] <- date + 1900 # turn 78 into 1978
    } else {
      banddata$Year[i] <- date
    }
  }
}
#now band data is all sorted by year
banddata <- banddata[order(banddata$Year), ]

#I need a nice useable Julian date for each of the band entries
banddata$NJDate <- as.Date(NA)
a=0
for (date in banddata$Date){
  a=a+1
  if(!is.na(date)){
    if(nchar(as.character(date))==6){
      banddata$NJDate[a] <- as.Date (as.character(date), format=("%y%m%d"))
    } else if(nchar(as.character(date))==2){
      banddata$NJDate[a] <- NA
    } else if(nchar(as.character(date))==8){
      banddata$NJDate[a] <- as.Date (as.character(date), format=("%Y%m%d")) 
    }
    
  }
}

uploaddir<-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/FINAL QC DONE"
listfiles<-list.files(uploaddir)

for (f in listfiles){
  filename<-paste(uploaddir, "/", f, sep="" )
  initialnestdata <- read.csv(filename, as.is=TRUE, na.strings = c("", "NA"))
  #Add the siteID ased on the function for renests--needed to check in the band data
  #Add the renest column based on the function I wrote. 
  message("Processing ", f)
  nestdata<-AssignRenestStatus(initialnestdata)
  Filledinnestdata<-fillinbandID(nestdata, banddata)
  resultdir<-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/1 All available adult band IDs added/"
  updatedfilename<-paste(resultdir, "Nest Data", Filledinnestdata$Year[1], "with all possible adult bands.csv")
  write.csv(Filledinnestdata, file=updatedfilename, row.names = FALSE, na=""  )
  message("updated", f, "with all possible band IDs", sep=" ")
}
