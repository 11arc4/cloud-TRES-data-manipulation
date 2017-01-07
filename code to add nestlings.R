#Use the updatenestlings function to update all the different files


banddata<-read.csv("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/Updated band data 1975-2001.csv", as.is=TRUE, na.strings = c("", "NA"))
sourcedir<-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/1 All available adult band IDs added"
resultdir<- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/2 all nestlings added"
  
files<-list.files(sourcedir)

for (filename in files){
  nestdata<-read.csv(paste(sourcedir, filename, sep="/"), as.is=TRUE, na.strings = c("", "NA") )
  message("Processing nest data", nestdata$Year[1])
  improveddata<-updatenestlings(nestdata = nestdata, banddata = banddata)
  newfilename<-paste("Nest Data", nestdata$Year[1], "updated with nestlings")
  write.csv(improveddata, file=paste(resultdir, newfilename, sep="/"), na="", row.names = FALSE)
  message("updated nest data", nestdata$Year[1])
}
