#Calculate Yearly Recruitment totals
#Question: How many of the total nestlings EVER return? 
#Answer=Recruitment for that year

#This won't work until all of the data is in--don't trust anything until all the
#data for all the nests is available!


outerdir <-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data"
inputdir <- "4 malaria status added"
inputfiles<- list.files(paste(outerdir, inputdir, sep="/"))
outputdir <- "Yearly data"
 
#First I need to build the hash of all the adult birds ever found 
hash_adults<-new.env(hash=TRUE, parent=emptyenv()) 
for( inputfilename in inputfiles){
  nestdata<-read.csv(paste(outerdir, inputdir, inputfilename, sep="/"),
                     as.is=TRUE, na.strings = c("NA", ""))
  hash_adults<-buildAllAdultHash(nestdata = nestdata)
  year<-nestdata$Year[1]
  message("adding adults from", year, "to hash", sep=" ")
}
#ls(hash_adults, all.names = TRUE)

#Then I need to loop through the different nestling years to see how many of
#those nestlings showed up again
year<- rep(NA,length(inputfiles))
recruitment<-rep(NA,length(inputfiles))

f=0
for( inputfilename in inputfiles){
  f=f+1
  nestdata<-read.csv(paste(outerdir, inputdir, inputfilename, sep="/"),
                     as.is=TRUE, na.strings = c("NA", ""))
  hash_nestling <- buildYearsNestlingsHash(nestdata = nestdata)
  year[f]<-nestdata$Year[1]
  message("Processing year", year[f], sep=" ")
  recruit=0
  for(birdID in ls(hash_nestling)){
    if(exists(birdID, hash_adults)){
      recruit=recruit+1
      message("Found recruit from ", year[f])
      recruitment[f]<-recruit  
    }
  }

}
yearlyrecruitment<-data.frame(year, recruitment)
