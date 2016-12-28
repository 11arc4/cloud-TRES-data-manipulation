
malaria<-read.csv("~/Masters Thesis Project/Tree Swallow Data/TRES data/Data Sets I really need to ADD to my current dataset/Individual malaria status.csv",as.is=TRUE, na.strings = c("", "NA"))
setwd("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics/Malaria and Adult metrics added")

#making the has/lookupkey
hash_malaria <- new.env(hash=TRUE, parent=emptyenv(), size=length(malaria$ID))
for (i in 1:length(malaria$ID)){
  bandID<- malaria$ID[i]
  if (! is.na(bandID) ){
    assign(as.character(bandID), i, hash_malaria )
  }
}

#making the list of data files we'd like to look at
nestDataFiles <- list.files("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics/mangled")


#looping through all of the files to make a better version of each of them
for (i in 1:length(nestDataFiles)) {
  
  nestdata <- read.csv(paste("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics/mangled/", nestDataFiles[i], sep=""), as.is=TRUE, na.strings = c("NA", ""))  
  nestdata$F.Malaria.Status <- rep(NA, nrow(nestdata))
  nestdata$F.blooddate <- rep(NA, nrow(nestdata))
  
  j = 0
  for(bandID in as.character(nestdata$FemaleID)){
    j = j + 1
    
    
    if (exists(bandID, hash_malaria)){
      malariaIdx <- get(bandID, hash_malaria)
      if(nestdata$Year[j]==malaria$Year[malariaIdx]) {
        nestdata$F.Malaria.Status[j]=malaria$Positive[malariaIdx]
        nestdata$F.blooddate[j]=paste (malaria$Exact.date[malariaIdx], malaria$Year[malariaIdx])
        
      }
    }
  }
  
  #now let's do the males
  nestdata$M.Malaria.Status<-rep(NA, nrow(nestdata))
  nestdata$M.blooddate<-rep(NA, nrow(nestdata))
  
  j = 0
  for(bandID in as.character(nestdata$MaleID)){
    j = j + 1
    if (exists(bandID, hash_malaria) ){
      malariaIdx <- get(bandID, hash_malaria)
      
      if(nestdata$Year[j]==malaria$Year[malariaIdx]){
        nestdata$M.Malaria.Status[j]=malaria$Positive[malariaIdx]
        nestdata$M.blooddate[j]=paste (malaria$Exact.date[malariaIdx],malaria$Year[malariaIdx])
      } #if
      
    } # if
  } # for
  
 write.csv(x=nestdata, file=paste("Nest", nestdata$Year [2], "with malaria and adult.csv", sep=" "), na="")
}
#write the csv files into a new location


  