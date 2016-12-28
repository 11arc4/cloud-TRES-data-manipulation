#Adding malarial status (1988-2012) based on band number
rm(list=ls())
setwd("~/Masters Thesis Project/Tree Swallow Data/TRES data/Data Sets I really need to ADD to my current dataset")
malaria<-read.csv("Individual malaria status.csv",as.is=TRUE, na.strings = c("", "NA"))



#first we have to make a hash so it's speedy. 
#I will make the hash using the bands of the birds checked for malaria because that's a much smaller dataset


hash_malaria <- new.env(hash=TRUE, parent=emptyenv(), size=length(malaria$ID))
for (i in 1:length(malaria$ID)){
  bandID<- malaria$ID[i]
  if (! is.na(bandID) ){
    assign(as.character(bandID), i, hash_malaria )
  }
}
#ls(hash_malaria, all.names = TRUE) 
#Checks to see if the hash was made properly
#YAY! Our hash is working just fine now!



#OK, now we want to take our file and run through the hash to see if any band dates match!

setwd("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics/mangled")
nestdata_1988<-read.csv("updated nest data 88.csv")



#Since males and females are in seperate columns in the nest data we will have to run thorugh this data twice, once for the males and once for the females

nestdata_1988$F.Malaria.Status<-rep(NA, nrow(nestdata_1988))
nestdata_1988$F.blooddate<-rep(NA, nrow(nestdata_1988))

j = 0
for(bandID in as.character(nestdata_1988$FemaleID)){
  j = j + 1


  if (exists(bandID, hash_malaria)){
    malariaIdx <- get(bandID, hash_malaria)
    if(nestdata_1988$Year[j]==malaria$Year[malariaIdx]{
    nestdata_1988$F.Malaria.Status[j]=malaria$Positive[malariaIdx]
    nestdata_1988$F.blooddate[j]=paste (malaria$Exact.date[malariaIdx],malaria$Year[malariaIdx])
    
}
  }
}

#now let's do the males
nestdata_1988$M.Malaria.Status<-rep(NA, nrow(nestdata_1988))
nestdata_1988$M.blooddate<-rep(NA, nrow(nestdata_1988))

j = 0
for(bandID in as.character(nestdata_1988$MaleID)){
  j = j + 1
  if (exists(bandID, hash_malaria) ){
    malariaIdx <- get(bandID, hash_malaria)
  
    if(nestdata_1988$Year[j]==malaria$Year[malariaIdx]){
      nestdata_1988$M.Malaria.Status[j]=malaria$Positive[malariaIdx]
     nestdata_1988$M.blooddate[j]=paste (malaria$Exact.date[malariaIdx],malaria$Year[malariaIdx])
    }
    
  }
}

