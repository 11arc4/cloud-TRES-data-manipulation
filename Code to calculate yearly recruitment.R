#Calculate Yearly Recruitment and Return totals
#Question: How many of the total nestlings EVER return? 
#Answer=Recruitment for that year

#Question: How many of the adults are returning birds from previous years?
#Answer=Return for that year

#This won't work until all of the data is in--don't trust anything until all the
#data for all the nests is available!


outerdir <-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data"
inputdir <- "4 malaria status added"
inputfiles<- list.files(paste(outerdir, inputdir, sep="/"))
outputdir <- "Yearly data"

#First I need to build the hash of all the adult birds ever found 
hash_alladultsever<-new.env(hash=TRUE, parent=emptyenv()) 
for( inputfilename in inputfiles){
  nestdata<-read.csv(paste(outerdir, inputdir, inputfilename, sep="/"),
                     as.is=TRUE, na.strings = c("NA", ""))
  hash_alladultsever<-buildAllAdultHash(nestdata = nestdata, hash_alladultsever)
  year<-nestdata$Year[1]
  message("adding adults from", year, "to hash", sep=" ")
}
#ls(hash_alladultsever, all.names = TRUE)

#Then I need to loop through the different nestling years to see how many of
#those nestlings showed up again
year<- rep(NA,length(inputfiles))
recruitment<-rep(NA,length(inputfiles))
return<- rep(NA,length(inputfiles))

hash_adultsuptonow<- new.env(hash=TRUE, parent=emptyenv() )
f=0
for( inputfilename in inputfiles){
  
  f=f+1
  nestdata<-read.csv(paste(outerdir, inputdir, inputfilename, sep="/"),
                     as.is=TRUE, na.strings = c("NA", ""))
 
  
  year[f]<-nestdata$Year[1]
  
  message("Processing year", year[f], sep=" ")
  
  #Build the hash of the adults found this year.
  hash_yearAdults<- new.env(hash=TRUE, parent=emptyenv() )
  hash_yearAdults<-buildAllAdultHash(nestdata, hash_yearAdults)
  return[f]=0
  for(adult in ls(hash_yearAdults)){
    if(exists(adult, hash_adultsuptonow)){
      return[f]=return[f]+1
      
      
    }
  }
  #Now we can update the hash_alladults to include this new year
  hash_adultsuptonow<-buildAllAdultHash(nestdata, hash_adultsuptonow)
  
  #Build the nestling hash (automatically creates a new one each year because it's within the function)
  hash_nestling <- buildYearsNestlingsHash(nestdata = nestdata)
  #Calculate recruitment
  recruit=0
  for(birdID in ls(hash_nestling)){
    if(exists(birdID, hash_alladultsever)){
      recruit=recruit+1
      message("Found recruit from ", year[f])
      recruitment[f]<-recruit  
    }
    
  }
  
}
yearlyrecruitment<-data.frame(year, recruitment, return)
