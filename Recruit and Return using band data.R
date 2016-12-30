#Recruit, Return, Nestling or New Bird? 



#First we are going to need to make a hash table of all the birds, with a list inside for all the times they show up
band<-read.csv("~/Masters Thesis Project/Tree Swallow Data/TRES data/Data Sets I really need to ADD to my current dataset/TRESBAND_75-01.csv" ,as.is=TRUE, na.strings = c("NA", ""))
hash_allbirds<-new.env(hash=TRUE, parent=emptyenv(), size=(length(band$Band.Number)))

for (i in 1:length( band$Band.Number)){
  bandID<-as.character(band$Band.Number[i])
  if(!is.na(bandID) ){
    if(exists(bandID, hash_allbirds)){
    assign(as.character(bandID), append(x=get(bandID, hash_allbirds), values=i), hash_allbirds)  
    } else
      {assign(as.character(bandID), i, hash_allbirds)
      } #This if else makes sure that if a bird shows up multiple years, where to look it up will show up multiple years as well. 
  }
}


ls(hash_allbirds, all.names = TRUE)
#Yay! Hash successful
# I have this lovely key with all the birds entered by band number, with look ups to the different years they showed up in
#Now I'd like to take a year of data, and check to see whether one of those adults shows up in the hash table
#It should show up--if not then we have an issue with the data and I need to know (PRINT YOURSELF A WARNING)
#When the bird band matches with the hash table then I need to loop through the list to check to be sure that I'm matching with the right year
#Then I need to use and if to see if there was a previous year in the list.
#If yes, then the bird is either a recruit or a return, depending on whether the previousl year was a nestling
#If no, then the bird is a new bird because I'm running through only the adults!

#Here make sure that you are importing the right data! You will want the data saved in Malaria and Adult Metrics added so that you are adding to this file properly
filestoaddto<-list.files("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics/Malaria and Adult metrics added")


#We have to fix the dates on the band data so that it's useful (IE we know what the year is and can pull out a 2 digit year to compare with the year in the nest data)


for( i in 1:length(band$Date)){
  date <- band$Date[i]
  if(!is.na(date)){
    date = as.integer(as.character(date))
    if (is.na(date)) {
      print(sprintf("entry %d is NA", i))
      next
    }
    if (date > 100) {
      date = date %/% 10000
    }
    if (date < 2000){
      band$Year[i] <- date + 1900 # turn 78 into 1978
    } else {
      band$Year[i] <- date
    }
  }
}

for(a in 1:length(filestoaddto)){
  nestdata<-read.csv(paste("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics/Malaria and Adult metrics added/", filestoaddto[a],  sep=""), as.is=TRUE, na.strings = c("NA", ""))
  nestdata$F.Return.Status <- rep(NA, nrow(nestdata))
  
  
  b=0
  
  for (birdID in as.character(nestdata$FemaleID)){
    b= b + 1
    if(!is.na(birdID)){
      if(!exists(birdID, hash_allbirds)){
        print(paste("Warning:", birdID, "not found in master band database: Check line", b, sep=" "))
      }
      if (exists(birdID, hash_allbirds)){
        bandIdx<-get(birdID, hash_allbirds) #list of all the places this band ID shows up
        returnIdx = 0
        for(c in bandIdx){
          returnIdx = returnIdx + 1
          #For the new bird if, I've included a statement that shows that if the year that it showed up in 
          #first is less than the year that is shown in the banding data, then we mark it as a new bird 
          #because it looks like some of the birds in 1975 didn't get added to the banding data
          if (returnIdx == 1 & (band$Year[c] >= nestdata$Year[b])) {
            nestdata$F.Return.Status[b] <- "New"
          } #close new if
          
          else {
            
            if (band$Year[c]==nestdata$Year[b] & band$Previous.Capture.[bandIdx[returnIdx-1]]=="N"){
              if(band$Year[bandIdx[returnIdx-1]]-band$Year[c]==1){
                nestdata$F.Return.Status[b]=="Recruit"
              } 
              else{
                nestdata$F.Return.Status[b]=="Slow Recruit"
              }
            } #close recruit if 
            #currently recruit if your previous sighting was as a nestling, regardless of how many years ago that was.
            else{
              if(band$Year[c]==nestdata$Year[b]){            
                nestdata$F.Return.Status[b]=="Return"
              }#close return if
            }#close else
            
          }#close recruit/return else
        }
      } #close for
      
    } #closer if
    
  } #close for
  filename<-paste("Nest update w recruit ", nestdata$Year[1], ".csv"  ,sep="")
  write.csv(x=nestdata, file=filename, na="")  
} #close for

