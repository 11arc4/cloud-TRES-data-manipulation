#Updates 2005 data to include the adult morphometrics. 



nestdatadir<-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/"
#We will need to do this for 2005-2016
#up to 2001 is dealt with in the banding data, and up through 2004 has been delt with in excel

filename<-"Nest Level Data 2005 QC wo adult metrics.csv"
nestdata<- read.csv(paste(nestdatadir, filename, sep="/"), as.is=TRUE, na.strings = c("", "NA"))


banddata2005<-read.csv("~/Masters Thesis Project/Tree Swallow Data/TRES data/2005/2005_BANDING_final_26July05.csv", as.is=TRUE, na.strings = c("", "NA"))
 
#creating blank hashes
F.hash_2005bands<-new.env(hash=TRUE, parent=emptyenv(), size=(length(nestdata$FemaleID)))
M.hash_2005bands<-new.env(hash=TRUE, parent=emptyenv(), size=(length(nestdata$MaleID)))

#populating hashes with the nest data

#Female hash populated
for(i in 1:length(nestdata$FemaleID)){
if(!is.na(nestdata$FemaleID[i])){  
  bandID<-as.character(nestdata$FemaleID[i])
  if(exists(bandID, F.hash_2005bands)){
    assign(bandID,
           append(x = get(bandID, F.hash_2005bands), values = i),
           F.hash_2005bands)
  } else{
    assign(bandID, i, F.hash_2005bands ) 
    
  }
}
}
#Male hash populated
for(i in 1:length(nestdata$MaleID)){
  if(!is.na(nestdata$MaleID[i])){  
    bandID<-as.character(nestdata$MaleID[i])
    if(exists(bandID, M.hash_2005bands)){
      assign(bandID,
             append(x = get(bandID, M.hash_2005bands), values = i),
             M.hash_2005bands)
    } else{
      assign(bandID, i, M.hash_2005bands ) 
      
    }
  }
}


#now we need to check to see if these things exist in the banding data
nestdata$F.Day.measured<-NA

for(i in 1:length(banddata2005$Band.Number)){
  band<-banddata2005$Band.Number[i]
  
  if(!is.na(band)){
    
    if(exists(band, F.hash_2005bands)){
      bandIDx<-get(band, F.hash_2005bands)
      d <- banddata2005$DATE[i]
      v <- as.Date(d, format="%m/%d/%Y")
      if(is.na(nestdata$F.Day.measured[bandIDx])){
        
        if(!is.na(d)){
          nestdata$F.Day.measured[bandIDx] <- as.character(v)
        }
        nestdata$F.Mass..g.[bandIDx]<-banddata2005$BIRD.mass[i] #set mass
        nestdata$F.Age[bandIDx]<-banddata2005$Age[i]            #set age
        nestdata$F.Tarsus..mm.[bandIDx] <- banddata2005$Left.Tarsus[i] #set tarsus
        nestdata$F.Wing..mm.[bandIDx]<-banddata2005$Left.Wing[i] #set wing chord (they didn't measure 9th primary)
        
        
      } else{
        if(as.Date(nestdata$F.Day.measured[bandIDx], format="%Y-%m-%d" )< as.Date("4/30/2005", format="%m/%d/%Y") ){
          nestdata$F.Day.measured[bandIDx] <- as.character(v)
          message("Adult measured multiple times in banding data!")
          nestdata$F.Mass..g.[bandIDx]<-banddata2005$BIRD.mass[i]
          #set mass 
          nestdata$F.Age[bandIDx]<-banddata2005$Age[i]            #set age
          nestdata$F.Tarsus..mm.[bandIDx]<-banddata2005$Left.Tarsus[i] #set tarsus
          nestdata$F.Wing..mm.[bandIDx]<-banddata2005$Left.Wing[i] #set wing chord (they didn't measure 9th primary)
          
        } else{
          if (as.Date(nestdata$F.Day.measured[bandIDx], format="%Y-%m-%d")< v){
            next
          } 
        }
        
      }
      
    }
  } #close if exists
  
  
  
} #Close for





#Now we need to do the same thing for the males
nestdata$M.Day.measured<-NA

for(i in 1:length(banddata2005$Band.Number)){
  band<-banddata2005$Band.Number[i]
  
  if(!is.na(band)){
    
    if(exists(band, M.hash_2005bands)){
      for (bandIDx in get(band, M.hash_2005bands)){
        d <- banddata2005$DATE[i]
        v <- as.Date(d, format="%m/%d/%Y")
        if(is.na(nestdata$M.Day.measured[bandIDx])){
          
          if(!is.na(d)){
            nestdata$M.Day.measured[bandIDx] <- as.character(v)
          }
          nestdata$M.Mass..g.[bandIDx]<-banddata2005$BIRD.mass[i] #set mass
          nestdata$M.Age[bandIDx]<-banddata2005$Age[i]            #set age
          nestdata$M.Tarsus..mm.[bandIDx] <- banddata2005$Left.Tarsus[i] #set tarsus
          nestdata$M.Wing..mm.[bandIDx]<-banddata2005$Left.Wing[i] #set wing chord (they didn't measure 9th primary)
          
          
        } else{
          if(as.Date(nestdata$M.Day.measured[bandIDx], format="%Y-%m-%d" )< as.Date("4/30/2005", format="%m/%d/%Y") ){
            nestdata$M.Day.measured[bandIDx] <- as.character(v)
            message("Adult measured multiple times in banding data!")
            nestdata$M.Mass..g.[bandIDx]<-banddata2005$BIRD.mass[i]
            #set mass 
            nestdata$M.Age[bandIDx]<-banddata2005$Age[i]            #set age
            nestdata$M.Tarsus..mm.[bandIDx]<-banddata2005$Left.Tarsus[i] #set tarsus
            nestdata$M.Wing..mm.[bandIDx]<-banddata2005$Left.Wing[i] #set wing chord (they didn't measure 9th primary)
            
          } else{
            if (as.Date(nestdata$M.Day.measured[bandIDx], format="%Y-%m-%d")< v){
              next
            } 
          }
        }
      }
      
    }
  } #close if exists
  
  
  
} #Close for

write.csv(x=nestdata, file="~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/FINAL QC DONE/Nest Level Data 2005.csv", na="", row.names = FALSE)
