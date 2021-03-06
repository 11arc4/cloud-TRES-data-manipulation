#Fixing ages, fixing sex where appropriate and adding return status
library(beepr)
exactAges <- c("HY", "SY", "3Y", "4Y", "5Y", "6Y", "7Y", "8Y", "9Y", "10Y", "11Y", "12Y", "13Y" )
estimateAges <- c("AHY", "ASY", "A3Y", "A4Y", "A5Y", "A6Y", "A7Y", "A8Y", "A9Y", "A10Y", "A11Y", "A12Y")


#AllBirds <- as.list(globalData$birds)

for (bird in as.list(globalData$birds)){
  #message("updating Tree Swallow ", bird$bandID )
  #need to sort the yearsSeen based on year (that way we can just run though
  #that list and it will all be wonderfully sorted and I will have an easy way
  #to assign return and age)
  
  
  if(bird$yearsSeen$length>1){
    #message("Sorting years")
    l2 <- bird$yearsSeen$as.list()
    l3 <- l2[order(sapply(l2, function(v) { v$year} ))]
    bird$yearsSeen$replaceList(l3)
  }
  #create a vector of sexes 
  sexes <- rep(NA_character_, 20)
  #not sure if I need to reinput them all (may just do this automatically because RC is mutable)
  t=0
  for (year in bird$yearsSeen$as.list()){
    #Reorder all the nests so they they are in order based on date within the year
    if(year$nest$length>1){
      l2 <- year$nest$as.list()
      l3 <- l2[order(sapply(l2, function(v) { 
        n <- get(v$m_key, globalData$nests)
        n$firstEggDate} ))]
      year$nest$replaceList(l3)
    }
    
    t=t+1
  
    sexes[t] <- year$sex
    
    if (t==1){
      if (is.na (year$hatchNest$m_key)){
        year$returnStatus <- "New"
        #year age stays the same!
        
        #QUick fixes to check to see that everyone is consistent about how they write ages
        if (length(year$age)!=0 ){
          if(!is.na(year$age)){
          
            if(year$age == "A2Y" ){
              year$age <- "ASY"
            }
            if(year$age=="AS"){
              year$age ="ASY"
            }
            if(year$age=="ASYR"){
              year$age ="ASY"
            }
            if(year$age=="LSY"){
              year$age ="ASY"
            }
            if(year$age=="AAY"){
              year$age ="ASY"
            }
            if(year$age=="AFY"){
              year$age ="A5Y"
            }
            
            if(year$age=="A1Y" ){
              year$age="AHY"
            }
            if(year$age=="U"){
              year$age="AHY"
            }
            if(year$age=="TRES"){
              year$age = "AHY"
            }
            if( year$age==" "){
              year$age = "AHY"
            }
            if(year$age=="SYN" ){
              year$age= "SY"
            }
            if(year$age=="SY?" ){
              year$age= "SY"
            }
            if( year$age=="2Y"){
              year$age= "SY"
            }
            if(year$age=="TY"){
              year$age="3Y"
            }
            if(year$age=="ATY"){
              year$age=="A3Y"
            }
            ###If you are a adult male and it's the first year that we've seen
            ###you then you need to be entered as a AHY bird because there's no
            ###other way for us to id your age!
            if(year$sex=="M" & year$age != "AHY"){
              message("Bird ", bird$bandID, " identified as a male was aged as ", year$age, "in its first year seen--set to AHY")
              year$age <- "AHY"
            }
            #If you're an adult female and it's the first year we caught you all
            #you can be is ASY or SY because that's all we can tell! Will need
            #to set it to AHY because now we don't know if it's SY or ASY
            if(year$sex=="F" & year$age != "SY" &  year$age != "ASY" & year$age != "AHY"){
              message("Bird ", bird$bandID, " identified as a female was aged as ", year$age, " in its first year seen--set to AHY")
              year$age<- "AHY"
            }
          
            
            
          } else {
            message("Bird", bird$bandID, "from", year$year,  "has unknown age. Entered here as AHY", sep=" ")
            year$age<- "AHY"
          }
        } else {
          message("Bird", bird$bandID, "from", year$year,  "has unknown age. Entered here as AHY", sep= " ")
          year$age<- "AHY"
        }
        
      } else {
        year$returnStatus <- "Nestling"
        year$age <- "HY"
      }
    } else {
      if (t== 2){
        if (prev$returnStatus == "Nestling"){
          year$returnStatus <- "Recruit"
          year$age <- "SY"
        } else {
          year$returnStatus <- "Return"
          agevec <- substring (prev$age, first=1, last=1)
          if(agevec==" "){
            agevec <- substring (prev$age, first=2, last=2)
            AGE<- prev$age
            prev$age <- substring(AGE, first=2, last=5)
          }
          if(agevec == "A"){
            year$age <- estimateAges[which(estimateAges==prev$age)+(year$year-prev$year)]
          } else {
            year$age <- exactAges[which(exactAges==prev$age)+(year$year-prev$year)]
          }
        }
        
        
      } else {
        year$returnStatus <- "Return"
        agevec <- substring (prev$age, first=1, last=1)
        if(agevec == "A"){
          year$age <- estimateAges[which(estimateAges==prev$age)+(year$year-prev$year)]
        } else {
          year$age <- exactAges[which(exactAges==prev$age)+(year$year-prev$year)]
        }
      }
    }
    #Setting this iteration to be the previous year's info for the next iteration
    
    prev <- year
    
  }
  sexes <- sexes[1:t]
  if (nlevels (as.factor (sexes))==1) {
    #If there is only one level (ie it's Male or Female, but not both then we're all set and can assign it)
    
    
    
    ######THIS IS THE SEX ISSUE
    bird$sex <- sexes[1]
  } else {
    timesF <- length(which(sexes=="F"))
    timesM <- length(which(sexes=="M"))
    
    #if sex is not in agreement
    if (timesF > timesM){
      bird$sex <- "F"
      message("bird", bird$bandID,  "sex inconsistant-> changed to Female", sep=" ")
      
    } else {
      if (timesM > timesF){
        bird$sex <- "M"
        message("bird ", bird$bandID,  " sex inconsistant-> changed to Male", sep=" ")
        
      } else {
        #if the bird is seen equally as a male and as a female then I have no
        #idea what sex it is and we should put it in as an unknown bird. This
        #does not take into account whether the bird was seen with a known
        #partner but that's probably for the best in case the bird is actually
        #polygamous!
        bird$sex <- "U"
        message("bird ", bird$bandID,  " sex inconsistant-> changed to unknown", sep=" ")
      }
    }
  }
 if (length(bird$viewYears()) > nlevels(as.factor(bird$viewYears()))){
   message("Added too many versions of yearSeen in bird ", bird$bandID)
 }
}

beep(1)

