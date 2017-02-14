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
    t=t+1
    if (length(year$sex) >0){
      sexes[length(sexes)+1] <- year$sex
    } else {
      sexes[t] <- "U"
    }
    if (t==1){
      if (is.na (year$hatchNest$m_key)){
        year$returnStatus <- "New"
        #year age stays the same!
        
        #QUick fixes to check to see that everyone is consistent about how they write ages
        if (length(year$age)!=0 ){
          if(!is.na(year$age)){
            
            if( year$age=="2Y") {
              year$age <- "SY"
            }
            if(year$age == "A2Y"){
              year$age <- "ASY"
            }
            if(year$age=="AFY"){
              year$age ="A5Y"
            }
            if(year$age=="ASYR"){
              year$age = "ASY"
            }
            if(year$age=="A1Y"){
              year$age="AHY"
            }
            if(year$age=="TRES"){
              year$age = "AHY"
            }
            if(year$age=="SYN"){
              year$age= "SY"
            }
            if(year$age=="SY?"){
              year$age="SY"
            }
            
          } else {
            message("Bird", bird$bandID, "from", year$year,  "has unknown age. Entered here as AHY")
            year$age<- "AHY"
          }
        } else {
          message("Bird", bird$bandID, "from", year$year,  "has unknown age. Entered here as AHY")
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
    if(is.na(year$age)){
     # message("Check year$age")
    }
    prev <- year
    
  }
  sexes <- na.omit(sexes)[[1]]
  if (nlevels (as.factor (sexes))==1) {
    #If there is only one level (ie it's Male or Female, but not both then we're all set and can assign it)
    bird$sex <- sexes[1]
  } else {
    timesF <- length(sexes[sexes=="F"])
    timesM <- length(sexes[sexes=="M"])
    
    #if sex is not in agreement
    if (timesF > timesM){
      bird$sex <- "F"
      message("bird", bird$bandID,  "sex inconsistant-> changed to Female", sep=" ")
      
    } else {
      if (timesM > timesF){
        bird$sex <- "M"
        message("bird", bird$bandID,  "sex inconsistant-> changed to Male", sep=" ")
        
      } else {
        #if the bird is seen equally as a male and as a female then I have no
        #idea what sex it is and we should put it in as an unknown bird. This
        #does not take into account whether the bird was seen with a known
        #partner but that's probably for the best in case the bird is actually
        #polygamous!
        bird$sex <- "U"
        message("bird", bird$bandID,  "sex inconsistant-> changed to unknown", sep=" ")
      }
    }
  }
 if (length(bird$viewYears()) > nlevels(as.factor(bird$viewYears()))){
   message("Added too many versions of yearSeen in bird ", bird$bandID)
 }
}

beep(1)

