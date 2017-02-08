#Fixing ages, fixing sex where appropriate and adding return status
exactAges <- c("HY", "SY", "3Y", "4Y", "5Y", "6Y", "7Y", "8Y", "9Y", "10Y", "11Y" )
estimateAges <- c("AHY", "ASY", "A3Y", "A4Y", "A5Y", "A6Y", "A7Y", "A8Y", "A9Y", "A10Y", "A11Y")


#AllBirds <- as.list(globalData$birds)

for (bird in as.list(globalData$birds)){
  message("updating Tree Swallow ", bird$bandID )
  #need to sort the yearsSeen based on year (that way we can just run though
  #that list and it will all be wonderfully sorted and I will have an easy way
  #to assign return and age)
  #NEED TO SORT THIS YEARS SEEN LIST BY YEAR! Probably just fine as is but should double check
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
        if (!is.na(year$age)){
          if( year$age=="2Y") {
            year$age <- "SY"
          }
          if(year$age == "A2Y"){
            year$age <- "ASY"
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
          if(agevec == "A"){
            year$age <- estimateAges[which(estimateAges==prev$age)+1]
          } else {
            year$age <- exactAges[which(exactAges==prev$age)+1]
          }
          
        }
        
      } else {
        year$returnStatus <- "Return"
        agevec <- substring (prev$age, first=1, last=1)
        if(agevec == "A"){
          year$age <- estimateAges[which(estimateAges==prev$age)+1]
        } else {
          year$age <- exactAges[which(exactAges==prev$age)+1]
        }
      }
    }
    #Setting this iteration to be the previous year's info for the next iteration
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
    } else {
      if (timesM > timesF){
        bird$sex <- "M"
      } else {
        #if the bird is seen equally as a male and as a female then I have no
        #idea what sex it is and we should put it in as an unknown bird. This
        #does not take into account whether the bird was seen with a known
        #partner but that's probably for the best in case the bird is actually
        #polygamous!
        bird$sex <- "U"
      }
    }
  }
  
}



