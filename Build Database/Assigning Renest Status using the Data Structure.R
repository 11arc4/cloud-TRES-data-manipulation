#Let's clean up the nests as best as we can!
done=0
for (nestname in ls(globalData$nests, all.names = T)){
  nest <- get(nestname, globalData$nests)
  #First we will want to make sure that failure at one level is being carried
  #through to the end (ie if you failed to hatch any nestlings, fledge is listed
  #as 0 not NA)
  if(!is.na(nest$hatchSize)){
    if (nest$hatchSize==0 & is.na(nest$fledgeSize)){
      nest$fledgeSize<- as.integer(0)
    }
  }
  #now we need to go through and check to see whether nests are renest are
  #actually renests or not If we don't know anyting about either of the birds
  #involved, best to assume it's a first nest
  if(is.na(nest$femaleID$m_key) & is.na(nest$maleID$m_key)){
    nest$renestStatus <- "First"
  } else {
    #We know somethink about the adults in the nest
    #If we know something about the female
    if(is.na(nest$maleID$m_key)){
      female <- get(nest$femaleID$m_key, globalData$birds)
      #need to pick out the right year in her years seen for this list
      for (year in female$yearsSeen$as.list()){
        if(year$year==nest$year){
          fyear<- year
          break
        } else {
          next
        }
      }
      
      if(fyear$nest$length==0){
        fyear$addNest(EnvPointer( nestname, globalData$nests))
      }
      #If she only had one nest this year then that's the first one
      if(fyear$nest$length==1){
        nest$renestStatus <- "First"
      } else {
        #If she has multiple nests then we need to find out which one of the
        #nests we are looking at
        i=0
        for (fnest in fyear$nest$as.list()){
          i=i+1
          if(fnest$m_key==nestname){
            if(i==1){
              nest$renestStatus<- "First"
            } else {
              nest$renestStatus <- "Female Renest/Male Unknown"
            }
            break
          }
        }
        
      }
    }
    #If we know the male but not the female
    if(is.na(nest$femaleID$m_key)){
      male <- get(nest$maleID$m_key, globalData$birds)
      #need to pick out the right year in her years seen for this list
      for (year in male$yearsSeen$as.list()){
        if(year$year==nest$year){
          myear<- year
          break
        } else {
          next
        }
      }
      
      if(myear$nest$length==0){
        myear$addNest(EnvPointer( nestname, globalData$nests))
      }
      #If he only has one nest then we know it's his first nest
      
      if(myear$nest$length==1){
        nest$renestStatus <- "First"
      } else {
        #If she has multiple nests then we need to find out which one of the
        #nests we are looking at
        i=0
        for (mnest in myear$nest$as.list()){
          i=i+1
          if(mnest$m_key==nestname){
            if(i==1){
              nest$renestStatus<- "First"
            } else {
              nest$renestStatus <- "Male Renest/Female Unknown"
            }
            break
          }
        }
        
      }
      
    }
    #If we know both
    if(!is.na(nest$femaleID$m_key) & !is.na(nest$maleID$m_key)){
      male <- get(nest$maleID$m_key, globalData$birds)
      female <- get(nest$femaleID$m_key, globalData$birds)
      #pick the right female year
      for (year in female$yearsSeen$as.list()){
        if(year$year==nest$year){
          fyear<- year
          break
        } else {
          next
        }
      }
      #pick the right male year
      for (year in male$yearsSeen$as.list()){
        if(year$year==nest$year){
          myear<- year
          break
        } else {
          next
        }
      }
      #Now we need to pick the right male nest and the right female nest appears
      #that sometimes males aren't connected to the proper nests! I imagine this
      #is due to issues with inputting floaters from the band data but can't be
      #sure. it seems quite uncommon
      if(myear$nest$length==0){
        myear$addNest(EnvPointer( nestname, globalData$nests))
      }
      if(fyear$nest$length==0){
        fyear$addNest(EnvPointer( nestname, globalData$nests))
      }
      
      
      i_m<-0
      for (mnest in myear$nest$as.list()){
        i_m=i_m+1
        if(mnest$m_key==nestname){
          break
        }
      }
      i_f<-0
      for (fnest in fyear$nest$as.list()){
        i_f=i_f+1
        if(fnest$m_key==nestname){
          break
        }
      }
      if(i_f==1 & i_m==1){
        nest$renestStatus=="First"
      }
      if(i_f==1 & i_m!=1){
        nest$renestStatus=="Male Renest"
      }
      if(i_f!=1 & i_m==1){
        nest$renestStatus=="Female Renest"
      }
      if(i_f!=1 & i_m!=1){
        nest$renestStatus=="All Renest"
      }
    }
    
    
  }
  done=done+1
}
