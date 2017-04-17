
best<- function(state,outcomename){
  if(!state %in% outcome[,7]){
    stop("invalid state")
  }
  else if (!outcomename %in% c("heart attack","heart failure","pneumonia")){
    stop("invalid outcome")
    }
  else if(outcomename=="heart attack"){
      state_subset<-outcome[outcome[,7]==state, ]
      min_index<- which.min(as.numeric(state_subset[,11]))
      hospital<- state_subset[min_index,2]
      return(hospital)
    }
  else if(outcomename=="heart failure"){
      state_subset<-outcome[outcome[,7]==state, ]
      min_index<- which.min(as.numeric(state_subset[,17]))
      hospital<- state_subset[min_index,2]
      return(hospital)
    } 
  else if(outcomename=="pneumonia"){
      state_subset<-outcome[outcome[,7]==state, ]
      min_index<- which.min(as.numeric(state_subset[,23]))
      hospital<- state_subset[min_index,2]
      return(hospital)
    }
}
      
rankhospital<- function(state,outcomename,num){
  if(!state %in% outcome[,7]){
    stop("invalid state")
  }
  else if (!outcomename %in% c("heart attack","heart failure","pneumonia")){
    stop("invalid outcome")
  }

  else if(outcomename=="heart attack"){
    state_subset<-outcome[outcome[,7]==state & outcome[,11]!='Not Available', ]
    state_subset = state_subset[order(as.numeric(state_subset[,11]), state_subset[,2]), ]
    if(num=="best"){
      num=1
    }
    else if(num=="worst"){
      num=sum(!is.na(state_subset[,11]))
    }
    hospital <- state_subset[num, 2]
     return(hospital)
  }
  else if(outcomename=="heart failure"){
    
    state_subset<-outcome[outcome[,7]==state & outcome[,17]!='Not Available', ]
    state_subset = state_subset[order(as.numeric(state_subset[,17]), state_subset[,2]), ]
    #print(state_subset[1:4,c(2,7,17)])
    if(num=="best"){
      num=1
    }
    else if(num=="worst"){
      num=sum(!is.na(state_subset[,17]))
    }
    hospital <- state_subset[num, 2]
    return(hospital)
  }
  else if(outcomename=="pneumonia"){
    state_subset<-outcome[outcome[,7]==state & outcome[,23]!='Not Available', ]
    state_subset = state_subset[order(as.numeric(state_subset[,23]), state_subset[,2]), ]
    if(num=="best"){
      num=1
    }
    else if(num=="worst"){
      num=sum(!is.na(state_subset[,23]))
    }
    hospital <- state_subset[num, 2]
    return(hospital)
  }  
}


  
rankall<- function(outcomename,num){
   if (!outcomename %in% c("heart attack","heart failure","pneumonia")){
    stop("invalid outcome")
  }
  
  else if(outcomename=="heart attack"){
    data_subset<-outcome[outcome[,11]!='Not Available', ]
    if(num=="best"){
      num=1
    }
    else if(num=="worst"){
      num=sum(!is.na(outcome[,11]))
    }
    hospital <- outcome[num, 2]
    return(hospital)
  }
  else if(outcomename=="heart failure"){

    data_subset<-outcome[outcome[,17]!='Not Available', ]
    if(num=="best"){
      num=1
    }
    else if(num=="worst"){
      num=sum(!is.na(outcome[,17]))
    }
    hospital <- outcome[num, 2]
    return(hospital)
  }
  else if(outcomename=="pneumonia"){
    data_subset<-outcome[outcome[,23]!='Not Available', ]
    if(num=="best"){
      num=1
    }
    else if(num=="worst"){
      num=sum(!is.na(outcome[,23]))
    }
    hospital <- outcome[num, 2]
    return(hospital)
  }  
}


r<- rankall("heart attack",4)
as.character(subset(r,state == "HI")$hospital)
