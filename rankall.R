#source('rankhospital.R')
library(dplyr)
rankall<-function(outcome,num="best"){

  data<-read.csv("Dataset/outcome-of-care-measures.csv",colClasses = "character")
  

  subset1<-data[,c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
  subset2<-data[,c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
  subset3<-data[,c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]

  colnames(subset1)<-c("Hospital","State","HeartAttack")
  colnames(subset2)<-c("Hospital","State","HeartFailure")
  colnames(subset3)<-c("Hospital","State","Pneumonia")
  

    
  subset1$HeartAttack<-as.numeric(subset1$HeartAttack)
  subset2$HeartFailure<-as.numeric(subset2$HeartFailure)
  subset3$Pneumonia<-as.numeric(subset3$Pneumonia)
  
  
  subset1<-na.omit(subset1)
  subset2<-na.omit(subset2)
  subset3<-na.omit(subset3)
  
  if (outcome=="heart attack"){ 
  
  subset1<-subset1[with(subset1,order(HeartAttack)),]
  subset1<-subset1[order(subset1$HeartAttack,subset1$Hospital),]
  
  
  sp<-split(subset1,subset1$State)
  
  st<-names(sp)

  y<-NULL
  for (i in st) {
     
    t<-sp[[i]]
    
    t$rank<-1:nrow(t)
    
    if (num=="best") {
      
    num<-min(t$rank)
    
    result<-filter(t,rank==num)

    
      r<-(paste("Hospital:",result[,c(1)],"State:",i,sep=" "))
      temp<-r
      y<-rbind(y,temp)
      
   
  }
  else if(num=="worst"){
  
  num1<-max(t$rank)
  
  result<-filter(t,rank==num1)
  
  
  r<-(paste("Hospital:",result[,c(1)],"State:",i,sep=" "))
  temp<-r
  y<-rbind(y,temp)
  
  
  }else{
    
    result<-filter(t,rank==num)
    
    
    r<-(paste("Hospital:",result[,c(1)],"State:",i,sep=" "))
    temp<-r
    y<-rbind(y,temp)
    
  }
  
}
return(y)
}
else if(outcome=="heart failure"){
  
  subset2<-subset2[with(subset2,order(HeartFailure)),]
  subset2<-subset2[order(subset2$HeartFailure,subset2$Hospital),]
  
  
  sp<-split(subset2,subset2$State)
  
  st<-names(sp)
  
  y<-NULL
  for (i in st) {
    
    t<-sp[[i]]
    
    t$rank<-1:nrow(t)
    
    #print(t)
    
    if (num=="best") {
      
      num<-min(t$rank)
      
      result<-filter(t,rank==num)
      
      
      r<-(paste("Hospital:",result[,c(1)],"State:",i,sep=" "))
      temp<-r
      y<-rbind(y,temp)
      
      
    }
  else if(num=="worst"){
    
    num1<-max(t$rank)
    
    result<-filter(t,rank==num1)
    
    
    r<-(paste("Hospital:",result[,c(1)],"State:",i,sep=" "))
    temp<-r
    y<-rbind(y,temp)
    
    
  }else{
    
    result<-filter(t,rank==num)
    
    
    r<-(paste("Hospital:",result[,c(1)],"State:",i,sep=" "))
    temp<-r
    y<-rbind(y,temp)
    
  }
  
  }
  return(y)
} 
else if(outcome=="pneumonia"){
  
  subset3<-subset3[with(subset3,order(Pneumonia)),]
  subset3<-subset3[order(subset3$Pneumonia,subset3$Hospital),]
  
  
  sp<-split(subset3,subset3$State)
  
  st<-names(sp)
  
  y<-NULL
  for (i in st) {
    
    t<-sp[[i]]
    
    t$rank<-1:nrow(t)
    
    #print(t)
    
    if (num=="best") {
      
      num<-min(t$rank)
      
      result<-filter(t,rank==num)
      
      
      r<-(paste("Hospital:",result[,c(1)],"State:",i,sep=" "))
      temp<-r
      y<-rbind(y,temp)
      
      
    }
  else if(num=="worst"){
    
    num1<-max(t$rank)
   # print(num1)
    result<-filter(t,rank==num1)
    
    
    r<-(paste("Hospital:",result[,c(1)],"State:",i,sep=" "))
    temp<-r
    y<-rbind(y,temp)
    
    
  }else{
    
    result<-filter(t,rank==num)
    
    
    r<-(paste("Hospital:",result[,c(1)],"State:",i,sep=" "))
    temp<-r
    y<-rbind(y,temp)
    
  }
  
  }
  
  return(y)
  
}else{
  
  stop("invalid outcome")
  
}
}
  