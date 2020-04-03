#source('rankhospital.R')

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
  
  subset1<-subset1[with(subset1,order(HeartAttack)),]
  subset1<-subset1[order(subset1$HeartAttack,subset1$Hospital),]
  
  
  sp<-split(subset1,subset1$State)
  
  st<-names(sp)

  y<-NULL
  for (i in st) {
     
    t<-sp[[i]]
    
    t$rank<-1:nrow(t)
    
    result<-filter(t,rank==num)
  
    
    if(is.null(row.names(result))){
        print(result)
    }
   else{
   
    r<-(result[,c(1,2)])
    temp<-r
    y<-rbind(y,temp)
   }
  }
  print(head(y,10))
} 
  
  