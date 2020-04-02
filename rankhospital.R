library(dplyr)

rankhospital<-function(state,outcome,num="best"){
  
  #read data
  data<-read.csv("Dataset/outcome-of-care-measures.csv",colClasses = "character")
  
  #data validation
  possible_outcome<-c("heart attack","heart failure","pneumonia")
  
  if (any(data$State==state)) {
    message("**Your state parameter is valid**")
  }else{
    stop("**invalid state**")
  }
  if (any(outcome==possible_outcome)) {
    message("--Your outcome parameter is valid--")
  }
  else{
    stop("--invalid outcome--")
  } 
  
  
  
  subset1<-data[,c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
  subset2<-data[,c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
  subset3<-data[,c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  
  filData1<-filter(subset1,State==state)
  filData2<-filter(subset2,State==state)
  filData3<-filter(subset3,State==state)
  
  
  filData1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(filData1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  
  filData2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(filData2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  
  filData3$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(filData3$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  
  df1<-na.omit(filData1)
  df2<-na.omit(filData2)
  df3<-na.omit(filData3)
  
  
  if(outcome=="heart attack"){
    
    
    dfFinal1<-df1[,c(1,3)]
    dfFinal1<-dfFinal1[with(dfFinal1,order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
    dfFinal1<-dfFinal1[order(dfFinal1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,dfFinal1$Hospital.Name),]
    dfFinal1$Rank<-seq.int(nrow(dfFinal1))
    colnames(dfFinal1)<-c("Hospital.Name","Rate","Rank")
    #dfFinal1$Rank<-as.character(dfFinal1$Rank)

    if (num=="best") {
      
      num<-min(dfFinal1$Rank)
      
      print(dfFinal1[num,1])
      
    }else if(num=="worst"){
      
      num<-max(dfFinal1$Rank)
      
      print(dfFinal1[num,1])
      
    }else{
      if (num>nrow(dfFinal1)) {
        return(NA)
      }else{
        print(dfFinal1[num,1])
      }
      
    }
     
          
  }else if(outcome=="heart failure"){
    
    dfFinal2<-df2[,c(1,3)]
    dfFinal2<-dfFinal2[with(dfFinal2,order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
    dfFinal2<-dfFinal2[order(dfFinal2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,dfFinal2$Hospital.Name),]
    dfFinal2$Rank<-seq.int(nrow(dfFinal2))
    colnames(dfFinal2)<-c("Hospital.Name","Rate","Rank")
    #dfFinal2$Rank<-as.character(dfFinal2$Rank)
    
    if (num=="best") {
      
      num<-min(dfFinal2$Rank)

      print(dfFinal2[num,1])
      
    }else if(num=="worst"){
      
      num<-max(dfFinal2$Rank)

      print(dfFinal2[num,1])
      
    }else{
        if (num>nrow(dfFinal2)) {
          return(NA)
        }else{
          print(dfFinal2[num,1])
        }
        
    }
        
    
  }else if(outcome=="pneumonia"){
    
    dfFinal3<-df3[,c(1,3)]
    dfFinal3<-dfFinal3[with(dfFinal3,order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
    dfFinal3<-dfFinal3[order(dfFinal3$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,dfFinal3$Hospital.Name),]
    dfFinal3$Rank<-seq.int(nrow(dfFinal3))
    colnames(dfFinal3)<-c("Hospital.Name","Rate","Rank")
    
    if (num=="best") {
      
      num<-min(dfFinal3$Rank)
      
      print(dfFinal3[num,1])
      
    }else if(num=="worst"){
      
      num<-max(dfFinal3$Rank)
      
      print(dfFinal3[num,1])
      
    }else{
      if (num>nrow(dfFinal3)) {
        return(NA)
      }else{
        print(dfFinal3[num,1])
      }
      
    }
        
        
  }
  
}