pollutantmean<-function(directory, pollutant, id=1:332){
  setwd(paste("C:/Users/bib1/Documents/RStudio/MyProject/",directory,sep=""))
  x<-data.frame(
    pollutant=numeric()
  )
  for (file in dir()){
    specdata<-read.csv(file,skip=1,header=FALSE,",")
    names(specdata)<-c("Date","sulfate","nitrate","ID")
    spec<-subset(specdata,specdata$ID %in% id,select=pollutant)
    x <- rbind(x,spec)
  }
  x=t(x)
  mean(x,na.rm =TRUE)
}

complete<-function(directory, id=1:332){
  filename <- paste("C:/Users/bib1/Documents/RStudio/MyProject/",directory,sep="")
  x<-data.frame(
    id=numeric(),
    nobs=numeric()
  )
  for (val in id){
    if (val<10){
      specdata<-read.csv(paste(filename,"/00",val,".csv",sep=""),skip=1,header=FALSE,",")
    }else if(val<100){
      specdata<-read.csv(paste(filename,"/0",val,".csv",sep=""),skip=1,header=FALSE,",")
    }else {
      specdata<-read.csv(paste(filename,"/",val,".csv",sep=""),skip=1,header=FALSE,",")
    }
    names(specdata)<-c("Date","sulfate","nitrate","ID")
    good <- complete.cases(specdata)
    nobs <- nrow(specdata[good,])
    data <- cbind(val,nobs)
    x <- rbind(x,data)
  }
  x
}

corr<-function(directory,threshold=0){
  setwd(paste("C:/Users/bib1/Documents/RStudio/MyProject/",directory,sep=""))
  #x<-data.frame(
  #  corr=numeric()
  #)
  x=c()
  for (file in dir()){
    specdata<-read.csv(file,skip=1,header=FALSE,",")
    names(specdata)<-c("Date","sulfate","nitrate","ID")
    good <- complete.cases(specdata)
    nobs <- nrow(specdata[good,])
    if (nobs>threshold){
      corr<-cor(specdata[good,][,2],specdata[good,][,3])
      x <- c(x,corr)  
    }
  }
  x
}
