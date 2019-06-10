best <- function(state, outcome) {
  setwd("C:/Users/bib1/Documents/RStudio/MyProject/Module2Class4")
  cc <- rep('NULL', 46)       
  cc[c(2, 7,11,17,23)] <- 'character'     
  file <- read.csv("outcome-of-care-measures.csv", skip=1,header=FALSE,colClasses = cc)
  if (!state %in% file[,2]) {stop("Invalid Outcome1")}
  if (!outcome %in% c("heart attack","heart failure","pneumonia")) {stop("Invalid Outcome2")}
  if (outcome == "heart attack") {
    subfile <- subset(file,file[,2]==state)
    orderfile<-  subfile[order(as.numeric(subfile[,3]),subfile[,1]),]
  }
  if (outcome == "heart failure") {
    subfile <- subset(file,file[,2]==state)
    orderfile<-  subfile[order(as.numeric(subfile[,4]),subfile[,1]),]
  }
  if (outcome == "pneumonia") {
    subfile <- subset(file,file[,2]==state)
    orderfile<-  subfile[order(as.numeric(subfile[,5]),subfile[,1]),]
  }
  orderfile[1,1]
}

rankhospital <- function(state, outcome, num = "best") {
  setwd("C:/Users/bib1/Documents/RStudio/MyProject/Module2Class4")
  cc <- rep('NULL', 46)       
  cc[c(2, 7,11,17,23)] <- 'character'     
  file <- read.csv("outcome-of-care-measures.csv",header=FALSE, skip=1,colClasses = cc)
  if (!state %in% file[,2]) {stop("Invalid Outcome1")}
  if (!outcome %in% c("heart attack","heart failure","pneumonia")) {stop("Invalid Outcome2")}
  if (outcome == "heart attack") {
    subfile <- subset(file,file[,2]==state)
    orderfile<-  subfile[order(as.numeric(subfile[,3]),subfile[,1]),]
    last<-nrow(subset(orderfile,orderfile[,3]!="Not Available"))
  }
  if (outcome == "heart failure") {
    subfile <- subset(file,file[,2]==state)
    orderfile<-  subfile[order(as.numeric(subfile[,4]),subfile[,1]),]
    last<-nrow(subset(orderfile,orderfile[,4]!="Not Available"))
  }
  if (outcome == "pneumonia") {
    subfile <- subset(file,file[,2]==state)
    orderfile<-  subfile[order(as.numeric(subfile[,5]),subfile[,1]),]
    last<-nrow(subset(orderfile,orderfile[,5]!="Not Available"))
  }
  if (num == "best"){num<-1}
  if (num == "worst"){num<-last}
  orderfile[num,1]
  #else {"NA"}
  #num
  #last
}

rankall <- function(outcome, num = "best") {
  setwd("C:/Users/bib1/Documents/RStudio/MyProject/Module2Class4")
  cc <- rep('NULL', 46)       
  cc[c(2, 7,11,17,23)] <- 'character'     
  file <- read.csv("outcome-of-care-measures.csv", skip=1,header=FALSE,colClasses = cc)
  if (!outcome %in% c("heart attack","heart failure","pneumonia")) {stop("Invalid Outcome2")}
  x<-data.frame(
    hospital=character(),
    st=character()
  )
  state <- unique(file[,2])
  state<-sort(state)
  total <- sum(complete.cases(state))
  for (i in 1:total){
    if (outcome == "heart attack") {
      subfile <- subset(file,file[,2]==state[i])
      orderfile<-  subfile[order(as.numeric(subfile[,3]),subfile[,1]),]
      last<-nrow(subset(orderfile,orderfile[,3]!="Not Available"))
    }
    if (outcome == "heart failure") {
      subfile <- subset(file,file[,2]==state[i])
      orderfile<-  subfile[order(as.numeric(subfile[,4]),subfile[,1]),]
      last<-nrow(subset(orderfile,orderfile[,4]!="Not Available"))
    }
    if (outcome == "pneumonia") {
      subfile <- subset(file,file[,2]==state[i])
      orderfile<-  subfile[order(as.numeric(subfile[,5]),subfile[,1]),]
      last<-nrow(subset(orderfile,orderfile[,5]!="Not Available"))
    }
    if (num == "worst"){x<-rbind(x,cbind(orderfile[last,1],state[i]))}
    else if (num == "best"){x<-rbind(x,cbind(orderfile[1,1],state[i]))}
    else {x<-rbind(x,cbind(orderfile[num,1],state[i]))}
    #i<-i+1
  }
  names(x)<-c("hospital","state")
  x
}

