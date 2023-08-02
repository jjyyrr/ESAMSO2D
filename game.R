
library(tidyverse)

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student094", 
    host = "database-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student094", 
    password = "mdmea!4Qd*eH")
  
  conn
}

initialConditions <- function(){}

calcualteAdRevenue <- function(n){#input number of movies
  revenuePerAd<-100
  n*revenuePerAd
} 

calculateRentalCost <- function(tab){#table of movie and count

  cost<-0
  
  for (name in names(tab)){
    conn <- getAWSConnection()
    querytemplate <- "SELECT Cost FROM MoviesDB WHERE MovieName=?name"
    queryString<- sqlInterpolate(conn, querytemplate,name=name)
    queryoutput<-query(queryString)
    dbDisconnect(conn)

    cost<- cost+ tab[[name]]*queryoutput[["Cost"]]
    
    }
  cost
  
} 

calculateDemandCoeff <- function(period,day){
  
  conn <- getAWSConnection()
  
  if (day %in% c(6,7,12,13,14)) {
    querytemplate <- "SELECT Weekend FROM Demand WHERE Period=?period"
  } else {
    querytemplate <- "SELECT Weekday FROM Demand WHERE Period=?period"
  }
  
  queryString<- sqlInterpolate(conn, querytemplate,period=period)
  queryoutput<-query(queryString)
  dbDisconnect(conn)
  
  queryoutput[[1]]
}

calculateReleaseCoeff <- function(movie,day){
  
  conn <- getAWSConnection()
  querytemplate <- "SELECT EndingDate FROM MoviesDB WHERE MovieName=?name"
  queryString<- sqlInterpolate(conn, querytemplate,name=movie)
  queryoutput<-query(queryString)
  dbDisconnect(conn)
  endingday<-queryoutput[["EndingDate"]]
  
  a<- 0.3*(endingday-day)-2.2
  
  releasecoeff<-0.25 * ( (exp(a)-exp(-a))  /(exp(a)+exp(-a)) )+0.75 #tanh
  
  releasecoeff
  
} #input is the movie

calculateRatingsCoeff <- function(name){
  conn <- getAWSConnection()
  querytemplate <- "SELECT Popularity FROM MoviesDB WHERE MovieName=?name"
  queryString<- sqlInterpolate(conn, querytemplate,name=name)
  queryoutput<-query(queryString)
  dbDisconnect(conn)
  
  ratingscoeff<- 0.125*queryoutput[["Popularity"]] + 0.375
  ratingscoeff
}

calculateTicketsRevenue <- function(scheduled,day){
  rev<-0
  
  conn <- getAWSConnection()
  
  if (day %in% c(6,7,12,13,14)) {
    querytemplate2 <- "SELECT Weekend FROM Demand"
  } else {
    querytemplate2 <- "SELECT Weekday FROM Demand"
  }
  
  queryString2<- sqlInterpolate(conn, querytemplate2,name=movie)
  demandmodel<-query(queryString2)
  
  for (movie in unique(scheduled[,"movie"])){
    
    releasecoeff <- calculateReleaseCoeff(movie,day) 
    ratingscoeff <- calculateRatingsCoeff(movie)
    
    querytemplate1 <- "SELECT Dailydemand FROM MoviesDB WHERE MovieName=?name"
    queryString1<- sqlInterpolate(conn, querytemplate1,name=movie)
    queryoutput1<-query(queryString1)
    
    dd<-queryoutput1[["Dailydemand"]]
    
    
    for (p in scheduled[scheduled["movie"]==movie,"period"]){
      
      demandcoeff <- demandmodel[p,]
    
      tixsold <- round(100 * demandcoeff * releasecoeff * ratingscoeff * runif(1,0.5,1))
      dd<-dd-tixsold
      
      
      if (dd<=0) {
        rev<- rev + 0
      } else { if(day %in% c(6,7,12,13,14)){
                  rev<- rev + tixsold * 10
                } else{
                   rev<- rev + tixsold *  7
                  }
      }
    
  }
  }
  dbDisconnect(conn)
  rev
} 

calculate<- function(scheduled,day){
  AdRevenue <- calcualteAdRevenue( nrow(scheduled) ) 
  RentalCost <- calculateRentalCost( table(scheduled$movie) )
  TicketsRevenue <- calculateTicketsRevenue(scheduled,day)
  
  TotalRevenue <- AdRevenue + TicketsRevenue
  Profit <- TotalRevenue - RentalCost
  
  #return cost, rental(permovie or total?), tickets revenue(breakdown of tix per show? breakdown of revenue per movie?)
  result<- data.frame(Day = day, AdRevenue = AdRevenue, TicketsRevenue = TicketsRevenue, RentalCost=RentalCost, Profits=Profit)
  print(result)
  result
}



scheduled<-read.csv("scheduled.csv")
