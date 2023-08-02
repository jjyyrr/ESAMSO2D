library(DBI)
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
    queryoutput<-dbGetQuery(conn,queryString)
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
  queryoutput<-dbGetQuery(conn,queryString)
  dbDisconnect(conn)
  
  queryoutput[[1]]
}

calculateReleaseCoeff <- function(movie,day){
  
  conn <- getAWSConnection()
  querytemplate <- "SELECT EndingDate FROM MoviesDB WHERE MovieName=?name"
  queryString<- sqlInterpolate(conn, querytemplate,name=movie)
  queryoutput<-dbGetQuery(conn,queryString)
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
  queryoutput<-dbGetQuery(conn,queryString)
  dbDisconnect(conn)
  
  ratingscoeff<- 0.125*queryoutput[["Popularity"]] + 0.375
  ratingscoeff
}

calculateTicketsSold <- function(scheduled){
  day<-scheduled[1,"day"]
  
  conn <- getAWSConnection()
  
  if (day %in% c(6,7,12,13,14)) {
    querytemplate2 <- "SELECT Weekend FROM Demand"
  } else {
    querytemplate2 <- "SELECT Weekday FROM Demand"
  }
  
  queryString2<- sqlInterpolate(conn, querytemplate2,name=movie)
  demandmodel<-dbGetQuery(conn,queryString2)
  
  for (movie in unique(scheduled[,"movie"])){
    
    releasecoeff <- calculateReleaseCoeff(movie,day) 
    ratingscoeff <- calculateRatingsCoeff(movie)
    
    scheduledSorted<-scheduled[order(scheduled$period), ] #sort by period
    scheduled$tixsold <- round(100 * demandmodel[scheduledSorted$period,] * releasecoeff * ratingscoeff * runif(1,0.8,1.2))
    
    querytemplate1 <- "SELECT Dailydemand FROM MoviesDB WHERE MovieName=?name"
    queryString1<- sqlInterpolate(conn, querytemplate1,name=movie)
    queryoutput1<-dbGetQuery(conn,queryString1)
    dd<-queryoutput1[["Dailydemand"]]
    print(dd)
    
    for (i in 1:nrow(scheduled[scheduled["movie"]== movie,])){
      print(scheduled[scheduled["movie"]==movie,])
      dd<-dd - scheduled[scheduled["movie"]==movie,][i,]$tixsold
      print(dd)
      if (dd<=0) { #exceed daily demand, max(tixsold + dd , 0)
         scheduled[scheduled["movie"]==movie,][i,]$tixsold <- max(scheduled[scheduled["movie"]==movie,][i,]$tixsold + dd, 0)
        }
    
    }
  }
  dbDisconnect(conn)
  scheduled
} 

calculate<- function(scheduled){
  scheduled2 <- calculateTicketsSold(scheduled)
  AdRevenue <- calcualteAdRevenue( nrow(scheduled) ) 
  RentalCost <- calculateRentalCost( table(scheduled$movie) )
  TicketsRevenue
  
  TotalRevenue <- AdRevenue + TicketsRevenue
  Profit <- TotalRevenue - RentalCost
  
  #return cost, rental(permovie or total?), tickets revenue(breakdown of tix per show? breakdown of revenue per movie?)
  result<- data.frame(Day = day, AdRevenue = AdRevenue, TicketsRevenue = TicketsRevenue, RentalCost=RentalCost, Profits=Profit)
  result
}

#scheduled<-read.csv("scheduled.csv")
#scheduled
#calculateTicketsSold(scheduled)
