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

calcualteAdRevenue <- function(tab){#input number of movies
  revenuePerAd<-100
  tab$AdRevenue<-tab$shown*revenuePerAd
  tab
} 

calculateRentalCost <- function(tab){#table of movie and count
    
    conn <- getAWSConnection()
    querytemplate <- "SELECT MovieName, Cost FROM MoviesDB"
    queryString<- sqlInterpolate(conn, querytemplate,name=name)
    table2<-dbGetQuery(conn,queryString)
    dbDisconnect(conn)
    
    colnames(table2) <- c("Movie", "Cost")
    table3<-left_join(tab, table2, by = "Movie")
    table3$rentalcost<-table3$shown * table3$Cost
    table3[, c("Movie","rentalcost")]
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

calculatemovieStats<- function(scheduled2){
  scheduled2tab <- as.data.frame(table(scheduled2$movie))
  colnames(scheduled2tab) <- c("Movie", "shown")
  
  day<-scheduled2[1,"day"]
  AdRevenue<-calcualteAdRevenue(scheduled2tab)

  RentalCost<-calculateRentalCost(scheduled2tab)

  Ntixsold<-aggregate(tixsold ~ movie, scheduled2, sum)

  if (day %in% c(6,7,12,13,14)) {
    Ntixsold$ticketRevenue<-Ntixsold$tixsold*10
  } else {
    Ntixsold$ticketRevenue<-Ntixsold$tixsold*7
  }
  
  colnames(Ntixsold) <- c("Movie", "No. tickets sold","Ticket revenue")
  
  movie_stats <- merge(AdRevenue, RentalCost, by = "Movie")
  movie_stats <- merge(movie_stats, Ntixsold, by = "Movie")
  movie_stats <-data.frame(Day = day, movie_stats)
  colnames(movie_stats) <- c("Day","Movie","Shown","Ad Revenue","Rental Cost", "No. Tickets Sold","Tickets Revenue")
  movie_stats
}


calculateResult<- function(movie_stats){
  
  day<-movie_stats[1,"Day"]
  
  AdRevenue <- sum(movie_stats["Ad Revenue"])
  RentalCost <- sum(movie_stats["Rental Cost"])
  TicketsRevenue <- sum(movie_stats["Tickets Revenue"])
  
  TotalRevenue <- AdRevenue + TicketsRevenue
  Profit <- TotalRevenue - RentalCost
  
  result<- data.frame(day = as.integer(day), AdRevenue = AdRevenue, TicketsRevenue = TicketsRevenue, RentalCost=RentalCost, Profits=Profit)
  result
}

calculateUtilisation <- function(scheduled2){

  utils <- round(aggregate(rt ~ hall, scheduled2, sum)[2]/58,2)
  filled <- round(aggregate(tixsold ~ hall, scheduled2, sum)[2]/ (table(scheduled2$hall)*100),2)
  
  utilisation<- data.frame(day=as.integer(scheduled2[1,"day"]),
                           hall1util=utils[1,],
                           hall2util=utils[2,],
                           hall3util=utils[3,],
                           hall4util=utils[4,],
                           hall1filled=filled[1,],
                           hall2filled=filled[2,],
                           hall3filled=filled[3,],
                           hall4filled=filled[4,]
                           )
  utilisation
}

