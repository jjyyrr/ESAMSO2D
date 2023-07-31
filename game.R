getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student094", 
    host = "database-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student094", 
    password = "mdmea!4Qd*eH")
  
  conn
}

query <- function(queryString){
  conn <- getAWSConnection()
  result <- dbGetQuery(conn,queryString)
  dbDisconnect(conn)
  result
}


initialConditions <- function(){}

calcualteAdRevenue <- function(n){#input number of movies
  revenuePerAd<-100
  n*revenuePerAd
  } 

calculateRentalCost <- function(){#input number of movies for each movie
  # movie 1, query movie 1 price,* number of times
  # movie 2, query movie 1 price,* number of times
  cost<-0
  #for i in movies{
  
  #querytemplate <- "SELECT RentalPrice FROM MoviesDB WHERE moviename==?movie"
  #queryString<- sqlInterpolate(conn, querytemplate,movie=day)
  #queryoutput<-query(queryString)
  
  #cost <- cost + queryoutput*number of times shown
  #}
  cost
  
} 

calculateDemandCoeff <- function(){}

calculateReleaseCoeff <- function(){} #input is the movie

calculateRatingsCoeff <- function(){
  #querytemplate <- "SELECT ratings FROM MoviesDB WHERE moviename==?movie"
  #queryString<- sqlInterpolate(conn, querytemplate,movie=movie)
  #queryoutput<-query(queryString)
  #stars<-queryoutput
  #0.125*stars + 0.375
}

calculateTicketsRevenue <- function(){# input the whole df
  rev<-0
  for (show in shows){
    #calculate demandcoeff for each timeslot
    demandcoeff <- calculateDemandCoeff()
    releasecoeff <- calculateReleaseCoeff() #days since release
    ratingscoeff <- calculateRatingsCoeff()
    
    #<-capacity * demandcoeff * releasecoeff * ratingscoeff
    #tixsold<-min ()
    
    #rev<- rev + tixsold*ticketprice
  }
  #rev
  
} 

calculations<- function(df){
  AdRevenue<-calcualteAdRevenue(nrows(df)) 
  
  #df grp by movie name
  RentalCost<-calculateRentalCost()
  
  ##
  TicketsRevenue<-calculateTicketsRevenue()
  
  TotalRevenue<-AdRevenue+TicketsRevenue
  Profit<-TotalRevenue-RentalCost
  
  #convert all to df, return
  #return cost, rental(permovie or total?), tickets revenue(breakdown of tix per show? breakdown of revenue per movie?)
}