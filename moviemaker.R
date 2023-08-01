library(shiny)
library(DBI)
library(tidyverse)
library(htmlwidgets)
library(sortable)
library(magrittr)
library(shinyjs)

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

generatemlist <- function(day){
  print("im in generate mlist")
  
  conn <- getAWSConnection()
   
  querytemplate <- "SELECT * FROM MoviesDB WHERE ReleaseDate <= ?day AND EndingDate > ?day"
  
  queryString<- sqlInterpolate(conn, querytemplate,day=day)
  #print(queryString)
  queryoutput<-dbGetQuery(conn,queryString)
  #print(queryoutput)
  dbDisconnect(conn)
  movielist <- list()
  
  for (i in 1:nrow(queryoutput)) {
    assign( paste0("movie", i), subset(queryoutput[i,]) )
    movielist[[i]] <- subset(queryoutput[i,])
  }
  
  movielist
}


