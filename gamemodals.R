
tutorialModal <- function() {
  modalDialog(
    title = "Tutorial",
    div(tags$p("You are a film programmer of a cinema, schedule movies in order to optimise profits! Drag movies into the halls to schedule and hit run on the bottom left to simulate the day's earnings. Take note that different timings may bring different crowds! The final score is your cash balance at the end of 14 days")),
    footer = tagList(
      actionButton("startgame", "Start Game")
    )
    
  )
}

day2modal <- function() {
  modalDialog(
    title = "It's day 2!",
    div(tags$p("Great work on your first day on the job, keep up the good work!")),
    footer = tagList(
      modalButton("Lets Go!")
    )
    
  )
}

day4modal <- function() {
  modalDialog(
    title = "Its day 4!",
    div(tags$p("It's Thursday, a new movie has just been released! Modify your schedule to meet the demands for the new movie.")),
    footer = tagList(
      modalButton("Lets Go!")
    )
    
  )
}

day6modal <- function() {
  modalDialog(
    title = "The weekend is here!",
    div(tags$p("The weekend is here! Tickets are now selling at $10/ticket. Modify your schedule to meet demands for the weekend.")),
    footer = tagList(
      modalButton("Lets Go!")
    )
  )
}

day8modal <- function() {
  modalDialog(
    title = "Monday Blues",
    div(tags$p("Awesome work over the weekend! It's monday, ticket prices are back to $7/ticket.")),
    footer = tagList(
      modalButton("Lets Go!")
    )
  )
}

day9modal <- function() {
  modalDialog(
    title = "Mission Possible unlisted",
    div(tags$p("It has been 90 days since Mission Possible was released in cinemas. It is now released on Netflox. Management has decided that it is no longer profitable to screen it.")),
    footer = tagList(
      modalButton("Lets Go!")
    )
  )
}

day12modal <- function() {
  modalDialog(
    title = "Merry Christmas!",
    div(tags$p("Tis the season to be jolly! A holiday special movie has been released! Ticket prices have also increased to $10/ticket.")),
    footer = tagList(
      modalButton("Lets Go!")
    )
  )
}

utilsModal <- function() {
  modalDialog(
    title = "Utilisation data",
    tableOutput("utilisationboard"),
    size="l",
    footer = tagList(actionButton("downloadUtils", "Download Data"),modalButton("Done"))
  )
}

statsModal <- function() {
  modalDialog(
    title = "Movie Statistics",
    tableOutput("statsboard"),
    size="l",
    footer = tagList(actionButton("downloadStats", "Download Data"),modalButton("Done"))
  )
}

resultboardModal <- function() {
  modalDialog(
    title = "Results",
    tableOutput("resultboard"),
    footer = tagList(actionButton("downloadresults", "Download Data"),actionButton("nextday", "Next day >"))
  )
}

publishscore<-function(playername,cash){
  conn <- getAWSConnection()
  
  querytemplate <- "INSERT INTO Leaderboard (PlayerName,CashBalance) VALUES (?playername,?cash);"
  queryString<- sqlInterpolate(conn, querytemplate,playername=playername,cash=cash)
  result<-dbExecute(conn,queryString)
  
  dbDisconnect(conn)
  
}

getLeaderboard<- function(){
  conn <- getAWSConnection()
  queryString <- "SELECT * FROM Leaderboard ORDER BY CashBalance DESC LIMIT 10"
  queryoutput<-dbGetQuery(conn,queryString)
  dbDisconnect(conn)
  
  queryoutput
}

leaderboardModal <- function(cash) {
  modalDialog(
    title = "Leaderboard",
    paste0("Your performance: ",cash),
    tableOutput("leaderboard"),
    easyClose = FALSE,
    footer = actionButton("restart", label = "Restart?")
  )
}