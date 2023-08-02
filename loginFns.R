### AWS stuff ###
getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student094", 
    host = "database-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student094", 
    password = "mdmea!4Qd*eH")
  
  conn
}

getPlayerID <- function(playername,password){
  #open the connection
  conn <- getAWSConnection()
  querytemplate <- "SELECT * FROM PlayersDB WHERE PlayerName=?id1 AND Password=?id2;"
  queryString<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
  result <- dbGetQuery(conn,queryString)
  
  if (nrow(result)==1){
    playerid <- result$PlayerID[1]
  } else {
    print(result) #for debugging
    playerid <- 0
  }
  
  dbDisconnect(conn)
  
  playerid
}

#### startupModal ####
startUpModal <- function() {
  modalDialog(
    title = "Would you like to login or register",
    footer = tagList(
      actionButton("register", "Register"),
      actionButton("login", "Login"),
      #actionButton("skip", "Skip")
    )
    
  )
}

#### register ####

GenerateName <- function(){
  conn <- getAWSConnection()
  
  adj<-query("SELECT word FROM RandomWords WHERE adj=1 ORDER BY RAND() LIMIT 1")
  col<-query("SELECT word FROM RandomWords WHERE adj=2 ORDER BY RAND() LIMIT 1")
  ani<-query("SELECT word FROM RandomWords WHERE adj=3 ORDER BY RAND() LIMIT 1")
  
  dbDisconnect(conn)
  
  n<-paste(adj[1,1],col[1,1],ani[1,1],sep ="" )
}

registerModal <- function(username,failed = FALSE) {
  modalDialog(
    
    title = "Register a new user",
    
    ## TODO: make the generate button work
    textOutput("randomname") ,
    actionButton("generate", "Generate"),
    
    passwordInput("password1", "Enter a new password:"),
    passwordInput("password2", "Confirm by re-entering the new password:"),
    "If successful, you will be assigned a Player Name to go with this password.",
    if (failed)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      #modalButton("Cancel"),
      actionButton("registerOk", "OK")
    )
  )
}

registerPlayer <- function(playername,password){
  conn <- getAWSConnection()
  
  querytemplate <- "INSERT INTO PlayersDB (PlayerName,Password) VALUES (?id1,?id2);"
  queryString<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
  result<-dbExecute(conn,queryString)
  
  dbDisconnect(conn)
  playername
}


#### Login ####

loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login",
    textInput("playername", "Enter your assigned Player Name", "StrongPinkDragon"),
    passwordInput("password3", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered player with that name and password. Try again or re-register.", style = "color: red;")),
    
    footer = tagList(
      #modalButton("Cancel"),
      actionButton("loginok", "OK")
    )
  )
}

postLoginModal <- function() {
  modalDialog(
    title = "Would you like play the tutorial or start the game?",
    footer = tagList(
      actionButton("changepw", "Change Password"),
      actionButton("tutorial", "Tutorial"),
      actionButton("startgame", "Start Game")
    )
    
  )
}

#### Change Password ####

changepwModal <- function(playername, currentwrong = FALSE, newwrong = FALSE) {
  modalDialog(
    title = "Change Password",
    
    p("Changing password for: "),
    HTML("<b>", playername,"</b>"),
    
    passwordInput("passwordcurrent", "Enter your current password:"),
    passwordInput("passwordnew1", "Enter new password:"),
    passwordInput("passwordnew2", "Confirm new password:"),
    
    if (currentwrong)
      div(tags$b("Current password is wrong", style = "color: red;")),
    
    if (newwrong)
      div(tags$b("New passwords do not match", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirmchange", "Change Password")
    )
  )
}


changePwQuery <- function(playername,oldPW,newPW){
  conn <- getAWSConnection()
  querytemplate <- "UPDATE PlayersDB SET Password=?newPW WHERE PlayerName=?Plname AND Password=?oldPW;"
  queryString<- sqlInterpolate(conn, querytemplate, Plname=playername , newPW=newPW,oldPW=oldPW)
  dbExecute(conn,queryString)
  dbDisconnect(conn)
}
