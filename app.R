source("usePackages.R")
source("loginFns.R")
loadPkgs(c("shiny","DBI","tidyverse"))


resultsModal <- function() {
  modalDialog(
    as.integer(runif(1,1,100)),
    footer = actionButton("nextday", "Next day >")# TODO: button should increase day counter by 1
  )
}

getPlayerID <- function(playername,password){
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  querytemplate <- "SELECT * FROM LeaderPlayer WHERE playername=?id1 AND password=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
  print(query) #for debug
  result <- dbGetQuery(conn,query)
  # If the query is successful, result should be a dataframe with one row
  if (nrow(result)==1){
    playerid <- result$playerid[1]
  } else {
    print(result) #for debugging
    playerid <- 0
  }
  #print(result)
  #print(playerid)
  #Close the connection
  dbDisconnect(conn)
  # return the playerid
  playerid
}


publishScore <- function(playerid,gamevariantid,score){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO LeaderScore (playerid,gamevariantid,asoftime,score) VALUES (?id1,?id2,NOW(),?id3)"
  query <- sqlInterpolate(conn, querytemplate,id1=playerid,id2=gamevariantid,id3=score)
  #print(query) #for debug
  success <- FALSE
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("Score published")
      success <- TRUE
    }, error=function(cond){print("publishScore: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("publishScore: WARNING")
      print(cond)},
    finally = {}
  )
  dbDisconnect(conn)
}

getLeaderBoard <- function(gamevariantid){
  conn <- getAWSConnection()
  #First, we need to know whether highscorewins for this game variant
  query <- paste0("SELECT highscorewins FROM LeaderGameVariant WHERE gamevariantid=",gamevariantid)
  result <- dbGetQuery(conn,query)
  #result should return a single row
  highscorewins <- result$highscorewins[1]
  #Assemble the query for this gamevariantid
  query <- "SELECT lp.playername,ls.score,ls.asoftime  FROM LeaderScore as ls INNER JOIN LeaderPlayer as lp"
  query <- paste0(query," ON (ls.playerid=lp.playerid) WHERE ls.gamevariantid =")
  query <- paste0(query,gamevariantid)
  if (highscorewins)
    query <- paste0(query, " ORDER BY ls.score DESC,ls.asoftime ASC")
  else
    query <- paste0(query, " ORDER BY ls.score ASC,ls.asoftime ASC")
  print(query) # for debugging
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}


ui <- fluidPage(
  # Title
  h3("I LIKE TO MOVIE MOVIE"),
  
  # Day counter
  textOutput("count"),
  
  #Side bar
  sidebarPanel(h4("Drag and Drop Events"),
               tags$div(id = "event1",class = "event", draggable = "true","Movie 1"),
               tags$div(id = "event1",class = "event", draggable = "true","Movie 2")
  ),
  
  # Schedule
  mainPanel(div(h2("EMPTY SCHEDULE HERE")),
            actionButton("run", "RUN")
            
            )
)

server <- function(input, output, session) {
  # reactiveValues object for storing items like the user password
  vals <- reactiveValues(randomName=NULL,password = NULL,playerid=NULL,playername=NULL,gamevariantid=1,score=NULL, countervalue=1)
  
  
  #show modal on startup
  showModal(startUpModal())
  
  ############## listen to inputs buttons ###############
  
  #`Register button (in start up modal)
  observeEvent(input$register, {
    removeModal() #remove startupModal
    showModal(registerModal(failed=FALSE))
  })
  
  observeEvent(input$generate, {
    print("generating")
    vals$randomName = generatename()
  })
  
  # skip login button
  observeEvent(input$skip, {
    removeModal() #remove start up modal
  })
  
  # Register OK button (in register modal)
  observeEvent(input$registerOk, {
    
    # Check that password1 exists and it matches password2
    if (str_length(input$password1) >0 && (input$password1 == input$password2)) {
      vals$password <- input$password1

      vals$playername = registerPlayer(vals$randomName,vals$password)
      
      if (!is.null(vals$playername)){
          vals$playerid <- getPlayerID(vals$playername,vals$password)
      }

      removeModal()
    } else {
      showModal(registerModal(failed = TRUE))
    }
  })
  
  # Login Button (in start up modal)
  observeEvent(input$login, {
    showModal(loginModal(failed=FALSE))
  })
  
  # Login OK button (in login modal)
  observeEvent(input$loginok, {
    
    # Get the playerID and check if it is valid
    playerid <- getPlayerID(input$playername,input$password3)
    
    if (playerid>0) {
      #store the playerid and playername and close the dialog
      vals$playerid <- playerid
      vals$playername <- input$playername

      removeModal() #remove login modal
      showModal(postLoginModal())
      
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })
  
  # Tutorial Button
  observeEvent(input$tutorial, {
    removeModal() #remove post-login modal
    # TODO: Insert Tutorial modal here
  })
  
  
  #  Change PW button
  observeEvent(input$changepw, {
    showModal(changepwModal(vals$playername,FALSE,FALSE))
  })
  
  # confirm change button (in Change PW modal)
  observeEvent(input$confirmchange, {
    # Get the playerID and check if it is valid
    playerid <- getPlayerID(input$playername,input$password4)
    
    if (playerid>0) {
      if(input$password5 == input$password6){
        changePwQuery(input$playername,input$password4,input$password6)
        removeModal()
        showModal(successfulchangeModal())
      }else{showModal(changepwModal(vals$playername,FALSE,TRUE))} # if 2 new pw dont match
      
    } else {
      showModal(changepwModal(vals$playername,TRUE,FALSE)) #if current pw dont match
    }
  })
  
  # RUN button
  observeEvent(input$run, {
    showModal(resultsModal())
  })
  
  # next day button
  observeEvent(input$nextday, {
    removeModal() #remove summary page
    vals$countervalue <- vals$countervalue+1
  })
  
  # Publish score button
  observeEvent(input$publishscore,{
    publishScore(vals$playerid,vals$gamevariantid,vals$score)
  })
  
  
  ############### Output render ####################
  
  # Display name and change PW button if logged in
  output$loggedInAs <- renderUI({
    if (is.null(vals$playername))
      "Not logged in yet."
    else{ 
      tagList(
      p(vals$playername),
      actionButton("changepw", "Change Password")
      )
    }
  })
  
  # Display score (when play button is pressed, score!=0)
  output$score <- renderUI({
    if (is.null(vals$score))
      "No score yet."
    else
      as.character(vals$score)
  })
  
  output$count<- renderText({
    paste("Day ", vals$countervalue)
  }
    )
  
  # Display print Publish Score button
  output$moreControls <- renderUI({
    req(vals$score,vals$playerid) # if vals$score is NULL, the controls will not be visible
    tagList(
      actionButton("publishscore", "Publish Your Score"),
      tableOutput("leaderboard")
    )
  })
  
  #Display leaderboard
  output$leaderboard <- renderTable({numclicks <- input$publishscore +input$playgame #to force a refresh whenever one of these buttons is clicked
                                    leaderboard <- getLeaderBoard(vals$gamevariantid)
                                    leaderboard}
                                    )
}

##################### APP #####################
shinyApp(ui, server)

