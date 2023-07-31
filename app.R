### main code
library(shiny)
library(htmlwidgets)
library(sortable)
library(magrittr)
library(shinyjs)
library(DBI)
library(tidyverse)

source("loginFns.R")
source("moviemaker.R")
#one more for calculations
#one more for leaderboard

disp_conv <- function(x){name = x$MovieName; 
                          len = x$RunTime;
                          colstr=x$Color;
                          
                          lapply(name, function(name){tags$div(
                                                      icon("film", style = "font-size: 15px"),
                                                      style = "font-size: 15px",
                                                      tags$strong(name),
                                                      paste0("(", as.integer(len)*15, "mins)" ),
                                                      `data-mlen` = len,
                                                      `data-name` = name,
                                                      `data-colstr` = colstr
                                                    )}
                               )
                          } #create any custom attribute with `data-`

generateMobjs <- function(movielist){
  Mobjs <- list()
  print("i am generating Mobjs")
  print(length(movielist))
  for(i in 1:length(movielist)){Mobjs[i] <- disp_conv(movielist[[i]])}
  print(length(Mobjs))
  Mobjs
}

resultsModal <- function() {
  modalDialog(
    as.integer(runif(1,1,100)),
    footer = actionButton("nextday", "Next day >")# TODO: button should increase day counter by 1
  )
}

leaderboardModal <- function() {
  modalDialog(
    title = "Leaderboard",
    "Your performance: ",
    easyClose = FALSE,
    footer = actionButton("restart", label = "Restart?")
  )
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(includeCSS("www/styles.css"),
            tags$script(src = "movieobjects.js")),
  fluidRow(class = "panel panel-heading",
           div(class = "panel-heading",
               h3("I like to Movie Movie")
           ),
           sidebarPanel(class = "panel-body",width = 2,
                        
                        column(width = 12,
                               textOutput("loggedInAs"),
                               textOutput("day"),
                               textOutput("cash")# TODO add the cash on hand here
                        ),
                        
                        #Movie list container
                        column(width = 12,
                               tags$div(class = "panel panel-default",
                                        tags$div(class = "panel-heading", icon("film"), tags$strong("Movies") ), #makes it bold
                                        uiOutput("movieobjects")
                                        
                               )
                        ),
                        
                        #Legend container
                        column(width = 12,
                               tags$div(class = "panel panel-default",
                                        tags$div(class = "panel-heading", icon("compass"), tags$strong("Legend")),
                                        tags$div(class = "panel-body", id = "legend",
                                                 tags$div(class = "ad", id = "ad", "Advertisements"),
                                                 tags$div(class = "clean", id = "clean", "Cleaning"),
                                                 textOutput("ticketprices") #TODO: make this change when it is weekend/PH
                                        )
                               )
                        ),
                        
                        #Remove item container
                        column(width = 12,
                               tags$div(class = "panel panel-default",
                                        tags$div( class = "panel-heading", icon("trash"), tags$strong("Remove item") ),
                                        tags$div(class = "panel-body", id = "sortable_bin")
                               )
                        ),
                        
                        #Run button
                        column(width = 12,actionButton("run", "RUN")),
                        
                        
           ), #bracket for closing sidebarPanel
           
           #main ui area
           mainPanel(
             
             #Hall 1
             column(tags$style(HTML(".hall-body {min-height: 500px;}")),
                    width = 3,
                    tags$div(class = "panel panel-default",
                             tags$div(class = "panel-heading", icon("play"), "Hall 1"),
                             tags$div(class = "hall-body", id = "hall1",
                                      lapply(0:47, function(i) { #allows us to create the timings. change the loop number to change day length
                                        time = sprintf("%02d:%02d", 11 + i %/% 4, (i %% 4) * 15)
                                        tags$div(class = "time-frame", id = paste0("1time", i+1), time, #makes the period frame
                                                 tags$div(class = "entrycell", id = paste0("hall1period", i+1))) #makes the entry cell (green one to drop). entry cells are tied to the frame.
                                      })
                             )
                    )
             ),
             
             #Hall 2
             column(tags$style(HTML(".hall-body {min-height: 500px;}")),
                    width = 3,
                    tags$div(class = "panel panel-default",
                             tags$div(class = "panel-heading", icon("play"), "Hall 2"),
                             tags$div(class = "hall-body", id = "hall2",
                                      lapply(0:47, function(i) { 
                                        time = sprintf("%02d:%02d", 11 + i %/% 4, (i %% 4) * 15)
                                        tags$div(class = "time-frame", id = paste0("2time", i+1), time, #note the frames and cells have unique ids for each hall
                                                 tags$div(class = "entrycell", id = paste0("hall2period", i+1)))
                                      })
                             )
                    )
             ),
             
             #Hall 3
             column(tags$style(HTML(".hall-body {min-height: 500px;}")),
                    width = 3,
                    tags$div(class = "panel panel-default",
                             tags$div(class = "panel-heading", icon("play"), "Hall 3"),
                             tags$div(class = "hall-body", id = "hall3",
                                      lapply(0:47, function(i) { 
                                        time = sprintf("%02d:%02d", 11 + i %/% 4, (i %% 4) * 15)
                                        tags$div(class = "time-frame", id = paste0("3time", i+1), time, 
                                                 tags$div(class = "entrycell", id = paste0("hall3period", i+1)))
                                      })
                             )
                    )
             ),
             
             #Hall 4
             column(tags$style(HTML(".hall-body {min-height: 500px;}")),
                    width = 3,
                    tags$div(class = "panel panel-default",
                             tags$div(class = "panel-heading", icon("play"), "Hall 4"),
                             tags$div(class = "hall-body", id = "hall4",
                                      lapply(0:47, function(i) { 
                                        time = sprintf("%02d:%02d", 11 + i %/% 4, (i %% 4) * 15)
                                        tags$div(class = "time-frame", id = paste0("4time", i+1), time, 
                                                 tags$div(class = "entrycell", id = paste0("hall4period", i+1)))
                                      })
                             )
                    )
             ),
           ) #bracket for closing mainPanel
  ),#bracket for closing fluid row
  
  
  #making the legend. technically such a run around way but my brain was too tired to think properly. had to make 3 classes in the ui side lol.
                      #technically we dont need this block of code, but I shall leave it in here just for the lols - melvin
  
  sortable_js("legend", 
              options = sortable_options(
                sort = FALSE,
                filter = list('.ad', '.run', '.clean'),
                group = list(
                  pull = F,
                  name = "sortGroup2",
                  put = F
                )
              )
  ),
  
  #makes the hall a sortable list. this list is for the time frames NOT the entry cells.
  
  # Hall1 sortables
  sortable_js("hall1",  
              options = sortable_options(
                sort = F, #prevents movement of the time frames so u cant drag any of the frames.
                filter = '.time-frame', #prevents interaction with the time frames totally. makes them look like a static object.
                group = list(
                  group = "sortGroup1", #'sortGroup1' allows the frames to communicate. different lists must have same name in order to drag and drop between them. cant drop into a diff group.
                  put = F, #prevents dropping anything into the time frames. e.g. u can drop movies inbetween time frames if dont have this. 
                  pull = F #prevents dragging any timeframe out of the hall
                ),
                onSort = sortable_js_capture_input("sort_y"), #leave here for my future ref
              )
              
  ),
  
  # Hall2 sortables (same thing as hall 1. All the comments are the same for all halls)
  sortable_js("hall2", 
              options = sortable_options(
                sort = F,
                filter = '.time-frame',
                group = list(
                  group = "sortGroup1",
                  put = F,
                  pull = F
                ),
                onSort = sortable_js_capture_input("sort_y")
              )
  ),
  
  #Hall 3 sortables
  sortable_js("hall3",
              options = sortable_options(
                sort = F,
                filter = '.time-frame',
                group = list(
                  group = "sortGroup1",
                  put = F,
                  pull = F
                ),
                onSort = sortable_js_capture_input("sort_y")
              )
  ),
  
  #Hall 4 sortables
  sortable_js("hall4",
              options = sortable_options(
                sort = F,
                filter = '.time-frame',
                group = list(
                  group = "sortGroup1",
                  put = F,
                  pull = F
                ),
                onSort = sortable_js_capture_input("sort_y")
              )
  ),
  
  #Making the garbage disposal
  sortable_js("sortable_bin", 
              options = sortable_options(
                group = list(
                  group = "sortGroup1",
                  put = TRUE,
                  pull = TRUE
                ),
                onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }") # i think kinda self-explanatory this one. jz kills the item (movie).
              )
  ),
  
  #We gonna make the entry cells (the green box) now to drop the movies into. this makes each cell in each frame a unique sortable list. helps us call later
  
  #Hall 1 green cells
  lapply(0:47, function(i) { 
    period_id <- paste0("hall1period", i+1) #hall1period1 = first time of the day e.g. 11:00, hall1period2 = 11.15, so on so forth
    sortable_js(
      period_id,
      options = sortable_options(     
        group = list(group ="sortGroup1", 
                     
                     #'put' limits the cell to only take one element, preventing two movies from dropping into the same time slot
                     put = htmlwidgets::JS("function (to) {return to.el.children.length < 1;}")),
        
                      #onAdd executes the js code when a movie is dropped into a cell             
                      #ps the onAdd cant comment inline cos stupid js and r tingz. 
                      #but this is all to change the frame and cell colors when dropping. i tried to name it to make it as easy to understand le
                      onAdd = htmlwidgets::JS("function (evt) {onAddFunction1(evt);}"),
        
                      #onMove executes the moment u drag the movie off and it touches a different cell. 
                      #even hovering will count. this js reverts the colours back to the default
                      onMove = htmlwidgets::JS("function (evt) {onMoveFunction1(evt);}")
      )
    )
  }),
  
  #comments same as hall1. all this code is copy pasted just changed the names accordingly for the diff halls.
  #Hall 2 green cells
  lapply(0:47, function(i) {
    period_id <- paste0("hall2period", i+1)
    sortable_js(
      period_id,
      options = sortable_options(
        group = list(group ="sortGroup1", 
                     put = htmlwidgets::JS("function (to) {return to.el.children.length < 1;}")),
                      onAdd = htmlwidgets::JS("function (evt) {onAddFunction2(evt);}" ),
                      onMove = htmlwidgets::JS("function (evt) {onMoveFunction2(evt);}")
      )
    )
  }),
  
  #Hall 3 green cells
  lapply(0:47, function(i) {
    period_id <- paste0("hall3period", i+1)
    sortable_js(
      period_id,
      options = sortable_options(
        group = list(group ="sortGroup1", 
                     put = htmlwidgets::JS("function (to) {return to.el.children.length < 1;}")),
                      onAdd = htmlwidgets::JS("function (evt) {onAddFunction3(evt);}" ),
                      onMove = htmlwidgets::JS("function (evt) {onMoveFunction3(evt);}")
      )
    )
  }),
  
  #Hall 4 green cells
  lapply(0:47, function(i) {
    period_id <- paste0("hall4period", i+1)
    sortable_js(
      period_id,
      options = sortable_options(
        group = list(group ="sortGroup1",
                     put = htmlwidgets::JS("function (to) {return to.el.children.length < 1;}")),
                      onAdd = htmlwidgets::JS("function (evt) {onAddFunction4(evt);}" ),
                      onMove = htmlwidgets::JS("function (evt) {onMoveFunction4(evt);}")
      )
    )
  })
  
)

server <- function(input, output, session) {
  # reactiveValues object for storing items like the user password
  vals <- reactiveValues(randomName=NULL, password = NULL, playerid=NULL, playername=NULL, gamevariantid=1, score=NULL, day=1, gameEnded=F, cash=10000, mobjs=NULL)
  
  
  #show modal on startup
  showModal(startUpModal())
  
  ############## listen to inputs buttons ###############
  
  #`Register button (in start up modal)
  observeEvent(input$register, {
    removeModal() #remove startupModal
    vals$randomName <- GenerateName()
    print(vals$randomName)
    showModal(registerModal(vals$randomName,failed=FALSE))
  })
  
  #skip button
  observeEvent(input$skip, {
    removeModal() #remove start up modal
    vals$movielist<-generatemlist(1)
    vals$mobjs<-generateMobjs(vals$movielist) #generate movie list
  })
  
  #Generate Button
  observeEvent(input$generate, {
    print("generating")
    vals$randomName <- GenerateName()
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
      
      #TODO:load initial conditions
      vals$movielist<-generatemlist(1)
      vals$mobjs<-generateMobjs(vals$movielist) #generate movie list

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
  
  observeEvent(input$startgame, {
    removeModal() #remove tutorial modal
    
    #TODO:load initial conditions
    vals$movielist<-generatemlist(1)
    vals$mobjs<-generateMobjs(vals$movielist) #generate movie list
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
    dataArr <- htmlwidgets::JS('getScheduledData()') #TODO: read HTML data for movie timings for calculations
    print(dataArr)
    #results <- calculate(dataArr)
    #update cash balance
    showModal(resultsModal())
    
    if (vals$day == 14) {
      updateActionButton(session, "nextday", label = "View leaderboard")
      vals$gameEnded <- TRUE
    }
  
    
  })
  
  # next day button
  observeEvent(input$nextday, {
    removeModal() #remove summary page
    
    if (vals$gameEnded) {
      showModal(leaderboardModal())
    }
    
    vals$day <- vals$day+1
    
    if (vals$day %in% c(4,9,12)){ 
      vals$movielist<-generatemlist(vals$day)
      vals$mobjs<-generateMobjs(vals$movielist)
    }
  })
  
  observeEvent(input$restart, {
    # TODO: LOAD INITIAL CONDITIONS (cash = 10k, reset movies, bla bla bla)
    vals$day <- 1
    vals$gameEnded <- FALSE
    updateActionButton(session, "nextday", label = "Next day >")
    removeModal() # Close the summary page modal
  })
  
############### Output render ####################
  
  # Display name 
  output$loggedInAs <- renderText({
    if (is.null(vals$playername))
      "Not logged in yet."
    else{ 
      vals$playername
    }
  })
  
  # Display cash 
  output$cash <- renderText({
      paste("Cash balance: $ ", vals$cash)
  })
  
  #Day counter
  output$day <- renderText({
    if (vals$day <= 14) {
      paste("Day ", vals$day, "/14")
    } else {
      "Game Ended"
    }
  })
  
  #ticket prices
  output$ticketprices <- renderText({
    if (vals$day %in% c(6,7,12,13,14)) {
      paste("$", 10, "/ticket")
    } else {
      paste("$", 7, "/ticket")
    }
  })
  
  
  #movie objects
  output$movieobjects<- renderUI({
    print ("im rendering movie objects")
    print (length(vals$mobjs))
    tags$div(class = "panel-body", 
             id = "movies",
             vals$mobjs,
             sortable_js("movies", #note the container id
                                    options = sortable_options(
                                      sort = FALSE, #prevents the list items from moving inside the container but still can interact
                                      group = list(
                                        pull = "clone", #clones the movie when we drag it
                                        name = "sortGroup1", #makes all the movies fall under sortGroup1. kinda like a category
                                        put = F #prevents dropping into the movie area
                                      ),
                                      onSort = sortable_js_capture_input("sort_vars") #technically useless jz leave here first for my referencing
                                    )
             )
             )
  })
}

##################### APP #####################
shinyApp(ui, server)

