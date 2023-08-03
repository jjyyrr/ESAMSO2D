### main code
library(shiny)
library(shinyjs) # for Javascript
library(htmlwidgets)# for Javascript
library(sortable) # for drag and drop functionality
library(magrittr)
library(DBI) #for interacting with database
library(tidyverse) #for data manipulation

source("loginFns.R")
source("moviemaker.R")
source("game.R")
source("gamemodals.R")

disp_conv <- function(x){name = x$MovieName; 
len = x$RunTime; 
pops = x$Popularity;
pic = x$Pic;
colstr=x$Color;
cost=x$Cost;
endingday=x$EndingDate;
lapply(name, function(name){tags$div(class = "mvcon", id = paste0("moviecon", name),
                                     tags$div(id = "banner", tags$img(src=pic, style = "width: 100%"), `data-mlen` = len, `data-name` = name, `data-colstr` = colstr),
                                     sortable_js(paste0("moviecon", name),  
                                                 options = sortable_options(
                                                   sort = F, #prevents movement of the time frames so u cant drag any of the frames.
                                                   filter = '.mdeets', #prevents interaction with the time frames totally. makes them look like a static object.
                                                   group = list(
                                                     group = "sortGroup1", #'sortGroup1' allows the frames to communicate. different lists must have same name in order to drag and drop between them. cant drop into a diff group.
                                                     put = F, #prevents dropping anything into the time frames. e.g. u can drop movies inbetween time frames if dont have this. 
                                                     pull = "clone" #prevents dragging any timeframe out of the hall
                                                   ),
                                                   onSort = sortable_js_capture_input("sort_y") #leave here for my future ref
                                                 )),
                                     tags$div(class = "mdeets", id = "fdetails",
                                              tags$strong(icon("film"), name),
                                              style = "font-size: 15px",  tags$br(),
                                              paste0("Popularity: ", pops," stars"), tags$br(), 
                                              paste0("Run time: ", as.integer(len)*15, " mins" ),tags$br(),
                                              paste0("Cost: $: ", cost), tags$br(),
                                              paste0("Ending on day ", endingday)
                                              
                                     )
)}
)
}

generateMobjs <- function(movielist){
  Mobjs <- list()
  for(i in 1:length(movielist)){Mobjs[i] <- disp_conv(movielist[[i]])}
  Mobjs
}



ui <- fluidPage(
  useShinyjs(),
  tags$head(includeCSS("www/styles.css"),
            tags$script(src = "movieobjects.js")),
  
  fluidRow(class = "panel panel-heading",
          #style="background-image: url('LowerResbackground.png'); background-size: 100% auto ; background-repeat: no-repeat; background-position: center;",
           div(class = "panel-heading",
               h3(style = "padding: 20px; color: #FFFFFF; text-align: center;", "I like to Movie Movie"),
           ),
           
           sidebarPanel(class = "panel-body", width = 2,
                        
                        #Movie list container
                        column(width = 12,
                               tags$div(class = "panel panel-default",
                                        tags$div(class = "panel-heading", icon("film"), tags$strong("Movies") ), #makes it bold
                                        uiOutput("movieobjects")
                                        
                               )
                        ),
                        
                        #Remove item container
                        column(width = 12,
                               tags$div(class = "panel panel-default",
                                        tags$div( class = "panel-heading", icon("trash"), tags$strong("Remove item") ),
                                        tags$div(class = "panel-body", id = "sortable_bin")
                               )
                        ),
                        
                        #Legend container
                        column(width = 12,
                               tags$div(class = "panel panel-default",
                                        tags$div(class = "panel-heading", icon("compass"), tags$strong("Legend")),
                                        tags$div(class = "panel-body", id = "legend",
                                                 tags$div(class = "ad", id = "ad", "Advertisements"),
                                                 tags$div(class = "clean", id = "clean", "Cleaning"),
                                        )
                               )
                        ),
                        column(width = 12,
                               tags$div(class = "panel panel-default",
                                        tags$div(class = "panel-heading", icon("database"), tags$strong("Data")),
                                        tags$div(class = "panel-body", id = "legend",
                                                 actionButton("utilsmodal", "Utilisation data",style={"width: 100%; background-color:#337ab7; color: white; font-weight: bold"}),
                                                 actionButton("moviestats", "Movie statistics",style={"width: 100%; background-color:#337ab7; color: white; font-weight: bold"})
                                      )    
                                )
                        ),
                        
                        #Run button
                        column(width = 12,actionButton("run", "RUN",style={"width: 100%; background-color:#00A86B;color: white;font-weight: bold"})),
                        
                        
           ), #bracket for closing sidebarPanel
           
           #main ui area
          mainPanel(
            
            fluidRow(
              column(width = 3,
                     tags$div(class = "panel panel-default",
                              tags$div(class = "panel-heading", icon("user"), tags$strong("Username"), style = "text-align: center" ), #makes it bold
                              uiOutput("loggedInAs"),
                              
                     )
              ),
              column(width = 3,
                     tags$div(class = "panel panel-default",
                              tags$div(class = "panel-heading", icon("calendar-days"), tags$strong("Day") , style = "text-align: center"), #makes it bold
                              uiOutput("day")
                              
                     )
              ),
              column(width = 3,
                     tags$div(class = "panel panel-default",
                              tags$div(class = "panel-heading", icon("sack-dollar"), tags$strong("Cash balance"), style = "text-align: center" ), #makes it bold
                              uiOutput("cash")
                              
                     )
              ),
              column(width = 3,
                     tags$div(class = "panel panel-default",
                              tags$div(class = "panel-heading", icon("ticket"), tags$strong("Ticket prices"), style = "text-align: center" ), #makes it bold
                              uiOutput("ticketprices")     
                    )
             ),
),
             
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
  vals <- reactiveValues(randomName=NULL, password = NULL, playerid=NULL, playername=NULL, gamevariantid=1, day=0, gameEnded=F, cash=0, mobjs=NULL, cum_results=NULL,cum_utils=NULL,cum_moviestats=NULL)
  
  
  #show modal on startup
  showModal(startUpModal())
  
  ############## listen to inputs buttons ###############
  
  #`Register button (in start up modal)
  observeEvent(input$register, {
    removeModal() #remove startupModal
    vals$randomName <- GenerateName()
    showModal(registerModal(vals$randomName,failed=FALSE))
  })
  
  #skip button
  observeEvent(input$skip, {
    removeModal() #remove start up modal
    
    #load initial conditions
    vals$cash<- 10000
    vals$day<-1
    vals$movielist<-generatemlist(1)
    vals$mobjs<-generateMobjs(vals$movielist) #generate movie list
    vals$resultsdf <- data.frame(
      Day = integer(),
      AdRevenue= numeric(),
      TicketsRevenue = numeric(),
      RentalCost = numeric(),
      Profits = numeric(),
      stringsAsFactors = FALSE
    )
    vals$utilsdf <- data.frame(hall1util=numeric(),
                               hall2util=numeric(),
                               hall3util=numeric(),
                               hall4util=numeric(),
                               hall1filled=numeric(),
                               hall2filled=numeric(),
                               hall3filled=numeric(),
                               hall4filled=numeric()
    )
  })
  
  #Generate Button
  observeEvent(input$generate, {
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
      showModal(postLoginModal())
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
  
  #  Change PW button
  observeEvent(input$changepw, {
    showModal(changepwModal(vals$playername,FALSE,FALSE))
  })
  
  # Confirm change button (in Change PW modal)
  observeEvent(input$confirmchange, {
    playerid <- getPlayerID(input$playername,input$passwordcurrent)
    
    if (playerid>0) {
      if(input$passwordnew1 == input$passwordnew2){
        changePwQuery(input$playername,input$passwordcurrent,input$passwordnew1)
        removeModal()
        showModal(postLoginModal())
      }else{showModal(changepwModal(vals$playername,FALSE,TRUE))} # if 2 new pw dont match
      
    } else {
      showModal(changepwModal(vals$playername,TRUE,FALSE)) #if current pw dont match
    }
  })
  
  # Tutorial Button
  observeEvent(input$tutorial, {
    removeModal() #remove post-login modal
    showModal(tutorialModal())
  })
  
  # Start game button (in post login modal and tutorial modal)
  observeEvent(input$startgame, {
    removeModal() #remove tutorial modal
    
    #load initial conditions
    vals$cash<- 10000
    vals$day<-1
    vals$movielist<-generatemlist(1)
    vals$mobjs<-generateMobjs(vals$movielist) #generate movie list
    vals$cum_results <- data.frame("Day" = integer(),
                                  "Ad Revenue"= numeric(),
                                  "Tix Revenue" = numeric(),
                                  "Rental Cost" = numeric(),
                                  "Profits" = numeric()
                                )
    vals$cum_utils <- data.frame("Day"=integer(),
                               "H1 Util" = numeric(),
                               "H2 Util" = numeric(),
                               "H3 Util" = numeric(),
                               "H4 Util" = numeric(),
                               "H1 % filled" = numeric(),
                               "H2 % filled" = numeric(),
                               "H3 % filled" = numeric(),
                               "H4 % filled" = numeric()
                              )
    vals$cum_moviestats<- data.frame("Day" = integer(),
                                    "Movie" = numeric(),
                                    "Shown"=  integer(),
                                    "Ad Revenue" = numeric(),
                                    "Rental Cost" = numeric(), 
                                    "No. Tickets Sold" = numeric(),
                                    "Tickets Revenue" = numeric()
                                    )
  })
  
  # RUN button
  observeEvent(input$run, {
    runjs("getScheduledData()")
    
    if (vals$day == 14) {
      updateActionButton(session, "nextday", label = "View leaderboard")
      vals$gameEnded <- TRUE
    }
  
  })
  
  ### reading from timetable
  observeEvent(input$jsoutput, {
    df<-data.frame(input$jsoutput)
    
    scheduled <- data.frame(
      Day = as.integer(vals$day),
      Movie = df$input.jsoutput[c(TRUE, FALSE, FALSE, FALSE)],
      Period = as.integer(df$input.jsoutput[c(FALSE, TRUE, FALSE, FALSE)]),
      RT = as.integer(df$input.jsoutput[c(FALSE, FALSE, TRUE, FALSE)]),
      Hall = as.integer(df$input.jsoutput[c(FALSE, FALSE, FALSE, TRUE)])
    )

    #append data dataframe
    scheduled2 <- calculateTicketsSold(scheduled)
    print(scheduled2)
    
    moviestat <- calculatemovieStats(scheduled2)
    vals$cum_moviestats <- rbind(vals$cum_moviestats, moviestat)
    
    result <- calculateResult(moviestat) #returns adrev, tix rev, rental cost, profit
    vals$cum_results <- rbind(vals$cum_results, result)

    vals$cash<-vals$cash + result[["Profits"]] #add profit to cash balance
    
    utilisation <- calculateUtilisation(scheduled2)
    vals$cum_utils<- rbind(vals$cum_utils, utilisation)
    
    showModal(resultboardModal())
  })
    
    
  # Next day button
  observeEvent(input$nextday, {
    removeModal() #remove summary page
    
    if (vals$gameEnded) {
      publishscore(vals$playername,vals$cash)
      showModal(leaderboardModal(vals$cash))
      
    }
    
    vals$day <- vals$day+1
    
    if (vals$day %in% c(4,9,12)){ 
      vals$movielist<-generatemlist(vals$day)
      vals$mobjs<-generateMobjs(vals$movielist)
    }
    
    if (vals$day ==2){ 
      showModal(day2modal())
    }
    if (vals$day ==4){ 
      showModal(day4modal())
    }
    if (vals$day ==6){ 
      showModal(day6modal())
    }
    if (vals$day ==8){ 
      showModal(day8modal())
    }
    
    if (vals$day ==9){ 
      showModal(day9modal())
      #remove scheduled mission possible
      runjs("clearday9()")
    }
    if (vals$day ==12){ 
      showModal(day12modal())
    }
    
  })
  
  #restart button
  observeEvent(input$restart, {
    updateActionButton(session, "nextday", label = "Next day >")
    removeModal() # Close the summary page modal
    
    runjs("cleartimetable()")
    
    #load initial conditions
    vals$cash<- 10000
    vals$movielist<-generatemlist(1)
    vals$day<-1
    vals$mobjs<-generateMobjs(vals$movielist) #generate movie list
    vals$resultsdf <- data.frame(
      Day = numeric(),
      AdRevenue = numeric(),
      TicketsRevenue = numeric(),
      RentalCost = numeric(),
      Profits = numeric(),
      stringsAsFactors = FALSE
    )
    vals$gameEnded<-FALSE
  })
  
  
  # utils Button
  observeEvent(input$utilsmodal, {
    removeModal()
    showModal(utilsModal())
  })
  
  observeEvent(input$downloadUtils,{
    write.csv(vals$utilsdf,"utils.csv")
  })
  
  # moviestats Button
  observeEvent(input$moviestats, {
    removeModal()
    showModal(statsModal())
  })
  
  observeEvent(input$downloadStats,{
    write.csv(vals$movieStatsdf,"movie_stats.csv")
  })
  
  
############### Output render ####################
  
  #generating random name for registration
  output$randomname <- renderText({
    paste(vals$randomName)
  })
  
  # Display name 
  output$loggedInAs <- renderUI({
    if (is.null(vals$playername)) {
      tagList(
        tags$p("Not logged in yet.", style = "text-align: center;padding:8px;")
      )
    } else {
      tagList(
        tags$p(vals$playername, style = "text-align: center;padding:8px;")
      )
    }
  })
  
  # Display cash 
  output$cash <- renderUI({
    tagList(
      tags$p(
        style = "text-align: center;padding:8px;",
        paste("Cash balance: $", vals$cash)
      )
    )
  })
  
  
  #Day counter
  output$day <- renderUI({
    actualdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    if (vals$day <= 14) {
      day_of_week <- actualdays[vals$day]
      tagList(
        tags$p(
          style = "text-align: center;padding:8px;",
          paste("Day", vals$day, "/14,", day_of_week)
        )
      )
    } else {
      tagList(
        tags$p(
          style = "text-align: center;padding:8px;",
          "Game Ended"
        )
      )
    }
  })
  
  #ticket prices
  output$ticketprices <- renderUI({
    tagList(
      tags$p(
        style = "text-align: center;padding:8px;",
        if (vals$day %in% c(6, 7, 12, 13, 14)) {
          paste("$", 10, "/ticket")
        } else {
          paste("$", 7, "/ticket")
        }
      )
    )
  })
  
  #movie objects
  output$movieobjects<- renderUI({vals$mobjs})
  
  #utilisation board
  output$utilisationboard <- renderTable({vals$cum_utils})
  
  #stats board
  output$statsboard <- renderTable({vals$cum_moviestats})
  
  #resultsboard
  output$resultboard <- renderTable({vals$cum_results})
  
  #leaderboard
  output$leaderboard <- renderTable({getLeaderboard()})}

##################### APP #####################
shinyApp(ui, server)

