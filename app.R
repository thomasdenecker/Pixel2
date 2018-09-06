################################################################################
# Pixel version 2
# Thomas DENECKER
# 09/2018
#
# GitHub :
# https://github.com/thomasdenecker/Pixel_V2
################################################################################

################################################################################
# Library
################################################################################

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(evobiR)
library(plotly)
library(ape)
library(RPostgreSQL)
library(shinyalert)
library(googleVis)
library(V8)
library(tools)
library(DT)
library(shinyWidgets)

################################################################################
# Admin adress
################################################################################

adminAdress = "thomas.denecker@gmail.com"

################################################################################
# Country list (from googleVis in population dataframe)
################################################################################

COUNTRY = as.list(sort(Population$Country))
names(COUNTRY) = sort(Population$Country)


################################################################################
# Database connection
################################################################################

ipDB = read.table("Database/ipDB.txt", header = F, stringsAsFactors = F)[1,1]


################################################################################
# UI
################################################################################

header <- dashboardHeader(title = "Pixel")
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(useShinyjs(), useShinyalert(),
                      tags$head(tags$link(href = "Images/pixel_icon.png",
                                          rel ="icon", type="image/png")),
                      
                      # Add css style
                      tags$head(HTML('<link rel="stylesheet" type="text/css"
                                     href="style.css" />')),
                      uiOutput("body"))
ui <- dashboardPage(skin= "red", header, sidebar, body)


################################################################################
# SERVER
################################################################################

server <- function(input, output, session) {
  
  
  
  USERS <- reactiveValues()
  
  login <- div( class="authenfication", 
                h2("Login"),
                div(class = "center_input",textInput("USER", "Username")),
                div(class = "center_input",passwordInput("PW", "Password")),
                br(),
                actionButton("Login", "Log in")
  )
  
  # To logout back to login page
  login.page = paste(
    isolate(session$clientData$url_protocol),
    "//",
    isolate(session$clientData$url_hostname),
    ":",
    isolate(session$clientData$url_port),
    sep = ""
  )
  
  USER <- reactiveValues(Logged = F)
  observeEvent(input$Login, {
    if (USER$Logged == FALSE) {
      
      Username <- isolate(input$USER)
      Password <- isolate(input$PW)
      
      REQUEST = paste0("SELECT * 
            FROM pixeler
            WHERE ( email = '",Username,"' or user_name = '",Username,"') 
            AND password = crypt('",Password,"', password);")
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      
      if(input$USER != "" & input$PW != ""){
        RESULT_REQUEST = dbGetQuery(con, REQUEST)
        
        if(nrow(RESULT_REQUEST) != 0 ){
          USER$Logged <- TRUE
          USER$infos <- RESULT_REQUEST[1,]
          USER$UserType <- RESULT_REQUEST[1,7]
          if (USER$UserType == "Admin"){
            REQUEST = "SELECT * FROM pixeler;"
            pg <- dbDriver("PostgreSQL")
            con <- dbConnect(pg, user="docker", password="docker",
                             host=ipDB, port=5432)
            USERS$infos = dbGetQuery(con, REQUEST)
            dbDisconnect(con)
          }
        } else {
          USER$Logged <- F
          shinyalert("Oops!", "Something went wrong (Username or old password).", type = "error")
        }
      }
      dbDisconnect(con)
    }
  })
  
  #=============================================================================
  # Sidebarpanel
  #=============================================================================
  
  output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE) {
      if(USER$UserType == "Admin"){
        div(
          sidebarUserPanel(
            isolate(input$USER),
            subtitle = a(icon("sign-out"), "Logout", href = login.page)
          ),
          
          sidebarMenu(id = "tabs",
            menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard"), selected = T),
            menuItem("Submissions", tabName = "Submissions", icon = icon("plus-circle")),
            menuItem("Pixel sets", tabName = "Pixel_sets", icon = icon("search")), 
            menuItem("Explorer", tabName = "Explorer", icon = icon("signal")),
            menuItem("Administration", tabName = "Administration", icon = icon("wrench"),
                     startExpanded = TRUE,
                     menuSubItem("Pixeler", tabName = "Pixeler"),
                     menuSubItem("Pixel", tabName = "Pixel")),
            menuItem("Profile", tabName = "Profile", icon = icon("user"))
          )
        )
      } else {
        div(
          sidebarUserPanel(
            isolate(input$USER),
            subtitle = a(icon("sign-out"), "Logout", href = login.page)
          ),
          sidebarMenu("tabMenu",
            menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard"), selected = T),
            menuItem("Pixel sets", tabName = "Pixel_sets", icon = icon("search")), 
            menuItem("Explorer", tabName = "Explorer", icon = icon("signal")),
            menuItem("Profile", tabName = "Profile", icon = icon("user"))
          )
        )
      }
      
    }
  })
  
  #=============================================================================
  # Body
  #=============================================================================
  
  
  output$body <- renderUI({
    if (USER$Logged == TRUE) {
      tabItems(
        # Tab content : Dashboard
        tabItem(
          tabName = "Dashboard", 
          h2("Dashboard"),
          fluidRow(
            
          )),
        
        # Tab content : Submissions
        tabItem(
          tabName = "Submissions", 
          h2("Submissions"),
          fluidRow(
            
          )),
        
        # Tab content : Pixel_sets
        tabItem(
          tabName = "Pixel_sets", 
          h2("Pixel sets"),
          fluidRow(
            
          )),
        
        # Tab content : Explorer
        tabItem(
          tabName = "Explorer", 
          h2("Explorer"),
          fluidRow(
            
          )),
        
        # Tab content : Explorer
        tabItem(
          tabName = "Pixeler", 
          h2("Pixeler"),
          div(class = "table_style", 
              h3("Modify pixeler database"),
              DTOutput('adminUsers'),
              br(),
              actionButton('removeUser', class = "pull-right",
                           label = "Remove user (0)", 
                           icon = icon("minus-circle")),
              br(),
              h3("New pixeler"),
              fluidRow(class= "tableTitle",
                       column(2, "User name"), 
                       column(2, "First name"),
                       column(2, "Last name"),
                       column(2, "Email"),
                       column(2, "Country"),
                       column(2, "User type")
              ),
              fluidRow(
                column(2,div(class = "inputNew",textInput("USERNAME_NU", NULL, placeholder = "Username"))),
                column(2,div(class = "inputNew",textInput("FN_NU", NULL, placeholder = "First name"))),
                column(2,div(class = "inputNew",textInput("LN_NU", NULL, placeholder = "Last name"))),
                column(2,div(class = "inputNew",textInput("EMAIL_NU", NULL, placeholder = "Email"))),
                column(2,div(class = "inputNew",selectInput("COUNTRY_NU", NULL, 
                                                            choices = COUNTRY, 
                                                            selected = "France"))),
                column(2,div(class = "inputNew",selectInput("UserType", NULL,
                                                            c("Admin" = "Admin",
                                                              "User" = "User"))))
              ),
              actionButton('addUser', class = "pull-right",
                           'Add pixeler', icon = icon("plus-circle"))
              
          )),
        # Tab content : Pixel_sets
        tabItem(
          tabName = "Profile", 
          h2("Profile"),
          fluidRow(
            
            
            
          ))
      )
      
    } else {
      login
    }
  })
  
  observe({
    if(USER$Logged == TRUE){
      updateTabItems (session, "tabs", selected = "Dashboard")
    }
  })
  
  #-----------------------------------------------------------------------------
  # Admin user table 
  #-----------------------------------------------------------------------------
  
  output$adminUsers <- renderDT(USERS$infos, selection = 'multiple', 
                                editable = TRUE,
                                options = list(scrollX = TRUE))
  proxy = dataTableProxy('adminUsers')
  
  
  #.............................................................................
  # Remove user
  #.............................................................................
  observe({
    s = input$adminUsers_rows_selected
    if (length(s) > 0) {
      updateActionButton(session, "removeUser", 
                         label = paste0('Remove user (',length(s),')'),
                         icon = icon("minus-circle"))
      enable("removeUser")
    } else {
      updateActionButton(session, "removeUser", 
                         label = 'Remove user (0)', icon = icon("minus-circle"))
      disable("removeUser")
    }
  })
  
  observeEvent(input$removeUser, {
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    for(line in input$adminUsers_rows_selected){
      REQUEST = paste0("DELETE FROM pixeler
      WHERE id=",USERS$infos[line, 1],";")
      dbGetQuery(con, REQUEST)
    }
    USERS$infos = USERS$infos[-input$adminUsers_rows_selected, ]
    dbDisconnect(con)
  })
  
  #.............................................................................
  # Edit user
  #.............................................................................
  
  observeEvent(input$adminUsers_cell_edit, {
    info = input$adminUsers_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    confirmSweetAlert(
      session = session,
      inputId = "confirm_modif_user",
      type = "warning",
      title = "Want to confirm ?",
      text = paste(USERS$infos[i, j], "->", v ),
      danger_mode = TRUE
    )
    
    observeEvent(input$confirm_modif_user, {
      if (isTRUE(input$confirm_modif_user)) {
        USERS$infos[i, j] <<- DT::coerceValue(v, USERS$infos[i, j])
        replaceData(proxy, USERS$infos, resetPaging = F)  # important
        
        # Replace in database
        if (colnames(USERS$infos)[j] == 'password'){
          REQUEST = paste0("UPDATE pixeler SET password = crypt('",
                           USERS$infos[i, j],"', password) WHERE id =",USERS$infos[i, 1],";")
        } else {
          REQUEST = paste0("UPDATE pixeler SET ",colnames(USERS$infos)[j] ," = '",
                           USERS$infos[i, j],"' WHERE id =",USERS$infos[i, 1],";")
        }
        
        pg <- dbDriver("PostgreSQL")
        con <- dbConnect(pg, user="docker", password="docker",
                         host=ipDB, port=5432)
        dbGetQuery(con, REQUEST)
        dbDisconnect(con)
        
      } else {
        USERS$infos[i, j] <<- DT::coerceValue(USERS$infos[i, j], USERS$infos[i, j])
        replaceData(proxy, USERS$infos, resetPaging = F)  # important
      }
    }, ignoreNULL = TRUE)
  })
  
  #.............................................................................
  # Add user
  #.............................................................................
  
  observeEvent(input$addUser, {
    REQUEST_EXISTING = paste0("SELECT *
                                FROM pixeler
                              WHERE email = '",input$EMAIL_NU,"' or user_name = '",input$USERNAME_NU,"';")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    
    if(nrow(dbGetQuery(con, REQUEST_EXISTING)) != 0 ){
      shinyalert("Oops!", "This user is already in the database", type = "error")
    } else {
      REQUESTE_ADD = paste0("INSERT INTO pixeler (first_name, last_name, user_name, email, UserType, Country, password) VALUES (
                            '",input$FN_NU, "',
                            '",input$LN_NU, "',
                            '",input$USERNAME_NU,"',
                            '",input$EMAIL_NU, "',
                            '",input$UserType, "',
                            '",input$COUNTRY_NU, "',
                            crypt('tempPW', gen_salt('bf'))
      );
                            ")
      dbGetQuery(con, REQUESTE_ADD)
      dbDisconnect(con)
      shinyalert("Nice!", "A new pixeler is in the database"
                 , type = "success")
      
      REQUEST = "SELECT * FROM pixeler;"
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      USERS$infos = dbGetQuery(con, REQUEST)
      dbDisconnect(con)
    }
  })
  

  observe({
    if(is.null(input$FN_NU) | is.null(input$LN_NU) |
       is.null(input$USERNAME_NU) | is.null(input$EMAIL_NU)){
     disable("addUser")
    }else{
      enable("addUser")
    }
  })
  
}

################################################################################
# APP
################################################################################

shinyApp(ui, server)

