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
  
  options(shiny.maxRequestSize=1000*1024^2)
  
  USERS <- reactiveValues()
  rv <- reactiveValues()
  
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
                      menuItem("Chromosomal features", tabName = "CF_item", icon = icon("search")), 
                      menuItem("Explorer", tabName = "Explorer", icon = icon("signal")),
                      menuItem("Administration", tabName = "Administration", icon = icon("wrench"),
                               startExpanded = F,
                               menuSubItem("Pixeler", tabName = "Pixeler"),
                               menuSubItem("Pixel", tabName = "Pixel"), 
                               menuSubItem("Annotation", tabName = "Annotation")),
                      
                      menuItem("Profile", tabName = "Profile", icon = icon("user")),
                      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search...")
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
                      menuItem("Profile", tabName = "Profile", icon = icon("user")),
                      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search...")
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
            valueBoxOutput("nbPixel", width = 3),
            valueBoxOutput("nbPixelSet", width = 3),
            valueBoxOutput("nbPixeler", width = 3),
            valueBoxOutput("DataEntries", width = 3)
          ),
          
          fluidRow(
            # box(title = "Pixels by OmicsUnitType", width = 6, htmlOutput("PixelsByOmicsUnitType") ),
            # box(title = "Pixels by Species", width = 6, htmlOutput("PixelsByOmicsUnitType"))
            # 
            
            div(class="col-sm-6",
                div(class="box box-primary",
                    div(class="box-body",
                        h2(class="center","Pixels by OmicsUnitType"), br(),
                        htmlOutput("PixelsByOmicsUnitType")
                    )
                )
            ),
            
            div(class="col-sm-6",
                div(class="box box-primary",
                    div(class="box-body",
                        h2(class="center","Pixels by Species"), br(),
                        htmlOutput("PixelsBySpecies")
                    )
                )
            )
            
          ),
          
          fluidRow(
            div(class="col-sm-6",
                div(class="box box-primary",
                    div(class="box-body",
                        h2(class="center","Pixeler localisation"), br(),
                        htmlOutput("UsersMap")
                    )
                )
            ),
            
            div(class="col-sm-6",
                div(class="box box-primary",
                    div(class="box-body",
                        h2(class="center","Pixeler informations"), br(),
                        DT::dataTableOutput('pixelerInfo')
                    )
                )
            )
          )
          
        ),
        
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
        
        # Tab content : Pixel_sets
        tabItem(
          tabName = "CF_item", 
          h2("Chromosomal feature"),
          fluidRow(
            uiOutput("title_cf"),
            div( class = "margeProfile", tabsetPanel(id = "tab_sup_annot"))
            
          )),
        
        # Tab content : Explorer
        tabItem(
          tabName = "Explorer", 
          h2("Explorer"),
          fluidRow(
            
          )),
        
        # Tab content : Annotation
        tabItem(
          tabName = "Annotation", 
          h2("Chromosomal feature"),
          h3("1- Annotation source"),
          fluidRow( class='border-between ', 
                    column(6,align="center",
                           h3("Existing Sources"),
                           uiOutput("SSUI"),
                           htmlOutput("DescriCFSource")
                    ),
                    column(6,align="center",
                           h3("Create new source"),
                           h4("Source name"),
                           textInput("CFSourceName",NULL ),
                           h4("abbreviation"),
                           textInput("CFAbbreviation",NULL ),
                           h4("Source description"),
                           textAreaInput("CFSourceDescription", NULL, resize = "vertical"),
                           h4("Source URL"),
                           textInput("CFSourceURL", NULL),
                           actionButton("addCFSource", "Add source")
                    )
          ),
          
          
          h3("2- Import annotation file"),
          fluidRow(
            column(6,align="center",
                   h4("Annotation type "),
                   selectInput("importTypeCF",NULL,
                               c("Main information" = "main",
                                 "Supplementary information" = "sup"))
            ),
            column(6, 
                   h4("Help "),
                   textOutput("helpImportTypeCF"),
                   br(),
                   uiOutput("Sup")
                   
            )
          ),
          
          fluidRow(
            column(3,
                   h3("Parameters"),
                   fileInput("fileCF",label = NULL,
                             buttonLabel = "Browse...",
                             placeholder = "No file selected"),align = "center",
                   tags$hr(),
                   
                   # Input: Checkbox if file has header
                   radioButtons("header_CF", "Header",
                                choices = c("Yes" = TRUE,
                                            "No" = FALSE),
                                selected = TRUE, inline=T),
                   
                   # Input: Select separator ----
                   radioButtons("sep_CF", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = "\t", inline=T),
                   
                   # Input: Select quotes ----
                   radioButtons("quote_CF", "Quote",
                                choices = c(None = "",
                                            "Double Quote" = '"',
                                            "Single Quote" = "'"),
                                selected = "", inline=T)
            ), 
            column(9, 
                   h3("Preview"),
                   dataTableOutput(outputId = "contents_CF"))
          ),
          actionButton(inputId = "ImportCF", label = "Import", class= "myBtn" , icon = icon("upload"))
          
        ),
        
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
            uiOutput("Profile")
          ))
      )
      
    } else {
      login
    }
  })
  
  
  #-----------------------------------------------------------------------------
  # Dashboard
  #-----------------------------------------------------------------------------
  
  output$nbPixelSet <- renderValueBox({
    valueBox(
      8, "Pixelsets", icon = icon("folder"),
      color = "red"
    )
  })
  
  output$pixelerInfo <- DT::renderDataTable(USERS$infos[,c(2:5, 7:8)], options = list(scrollX = TRUE))
  
  output$nbPixel <- renderValueBox({
    valueBox(
      35200, "Pixels", icon = icon("puzzle-piece"),
      color = "purple"
    )
  })
  
  output$nbPixeler <- renderValueBox({
    valueBox(
      paste0(nrow(USERS$infos)), "Pixelers", icon = icon("users"),
      color = "green"
    )
  })
  
  
  output$DataEntries <- renderValueBox({
    valueBox(
      21531, "Data entries", icon = icon("database"),
      color = "blue"
    )
  })
  
  
  # PixelsByOmicsUnitType PixelsBySpecies
  
  output$PixelsByOmicsUnitType <- renderGvis({
    data<-data.frame(c('mRNA','Protein'),c(15,85))
    gvisPieChart(data,options=list(tooltip = "{text:'percentage'}"))
  })
  
  output$PixelsBySpecies <- renderGvis({
    data<-data.frame(c('C. glabrata','S. cerevisiae'),c(25,75))
    gvisPieChart(data,options=list(tooltip = "{text:'percentage'}"))
  })
  
  output$UsersMap <- renderGvis({
    REQUEST_MAP = paste0("SELECT lab_country, count(*) FROM pixeler GROUP BY lab_country ;")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    MapTable = dbGetQuery(con, REQUEST_MAP)
    
    gvisGeoChart(MapTable, locationvar="lab_country", 
                 colorvar="count",
                 options=list(projection="kavrayskiy-vii",height= 350,
                              colorAxis="{colors:['#ffe6e6','#b30000'], minValue:1}"))
    
  })
  
  
  #-----------------------------------------------------------------------------
  # Log
  #-----------------------------------------------------------------------------
  
  observe({
    if(USER$Logged == TRUE){
      updateTabItems (session, "tabs", selected = "Dashboard")
    }
  })
  
  
  #-----------------------------------------------------------------------------
  # Profile
  #-----------------------------------------------------------------------------
  
  output$Profile <- renderUI({
    tagList(
      div( class = "margeProfile",
           h3("General information"),
           p(tags$b("First names :"), USER$infos[1,2]),
           p(tags$b("Last names :"), USER$infos[1,3]),
           p(tags$b("User names :"), USER$infos[1,4]),
           p(tags$b("Email :"), USER$infos[1,5]),
           p(tags$b("Password (encrypted) :"), USER$infos[1,6]),
           p(tags$b("User type :"), USER$infos[1,7]),
           p(tags$b("Country :"), USER$infos[1,8]),
           p(tags$b("Creation date :"), USER$infos[1,9]),
           
           h3("Change the password"),
           passwordInput("OldPW", "Old password", placeholder = "Your old password" ),
           passwordInput("NewPW1", "New password", placeholder = "Your new password" ),
           passwordInput("NewPW2", "New password (verification)", placeholder = "Retype your new password" ),
           actionButton("ModifyPW", "Modify your password")
      )
    )
  })
  
  observeEvent(input$ModifyPW, {
    
    if(input$NewPW1 == input$NewPW2 & input$NewPW1 != "" & input$NewPW2!=""){
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      
      Password <- isolate(input$OldPW)
      
      REQUEST = paste0("SELECT * 
                 FROM pixeler
                 WHERE ( email = '",USER$infos[1, 5],"' or user_name = '",USER$infos[1, 4],"') 
                 AND password = crypt('",Password,"', password);")
      
      RESULT_REQUEST = dbGetQuery(con, REQUEST)
      
      if(nrow(RESULT_REQUEST) != 0){
        USER$infos <- RESULT_REQUEST[1,]
        REQUEST = paste0("UPDATE pixeler SET password = crypt('",
                         input$NewPW2,"', password) WHERE user_name ='",USER$infos[1, 4],"';")
        dbGetQuery(con, REQUEST)
        
        updateTextInput(session, "OldPW", value="")
        updateTextInput(session, "NewPW1", value="")
        updateTextInput(session, "NewPW2", value="")
        
        shinyalert("Congratulation", "Your password has been successfully changed!", type = "success")
        
      } else {
        shinyalert("Oops!", "Your old password is not the right one!", type = "error")
      }
      
      dbDisconnect(con)
    } else {
      if(input$NewPW1 != input$NewPW2){
        shinyalert("Oops!", "The two new passwords are not the same!", type = "error")
      } else {
        shinyalert("Oops!", "A field has not been entered!", type = "error")
      }
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
      REQUESTE_ADD = paste0("INSERT INTO pixeler (first_name, last_name, user_name, email, user_type, lab_country, password) VALUES (
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
  
  #.............................................................................
  # Annotation
  #.............................................................................
  
  output$contents_CF <-  renderDataTable({
    
    req(input$fileCF)
    
    df <- read.csv(input$fileCF$datapath,
                   header = as.logical(input$header_CF),
                   sep = input$sep_CF,
                   quote = input$quote_CF,
                   nrows=10
    )
    
  },  options = list(scrollX = TRUE , dom = 't'))
  
  
  observeEvent(input$addCFSource,{
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    rv$ERROR = F
    
    REQUEST_ANNOT = paste0("INSERT INTO CFSource (name,abbreviation, description, url) VALUES ('",
                           input$CFSourceName, "','", input$CFAbbreviation, "','", input$CFSourceDescription,"','",input$CFSourceURL,"');" )
    
    
    tryCatch(dbSendQuery(con, REQUEST_ANNOT)
             , error = function(c) {
               shinyalert("Error when importing", 
                          paste0(c,"\n The chevron shows you where the error is."), 
                          className="alert",
                          type = "error")
               rv$ERROR = T
             },warning = function(c) {
               shinyalert("Error when importing", 
                          paste0(c,"\n The chevron shows you where the error is."), 
                          className="alert",
                          type = "error")
               rv$ERROR = T
             }
    )
    if(rv$ERROR == F){
      
      
      updateTextAreaInput(session,"CFSourceDescription", value = "")
      updateTextInput(session,"CFSourceName", value = "" )
      updateTextInput(session,"CFSourceURL", value = "" )
      
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      rv$Source = dbGetQuery(con, "select * from CFSource;")
      
      dbDisconnect(con)
      
      updateSelectInput(session, "selectSource", choices = rv$Source[,2], selected = input$CFSourceName)
      
      shinyalert("Congratulations!", 
                 "The import was successful!",
                 type = "success")
    }
    
  })
  
  observeEvent(input$ImportCF,{
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    database = read.csv(input$fileCF$datapath,
                        header = as.logical(input$header_CF),
                        sep = input$sep_CF,
                        quote = input$quote_CF,
                        stringsAsFactors = F
    )
    
    withProgress(message = 'Import in Database', value = 0, {
      if(input$importTypeCF == "main"){
        if(ncol(database) == 8){
          n <- nrow(database)
          rv$ERROR = F
          for(i in 1:nrow(database)){
            
            incProgress(1/n, detail = paste("Doing part", i))
            
            REQUEST_INDB = paste0("SELECT * from ChromosomalFeature WHERE feature_name = '",database[i,1],"'" );
            if(nrow(dbGetQuery(con, REQUEST_INDB)) != 0){
              REQUEST_ANNOT = paste0("UPDATE ChromosomalFeature SET gene_name = '",database[i,2],"', chromosome = '",database[i,3],
                                     "', start_coordinate = ",database[i,4],", stop_coordinate =",database[i,5],", strand ='",database[i,6],
                                     "',species_name ='",database[i,7], "', url ='",database[i,8] ,"',default_db_name ='",input$selectSource 
                                     ,"' WHERE Feature_name = '",database[i,1],"';" );
            } else {
              REQUEST_ANNOT = paste0("INSERT INTO ChromosomalFeature (feature_name , gene_name,  chromosome, start_coordinate, stop_coordinate, strand, species_name, url, default_db_name) VALUES ( ",paste(c(paste0("'",database[i,1:3],"'"), database[i,4:5], paste0("'",database[i,6:8],"'"),paste0("'",input$selectSource,"'")),collapse = ","),
                                     ");")
            }
            
            tryCatch(dbSendQuery(con, REQUEST_ANNOT)
                     , error = function(c) {
                       shinyalert("Error when importing", 
                                  paste0(c,"\n The error occurred on line ",i," of the table.The chevron shows you where the error is."), 
                                  className="alert",
                                  type = "error")
                       rv$ERROR = T
                     },warning = function(c) {
                       shinyalert("Error when importing", 
                                  paste0(c,"\n The error occurred on line ",i," of the table.The chevron shows you where the error is."), 
                                  className="alert",
                                  type = "error")
                       rv$ERROR = T
                     }
            )
            
            if(rv$ERROR == T){
              break()
            } 
            
          }
          
          if(rv$ERROR == F){
            shinyalert("Congratulations!", 
                       "The import was successful!",
                       type = "success")
          }
          
          
        } else {
          shinyalert(paste0("The table format is not correct. The number of columns is",ncol(database)," instead of 8."), type = "error")
        }
      } else {
        
        # TABLE CREATION
        newTableName <- input$sup_name
        columnNewTable <- gsub("[[:punct:]]", "",colnames(database)[-1])
        columnNewTable <- gsub(" ", "_",columnNewTable )
        
        REQUEST = paste("CREATE TABLE", newTableName, "(id SERIAL PRIMARY KEY, feature_name TEXT,",
                        paste(paste( columnNewTable, "TEXT"), collapse = ",")
                        ,", cfsource_name TEXT, CONSTRAINT fkcfsource FOREIGN KEY (cfsource_name) REFERENCES CFSource (name), CONSTRAINT fkCF FOREIGN KEY (feature_name) REFERENCES ChromosomalFeature (feature_name));")
        
        tryCatch(dbSendQuery(con, REQUEST)
                 , error = function(c) {
                   shinyalert("Error : Table creation", 
                              paste0(c,"\n The chevron shows you where the error is."), 
                              className="alert",
                              type = "error")
                   rv$ERROR = T
                   
                 },warning = function(c) {
                   shinyalert("Error : Table creation", 
                              paste0(c,"\n The chevron shows you where the error is."), 
                              className="alert",
                              type = "error")
                   rv$ERROR = T
                   
                 }
        )
        
        # insert in new table
        n <- nrow(database)
        rv$ERROR = F
        for(i in 1:nrow(database)){
          
          incProgress(1/n, detail = paste("Doing part", i))
          
          if(nrow(dbGetQuery(con,paste0("select * from chromosomalfeature where feature_name ='",gsub("\'"," Prime", database[i,1]),"';")))!=0){
            
            REQUEST_ANNOT = paste0("INSERT INTO ",newTableName," (feature_name ,",paste(columnNewTable, collapse = ","),",cfsource_name ) VALUES ( ", paste0("'", gsub("\'"," Prime", database[i,]),"'",collapse = ","),",'",input$selectSource,"');")
            
            tryCatch(dbSendQuery(con, REQUEST_ANNOT)
                     , error = function(c) {
                       shinyalert("Error when importing",
                                  paste0(c,"\n The error occurred on line ",i," of the table.The chevron shows you where the error is."),
                                  className="alert",
                                  type = "error")
                       rv$ERROR = T
                       rv$ERROR_ALL = T
                       rv$linesNotSaved = c(rv$linesNotSaved , i)
                       cat(REQUEST_ANNOT, file= stderr())
                     },warning = function(c) {
                       shinyalert("Error when importing",
                                  paste0(c,"\n The error occurred on line ",i," of the table.The chevron shows you where the error is."),
                                  className="alert",
                                  type = "error")
                       rv$ERROR = T
                       rv$ERROR_ALL = T
                       rv$linesNotSaved = c(rv$linesNotSaved , i)
                       cat(REQUEST_ANNOT, file= stderr())
                     }
            )
            
            REQUEST_ANNOT = paste0("INSERT INTO annotation (feature_name , annot_table) VALUES ('",gsub("\'"," Prime", database[i,1]), "','",newTableName, "');")
            
            dbSendQuery(con, REQUEST_ANNOT)
            
          }
        }
        
        
        if(rv$ERROR == F){
          shinyalert("Congratulations!",
                     "The import was successful!",
                     type = "success")
        }
      }  
    })
    
  })
  
  
  output$helpImportTypeCF <- renderText({ 
    if(input$importTypeCF == "main"){
      "By selecting 'Main information', you choose to enter new chromosomal 
      features or update existing ones. The table must be composed of 7 columns: feature name (i.e : YAL068C), gene name (i.e PAU8), chromosome, start coordinate, stop coordinate, 
      url (ie. for SGD it's ' https://www.yeastgenome.org/locus/' + SGD id : 'https://www.yeastgenome.org/locus/S000002142').The selected source will be added as the default database. "
    } else if (input$importTypeCF == "sup"){
      "By selecting 'Supplementary information', you choose to import additional information. "
    }
  })
  
  
  #.............................................................................
  # Source 
  #.............................................................................
  
  output$SSUI = renderUI({
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    rv$Source = dbGetQuery(con, "select * from CFSource;")
    
    dbDisconnect(con)
    selectInput('selectSource', NULL, rv$Source[,2])
  })
  
  output$DescriCFSource <- renderText({
    rv$Source[which(rv$Source[, 2] == input$selectSource), 4]
  })
  
  
  output$Sup = renderUI({
    if(input$importTypeCF == "sup"){
      textInput("sup_name", "Name of table")
    }
  })
  
  
  
  CF = reactiveValues()
  
  observeEvent(input$searchButton,{
    updateTabItems (session, "tabs", selected = "CF_item")
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    CF$main_annotation = dbGetQuery(con, paste0("select * from chromosomalfeature where feature_name ='",input$searchText,"';"))
    CF$main_annotation[1, 'url'] = paste0("<a href='",CF$main_annotation[1, 'url'] ,"'  target='_blank'>",CF$main_annotation[1, 'url'], "</a>")
    CF$main_annotation = paste("<b>", gsub("_", " " ,colnames(CF$main_annotation)[-1]),"</b> : ", CF$main_annotation[1,-1])
    CF$main_annotation = paste(CF$main_annotation, collapse = "<br>")
    
    CF$name = input$searchText
    
    Sup_tab = dbGetQuery(con, paste0("select annot_table from annotation where feature_name ='",input$searchText,"';"))
    
    
    for(i in 1:nrow(Sup_tab)){
      CF$sup_annot = c(CF$sup_annot,
                       paste( unlist(dbGetQuery(con, paste0("select * from ",Sup_tab[i,1]," where feature_name ='",input$searchText,"';"))), collapse='   ')
      )
      
      result = paste( unlist(dbGetQuery(con, paste0("select * from ",Sup_tab[i,1]," where feature_name ='",input$searchText,"';"))), collapse='   ')
      if(i == 1){
        appendTab("tab_sup_annot", tabPanel(Sup_tab[i,1], result),select = T)
      } else {
        appendTab("tab_sup_annot", tabPanel(Sup_tab[i,1], result),select = F)
      }
    }
    
    
  })
  
  
  output$title_cf <- renderUI(
    tagList(
      div( class = "margeProfile",
           h1(CF$name),
           h2("Main information"),
           HTML(CF$main_annotation),
           h2("Supplementary information")
      ))
  )
  
  
  # output$dynamicTabs <- renderUI({
  #   tabs <- lapply(CF$test,function(x){
  #     tabPanel(
  #       title = paste0(CF$test,'_Tab')
  #       ,h3(paste0('Tab showing:',CF$test))
  #       ,textOutput(CF$name) ####### Comment this line and input$selectedTab outputs correctly
  #       ,value=CF$test
  #     )
  #   })
  #   do.call(tabsetPanel,c(tabs,id='selectedTab'))
  # })
  
  # output$CF_informations = renderUI({
  #   pg <- dbDriver("PostgreSQL")
  #   con <- dbConnect(pg, user="docker", password="docker",
  #                    host=ipDB, port=5432)
  #   on.exit(dbDisconnect(con))
  #   
  #   rv$Source = dbGetQuery(con, "select * from CFSource;")
  #   Sup_tab = dbGetQuery(con, paste0("select annot_table from annotation where feature_name ='",input$searchText,"';"))
  #   
  #   
  #   dbDisconnect(con)
  #   
  #   h1(input$searchText)
  #   tabsetPanel(type = "tabs",
  #               tabPanel(paste('toto_',1 ), p(input$searchText)))
  #   
  #   for(i in 1:nrow(Sup_tab)){
  #         appendTab(inputId = "tabs",
  #                   tabPanel(paste('toto_',i ), p(paste('toto_',i )))
  #         )
  #       }
  # })
  # 
  
  
  
  # output$CF_informations <- renderUI({ 
  #   
  #   pg <- dbDriver("PostgreSQL")
  #   con <- dbConnect(pg, user="docker", password="docker",
  #                    host=ipDB, port=5432)
  #   on.exit(dbDisconnect(con))
  #   
  #   h1(rv$gene)
  #   # 
  #   # 
  #   # Sup_tab = dbGetQuery(con, paste0("select annot_table from annotation where feature_name ='",input$searchText,"';"))
  #   # # YAL043C
  #   # 
  #   # 
  #   # tabsetPanel(type = "tabs")
  #   # 
  #   # for(i in 1:nrow(Sup_tab)){
  #   #   appendTab(inputId = "tabs",
  #   #             tabPanel(paste('toto_',i ), p(paste('toto_',i )))
  #   #   )
  #   # }
  #   
  #   # Sup_tab[i,1]
  #   
  #   # p(paste( unlist(dbGetQuery(con, paste0("select * from ",Sup_tab[i,1]," where feature_name ='",input$searchText,"';"))), collapse='   '))
  #   # sup_annot = NULL
  #   
  #   # sup_annot = c(sup_annot ,
  #   #               
  #   # )
  #   # 
  #   # cat(paste( unlist(dbGetQuery(con, paste0("select * from ",Sup_tab[i,1]," where feature_name ='",input$searchText,"';"))), collapse='   '), file = stderr())
  #   # 
  #   
  #   
  #   dbDisconnect(con)
  #   
  #   
  #   
  #   # paste(sup_annot, collapse = "<br> ")
  # })
  
  
}

################################################################################
# APP
################################################################################

shinyApp(ui, server)

