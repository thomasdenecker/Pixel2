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
library(xlsx)
library(UpSetR)

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
body <- dashboardBody(useShinyjs(), 
                      useShinyalert(),
                      tags$head(tags$link(href = "Images/pixel_icon.png",
                                          rel ="icon", type="image/png")),
                      
                      # Add css style
                      tags$head(HTML('<link rel="stylesheet" type="text/css"
                                     href="style.css" />'))
                      ,
                      uiOutput("body"))
ui <- dashboardPage(skin= "red", header, sidebar, body)


################################################################################
# SERVER
################################################################################

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=1000*1024^2)
  
  #=============================================================================
  # Reactive values 
  #=============================================================================
  USERS <- reactiveValues()
  rv <- reactiveValues()
  SEARCH_RV = reactiveValues()
  
  #=============================================================================
  # END Reactive values 
  #=============================================================================
  
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
            WHERE user_name = '",Username,"'
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
          sendSweetAlert(
            session = session,
            title = "Oops!",
            text = "Something went wrong (Username or old password).",
            type = "error"
          )
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
                      menuItem("Submissions", tabName = "Submissions", icon = icon("upload")),
                      menuItem("Explorer", tabName = "Explorer", icon = icon("search"),
                               startExpanded = F,
                               menuSubItem("Chromosomal feature", tabName = "CF_item"),
                               menuSubItem("Tags", tabName = "Tags")
                      ),
                      menuItem("PixelSets", tabName = "PixelSets", icon = icon("folder"),
                               startExpanded = F,
                               menuSubItem("A PixelSet", tabName = "PixelSet"),
                               menuSubItem("PixelSet list", tabName = "PixelSetList"),
                               menuSubItem("Exploration of Multi PixelSets", tabName = "PixelSetExplo")
                      ),
                      menuItem("Add information", tabName = "Administration", icon = icon("plus-circle"),
                               startExpanded = F,
                               menuSubItem("Chromosomal feature", tabName = "Annotation"),
                               menuSubItem("Omics unit type", tabName = "AddOUT"), 
                               menuSubItem("Data source", tabName = "AddDataSource"),
                               menuSubItem("Omics area", tabName = "AddOmicsArea"),
                               menuSubItem("Species & strains", tabName = "AddSpecies")
                      ),
                      
                      menuItem("Administration", tabName = "Administration", icon = icon("wrench"),
                               startExpanded = F,
                               menuSubItem("Pixeler", tabName = "Pixeler"),
                               menuSubItem("Pixel", tabName = "Pixel"), 
                               menuSubItem("Submissions", tabName = "SubmissionsAdmin")),
                      menuItem("Profile", tabName = "Profile", icon = icon("user")),
                      h4(class ='sideBar',"Quick search"),
                      tags$hr(class= "sideBar"),
                      h5(class= "sideBar", "Chromosomal feature"),
                      sidebarSearchForm(textId = "searchCF", buttonId = "searchButtonCF",label = "CAGL0A01243g..."),
                      h5(class= "sideBar", "Tag"),
                      sidebarSearchForm(textId = "searchTag", buttonId = "searchButtonTag",label = "Limma"),
                      h5(class= "sideBar", "PixelSet"),
                      sidebarSearchForm(textId = "searchPS", buttonId = "searchButtonPS",label = "PixelSet_2018-10-22_09-36-37_1...")
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
                      menuItem("PixelSet", tabName = "PixelSet", icon = icon("search")), 
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
        ########################################################################
        # Tab content : Dashboard
        ########################################################################
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
          ),
          
          
          fluidRow(
            div(class="col-sm-12",
                div(class="box box-primary",
                    div(class="box-body",
                        h2(class="center","Last PixelSet imported"), br(),
                        DT::dataTableOutput('PixelSetInfo')
                    )
                )
            )
          )
          
        ),
        ########################################################################
        # Tab content : Submissions
        ########################################################################
        tabItem(
          tabName = "Submissions", 
          
          h2("Submissions"),
          fluidRow( class="box-submission",
                    box( 
                      title = "1- Experiment", status = "danger", solidHeader = TRUE,
                      collapsible = TRUE,
                      p(class="info","This section describes the experimental conditions that were applied to obtain the secondary datafile (see section 'Analysis' below). Note that these experiments can be already published (in this situation a DOI is required) or not (in this situation a laboratory has to be specified)."),
                      h4("Description"),
                      textAreaInput("submission_Exp_description", rows = 5, resize = "vertical", label = NULL),
                      h4("Completion date"),
                      selectInput(
                        inputId = "submission_Exp_completionDate",
                        label = NULL,
                        2000:as.integer(format(Sys.Date(), "%Y"))
                      ),
                      h4('Omics area'),
                      uiOutput("Submission_Exp_omicsArea"),
                      h4("Omics unit type"),
                      uiOutput("submission_pixelSet_OUT_UI"),
                      h4('Data source'),
                      uiOutput("Submission_Exp_dataSource"),
                      h4('Species'),
                      uiOutput("Submission_Exp_Species"),
                      
                      h4('Strain'),
                      uiOutput("Submission_Exp_Strain"),
                      
                      div(class="inline", h4("Tag ")),
                      div(class="inline tag-inline",dropdownButton(
                        h4("Add new tag"),
                        textInput("Submission_tags_NewName", NULL, placeholder = "Name"),
                        textInput("Submission_tags_NewDescription", NULL, placeholder = "Description"),
                        actionButton("Submission_Exp_tags_Newbtn", "Add tag"), size = "xs",
                        circle = TRUE, status = "danger", icon = icon("plus"), width = "300px",
                        tooltip = tooltipOptions(title = "Add new tag")
                      )),
                      tags$br(class="clearBoth"),
                      uiOutput("Submission_Exp_tags")
                      
                    ),
                    
                    
                    box( 
                      title = "2- Analysis", status = "danger", solidHeader = TRUE, 
                      collapsible = TRUE,
                      
                      p(class = "info","This section describes the data analyses that were performed on secondary datasets to obtain pixel datasets. The secondary datafile has to be associated to the pixel datasets during the import process."),
                      
                      h4("Description"),
                      textAreaInput("submission_Analysis_description", rows = 5, resize = "vertical", label = NULL),
                      
                      h4("Completion date"),
                      selectInput(
                        inputId = "submission_Analysis_completionDate",
                        label = NULL,
                        2000:as.integer(format(Sys.Date(), "%Y"))
                      ),
                      
                      h4('Notebook'),
                      fileInput("submission_Analysis_notebook",label = NULL,
                                buttonLabel = "Browse...",
                                placeholder = "No file selected"),
                      
                      h4('Secondary data file'),
                      fileInput("submission_Analysis_secondary_data",label = NULL,
                                buttonLabel = "Browse...",
                                placeholder = "No file selected"),
                      
                      div(class="inline", h4("Tag ")),
                      div(class="inline tag-inline",dropdownButton(
                        h4("Add new tag"),
                        textInput("Submission_tags_NewName_analysis", NULL, placeholder = "Name"),
                        textInput("Submission_tags_NewDescription_analysis", NULL, placeholder = "Description"),
                        actionButton("Submission_Exp_tags_Newbtn_analysis", "Add tag"), size = "xs",
                        circle = TRUE, status = "danger", icon = icon("plus"), width = "300px",
                        tooltip = tooltipOptions(title = "Add new tag")
                      )),
                      tags$br(class="clearBoth"),
                      
                      uiOutput("Submission_Analysis_tags")
                    )
          ),
          fluidRow(class="box-submission",
                   box( 
                     title = "3- Pixel data sets", status = "danger", solidHeader = TRUE,
                     collapsible = TRUE,width = 12,
                     p(class="info","This section lists and describes each pixel datasets to be imported in the system. These files have to be associated to the secondary datafile (and the notebook datafile if available) during the import process. A specific comment can be added for each set of Pixel to better describe their differences."),
                     selectInput(
                       inputId = "submission_pixelSet_nbr",
                       label = NULL,
                       1:10
                     ),
                     
                     tabsetPanel(id = "tab_PixelSets"),
                     h4("Reading settings"),
                     fluidRow(
                       column(3,
                              h5("Parameters"),
                              # Input: Checkbox if file has header
                              radioButtons("header_PS", "Header",
                                           choices = c("Yes" = TRUE,
                                                       "No" = FALSE),
                                           selected = TRUE, inline=T),
                              
                              # Input: Select separator ----
                              radioButtons("sep_PS", "Separator",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t",
                                                       Space = " "),
                                           selected = "\t", inline=T),
                              
                              # Input: Select quotes ----
                              radioButtons("quote_PS", "Quote",
                                           choices = c(None = "",
                                                       "Double Quote" = '"',
                                                       "Single Quote" = "'"),
                                           selected = "", inline=T)
                       ), 
                       column(9, 
                              h5("Preview"),
                              dataTableOutput(outputId = "contents_PS"))
                     )
                   )
          ),
          
          fluidRow(class="box-submission",
                   box( 
                     title = "4- Submission", status = "danger", solidHeader = TRUE,
                     collapsible = TRUE,width = 12,
                     actionButton("SubmissionClear", "Clear"),
                     actionButton("Submission", "Submission")
                     
                   )
          )
          
        ),
        ########################################################################
        # Tab content : PixelSet
        ########################################################################
        tabItem(
          tabName = "PixelSet", 
          h2("PixelSet"),
          div( class = "PixelSet",
               fluidRow(
                 
                 column(6,
                        fluidRow(
                          h3("Properties"),
                          htmlOutput("PixelSet_explo")
                        ),
                        fluidRow(
                          h3("Tags"),
                          h4("Analysis"), 
                          uiOutput("PS_Tag_analysis_UI"),
                          h4("Experiment"),
                          uiOutput("PS_Tag_experiment_UI")
                        )
                 ),
                 column(6,
                        fluidRow(div(class= "search",h3("Graphical representations"))), 
                        fluidRow(
                          htmlOutput("PixelSetHistoValue")
                        ), 
                        fluidRow(
                          htmlOutput("PixelSetHistoQS")
                        ),
                        fluidRow(div(class= "search",
                                     h3("Search gene list"),
                                     p("To search a list of genes, separate them by';' To return to the complete list of genes, click on the clear button."),
                                     tags$textarea(id = "PS_searchGenelist", rows = 5),
                                     actionButton("PS_searchGenelist_clear_btn",label = "Clear"),
                                     actionButton("PS_searchGenelist_btn",label = "Search")
                        )
                        
                        )
                 )
               ),
               fluidRow(
                 h3("Pixels"),
                 DTOutput("PixelSet_explo_Pixel"))
          )),
        ########################################################################
        # Tab content : PixelSetList
        ########################################################################
        tabItem(
          tabName = "PixelSetList", 
          h2("PixelSet List"),
          fluidRow(
            column(12,
                   h3(class="title-pixelset","Selection"),
                   verbatimTextOutput('PixelSetRowSelected'),
                   actionButton(class="pull-right PS-btn","PixelSetExploreBtn", "Explore"),
                   actionButton(class="pull-right PS-btn","PixelSetExploreSelectAll", "Select all"),
                   actionButton(class="pull-right PS-btn", "PixelSetExploreDeselectAll", "Deselect all")
            )
          ),
          fluidRow(
            column(12,
                   h3(class="title-pixelset","Selection by tag"),
                   uiOutput("PixelSetTags")
            )
          ),
          fluidRow(
            column(12,
                   
                   DTOutput("PIXELSETLIST_tab")
            )
          )
        ),
        ########################################################################
        # Tab content : PixelSet Exploration
        ########################################################################
        tabItem(
          tabName = "PixelSetExplo", 
          h2("Exploration of Multi PixelSets"),
          fluidRow(
            column(12,
                   h3("Content of multi PixelSets",  class= "title-pixelset"),
                   DTOutput("PSExploContent"),
                   h3("Distribution",  class= "title-pixelset"),
                   uiOutput("PSExploUI")      
            )),
          fluidRow(
            column(12,
                   h3("Upset R",  class= "title-pixelset"),
                   plotOutput("UpsetR")
            )
          ),
          fluidRow(
            column(12,
                   h3("Search gene list",  class= "title-pixelset"),
                   p("To search a list of genes, separate them by';' To return to the complete list of genes, click on the clear button."),
                   tags$textarea(id = "MPS_searchGenelist", rows = 5),
                   actionButton("MPS_searchGenelist_clear_btn",label = "Clear"),
                   actionButton("MPS_searchGenelist_btn",label = "Search")
            )
          ),
          fluidRow(
            column(12, 
                   h3("Pixel",  class= "title-pixelset"),
                   downloadButton('MPS_export_csv', 'CSV'),
                   downloadButton('MPS_export_tsv', 'TSV'),
                   downloadButton('MPS_export_excel', 'EXCEL'),
                   br(),br(),
                   DTOutput("PSExploTab")
            )
          )
          
        ),
        ########################################################################
        # Tab content : Tags
        ########################################################################
        tabItem(
          tabName = "Tags", 
          h2("Tags"),
          htmlOutput("tagName"),
          div( class = "margeProfile",
               fluidRow(
                 h3("Analysis with this tag"),
                 DTOutput("Tag_analysis"),
                 h3("Experiment with this tag"),
                 DTOutput("Tag_experiment")
               ))),
        
        ########################################################################
        # Tab content : Chromosomal feature
        ########################################################################
        tabItem(
          tabName = "CF_item", 
          uiOutput("CF_title"),
          fluidRow(
            div(class="col-md-6",
                uiOutput("CF_information")
            ),
            div(class="col-md-6",
                h2(class="title-cf","Supplementary information"),
                tabsetPanel(id = "tab_sup_annot")
            )
          ),
          
          h2(class="title-cf", "Graphical representations"),
          fluidRow(
            div(class="col-md-6",
                h3(class="center","Omics Unit type"), br(),
                htmlOutput("CF_OUT_graph")
            ),
            div(class="col-md-6",
                h3(class="center","Omics Area"), br(),
                htmlOutput("CF_OmicsArea_graph")
            )
          ),
          
          fluidRow(
            div(class="col-md-6",
                h3(class="center","Analysis tags"), br(),
                htmlOutput("CF_Tag_Analysis_graph")
            ),
            div(class="col-md-6",
                h3(class="center","Experiment tags"), br(),
                htmlOutput("CF_Tag_Exp_graph")
            )
          ),
          
          
          fluidRow(
            div( class = "margeProfile", 
                 h3(class ="title-cf","PixelSets"),
                 DTOutput("CF_PixelSET"),
                 h3(class="title-cf","Pixel"),
                 DTOutput("CF_Pixel"),
                 h3(class="title-cf","Tags"),
                 fluidRow(
                   div(class="col-md-6",
                       h4("Analysis"), 
                       DTOutput("CF_Tag_analysis")
                   ),
                   div(class="col-md-6",
                       h4("Experiment"),
                       DTOutput("CF_Tag_experiment")
                   )
                 )
            )
          )),
        
        # Tab content : Add omicsUnitType
        tabItem(
          tabName = "AddOUT", 
          h2("Omics unit type"),
          fluidRow(
            div(class = "table_style",
                h3("Add Omics unit type"),
                fluidRow(class= "tableTitle",
                         column(2, "Name"), 
                         column(2, "Description"), 
                         column(8, "")
                ),
                fluidRow(
                  column(2,div(class = "inputNew",textInput("Name_OUT", NULL, placeholder = "Name"))),
                  column(2,div(class = "inputNew",textInput("Description_OUT", NULL, placeholder = "Description"))),
                  column(8,div(class = "inputNew",actionButton('addOUT_btn','Add OmicsUnitType', icon = icon("plus-circle"))))
                ),
                h3("Modify Omics unit type"),
                DTOutput('DT_AddOUT'))
            
          )),
        
        # Tab content : Add dataSource
        tabItem(
          tabName = "AddDataSource", 
          h2("Data source"),
          fluidRow(
            div(class = "table_style",
                h3("Add data source"),
                fluidRow(class= "tableTitle",
                         column(2, "Name"), 
                         column(2, "Description"), 
                         column(2, "published"),
                         column(2, "URL"),
                         column(4, "")
                ),
                fluidRow(
                  column(2,div(class = "inputNew",textInput("Name_DataSource", NULL, placeholder = "Name"))),
                  column(2,div(class = "inputNew",textInput("Description_DataSource", NULL, placeholder = "Description"))),
                  column(2,div(class = "inputNew",selectInput("Published_DataSource", NULL,
                                                              c("TRUE" = "TRUE",
                                                                "FALSE" = "FALSE")))),
                  column(2,div(class = "inputNew",textInput("URL_DataSource", NULL, placeholder = "URL"))),
                  column(4,div(class = "inputNew",actionButton('addDataSource_btn','Add DataSource', icon = icon("plus-circle"))))
                ),
                
                h3("Modify Omics unit type"),
                DTOutput('DT_AddDataSource'))
            
          )),
        
        # Tab content : Add dataSource
        tabItem(
          tabName = "AddOmicsArea", 
          h2("Omics area"),
          fluidRow(
            div(class = "table_style",
                
                
                
                h3("Add Omics area"),
                fluidRow(class= "tableTitle-left",
                         column(3, "Name"), 
                         column(3, "path"), 
                         column(3, "Description"),
                         column(3, "")
                ),
                fluidRow(
                  column(3,div(class = "inputNew",textInput("Add_OmicsArea_name", NULL, placeholder = "Name"))),
                  column(3,div(class = "inputNew",uiOutput("Add_OmicsArea_path"))),
                  column(3,div(class = "inputNew",textInput("Add_OmicsArea_description", NULL, placeholder = "Description"))),
                  column(3,div(class = "inputNew",actionButton('Add_OmicsArea_btn','Add OmicsArea', icon = icon("plus-circle"))))
                ),
                
                h3("Modify Omics area"),
                fluidRow(class= "tableTitle-left",
                         column(3, "Name"), 
                         column(3, "Path"), 
                         column(3, "Description"),
                         column(3, "")
                ),
                
                fluidRow(
                  column(3,div(class = "inputNew",uiOutput("Modify_OmicsArea_name"))),
                  column(3,div(class = "inputNew",uiOutput("Modify_OmicsArea_path"))),
                  column(3,div(class = "inputNew",uiOutput("Modify_OmicsArea_description") )),
                  column(3,div(class = "inputNew",actionButton('Modify_OmicsArea_btn','Modify OmicsArea', icon = icon("pen"))))
                  
                ) ,
                h3("Delete branch Omics area"),
                fluidRow(class= "tableTitle-left",
                         column(3, "Name"), 
                         column(3, ""), 
                         column(3, ""),
                         column(3, "")
                ),
                fluidRow(
                  column(3,div(class = "inputNew",uiOutput("Delete_branch_OmicsArea"))),
                  column(3,div(class = "inputNew",actionButton('Delete_branch_OmicsArea_btn','Modify OmicsArea', icon = icon("minus")))),
                  column(3,""),
                  column(3,"")
                ) ,
                
                div(class= "tree",
                    h3(class = 'center', "Organisation of omicsArea"),
                    htmlOutput("treeOmicsArea")
                )
                
                
            )
          )),
        
        # Delete_OmicsArea_btn
        #   Delete_branch_OmicsArea
        
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
        
        
        # Tab content : Species & strains
        tabItem(
          tabName = "AddSpecies",
          h2("Species"),
          fluidRow(
            div(class = "table_style",
                h3("Add new species"),
                fluidRow(class= "tableTitle",
                         column(2, "Name"),
                         column(2, "Description"),
                         column(2, "url"),
                         column(6, "")
                ),
                fluidRow(
                  column(2,div(class = "inputNew",textInput("Name_Species", NULL, placeholder = "Name"))),
                  column(2,div(class = "inputNew",textInput("Description_Species", NULL, placeholder = "Description"))),
                  column(2,div(class = "inputNew",textInput("URL_Species", NULL, placeholder = "url"))),
                  column(6,div(class = "inputNew",actionButton('addSpecies_btn','Add species', icon = icon("plus-circle"))))
                ),
                h3("Modify species"),
                DTOutput('DT_AddSpecies'))
            
          ),
          
          h2("Strains"),
          fluidRow(
            div(class = "table_style",
                h3("Add strain"),
                fluidRow(class= "tableTitle",
                         column(2, "Name"),
                         column(2, "Description"),
                         column(2, "Reference"),
                         column(2, "Species"),
                         column(4, "")
                ),
                fluidRow(
                  column(2,div(class = "inputNew",textInput("Name_Strain", NULL, placeholder = "Name"))),
                  column(2,div(class = "inputNew",textInput("Description_Strain", NULL, placeholder = "Description"))),
                  column(2,div(class = "inputNew",textInput("Ref_Strain", NULL, placeholder = "Reference"))),
                  column(2,div(class = "inputNew",uiOutput("Species_Strain"))),
                  column(4,div(class = "inputNew",actionButton('addStrain_btn','Add strain', icon = icon("plus-circle"))))
                ),
                h3("Modify strain"),
                DTOutput('DT_AddStrain'))
            
          )
          
          
          
          
        ),
        
        # Tab content : Pixeler
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
                                                              "Pixeler" = "Pixeler", 
                                                              "User" = "User"))))
              ),
              actionButton('addUser', class = "pull-right",
                           'Add pixeler', icon = icon("plus-circle"))
              
          )),
        
        ########################################################################
        # Tab content : Profile
        ########################################################################
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
  
  
  #=============================================================================
  # Dashboard
  #=============================================================================
  DASHBOARD_RV = reactiveValues()
  
  pg <- dbDriver("PostgreSQL")
  con <- dbConnect(pg, user="docker", password="docker",
                   host=ipDB, port=5432)
  on.exit(dbDisconnect(con))
  DASHBOARD_RV$PIXELSET = dbGetQuery(con,"SELECT count(*) from pixelset;")[1,1]
  DASHBOARD_RV$PIXEL = dbGetQuery(con,"SELECT count(*) from pixel;")[1,1]
  DASHBOARD_RV$CF = dbGetQuery(con,"SELECT count(*) from chromosomalfeature;")[1,1]
  DASHBOARD_RV$PixelSetTABLE = dbGetQuery(con,"SELECT * FROM pixelset order by id DESC LIMIT 10;")
  DASHBOARD_RV$OUT = dbGetQuery(con, "SELECT OUT.name, count(*) FROM Pixel, omicsunittype OUT WHERE out.id = omicsunittype_id group by OUT.name;")
  DASHBOARD_RV$Species = dbGetQuery(con, "SELECT species.name, count(*) FROM pixel, chromosomalfeature CF, species WHERE cf_feature_name = feature_name and CF.species_id = species.id group by species.name;")
  dbDisconnect(con)
  
  
  output$nbPixelSet <- renderValueBox({
    valueBox(
      DASHBOARD_RV$PIXELSET, "Pixelsets", icon = icon("folder"),
      color = "red"
    )
  })
  
  
  output$nbPixel <- renderValueBox({
    valueBox(
      DASHBOARD_RV$PIXEL, "Pixels", icon = icon("puzzle-piece"),
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
      DASHBOARD_RV$CF, "Chromosomal features", icon = icon("database"),
      color = "blue"
    )
  })
  
  output$pixelerInfo <- DT::renderDataTable(USERS$infos[,c(2:5, 7:8)],
                                            selection = 'none',
                                            options = list(scrollX = TRUE, pageLength = 5))
  
  output$PixelSetInfo <- DT::renderDataTable(DASHBOARD_RV$PixelSetTABLE,
                                             selection = 'single',
                                             options = list(scrollX = TRUE, pageLength = 10))
  
  observeEvent(input$PixelSetInfo_rows_selected,{
    SEARCH_RV$PIXELSET = DASHBOARD_RV$PixelSetTABLE[input$PixelSetInfo_rows_selected,"id"]
    proxy = dataTableProxy('PixelSetInfo')
    proxy %>% selectRows(NULL)
  })
  
  output$PixelsByOmicsUnitType <- renderGvis({
    gvisPieChart(DASHBOARD_RV$OUT,options=list(tooltip = "{text:'percentage'}"))
  })
  
  output$PixelsBySpecies <- renderGvis({
    gvisPieChart(DASHBOARD_RV$Species,options=list(tooltip = "{text:'percentage'}"))
  })
  
  output$UsersMap <- renderGvis({
    REQUEST_MAP = "SELECT lab_country, count(*) FROM pixeler GROUP BY lab_country ;"
    
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
  
  #=============================================================================
  # END Dashboard
  #=============================================================================
  
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
        
        sendSweetAlert(
          session = session,
          title = "Congratulation",
          text = "Your password has been successfully changed!",
          type = "success"
        )
        
      } else {
        sendSweetAlert(
          session = session,
          title = "Oops!",
          text = "Your old password is not the right one!",
          type = "error"
        )
      }
      
      dbDisconnect(con)
    } else {
      if(input$NewPW1 != input$NewPW2){
        
        sendSweetAlert(
          session = session,
          title = "Oops!",
          text = "The two new passwords are not the same!",
          type = "error"
        )
      
      } else {
        sendSweetAlert(
          session = session,
          title = "Oops!",
          text = "A field has not been entered!",
          type = "error"
        )
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
      sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "This user is already in the database",
        type = "error"
      )
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

      sendSweetAlert(
        session = session,
        title = "Nice!",
        text = "A new pixeler is in the database",
        type = "success"
      )
      
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
               sendSweetAlert(
                 session = session,
                 title = "Error : Table creation",
                 text = paste0(c,"\n The chevron shows you where the error is."),
                 type = "error"
               )
               rv$ERROR = T
             },warning = function(c) {
               sendSweetAlert(
                 session = session,
                 title = "Error : Table creation",
                 text = paste0(c,"\n The chevron shows you where the error is."),
                 type = "warning"
               )
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
 
      sendSweetAlert(
        session = session,
        title = "Congratulations!",
        text = "The import was successful!",
        type = "success"
      )
    
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
    # Select id from species where name ='Saccharomyces cerevisiae';
    
    species_id  = dbGetQuery(con, paste0("Select id from species where name = '",database[1,8],"';"))[1,1]
    default_db_id = dbGetQuery(con, paste0("Select id from CFSource where name = '",input$selectSource,"';"))[1,1]
    
    withProgress(message = 'Import in Database', value = 0, {
      if(input$importTypeCF == "main"){
        if(ncol(database) == 9){
          n <- nrow(database)
          rv$ERROR = F
          for(i in 1:nrow(database)){
            
            incProgress(1/n, detail = paste("Doing part", i))
            
            REQUEST_INDB = paste0("SELECT * from ChromosomalFeature WHERE feature_name = '",database[i,1],"'" );
            if(nrow(dbGetQuery(con, REQUEST_INDB)) != 0){
              REQUEST_ANNOT = paste0("UPDATE ChromosomalFeature SET gene_name = '",database[i,2],"', chromosome = '",database[i,3],
                                     "', start_coordinate = ",database[i,4],", stop_coordinate =",database[i,5],", strand ='",database[i,6],
                                     "',description ='",gsub("\'"," Prime",database[i,7]),"',species_id ='",species_id, "', url ='",database[i,9] ,"',default_db_id ='",default_db_id 
                                     ,"' WHERE Feature_name = '",database[i,1],"';" );
            } else {
              REQUEST_ANNOT = paste0("INSERT INTO ChromosomalFeature (feature_name , gene_name,  chromosome, start_coordinate, stop_coordinate, strand, description,species_id, url, default_db_id) VALUES ( ",paste(c(paste0("'",database[i,1:3],"'"), database[i,4:5], paste0("'",database[i,6],"'"), paste0("'",gsub("\'"," Prime",database[i,7]),"'"),paste0("'",species_id,"'"), paste0("'",database[i,9],"'"),paste0("'",default_db_id,"'")),collapse = ","),
                                     ");")
            }
            
            tryCatch(dbSendQuery(con, REQUEST_ANNOT)
                     , error = function(c) {
                       
                       sendSweetAlert(
                         session = session,
                         title = "Error : Table creation",
                         text = paste0(c,"\n The chevron shows you where the error is."),
                         type = "error"
                       )
                       rv$ERROR = T
                     },warning = function(c) {
                       sendSweetAlert(
                         session = session,
                         title = "Error : Table creation",
                         text = paste0(c,"\n The chevron shows you where the error is."),
                         type = "warning"
                       )
                       rv$ERROR = T
                     }
            )
            
            if(rv$ERROR == T){
              break()
            } 
            
          }
          
          if(rv$ERROR == F){
            sendSweetAlert(
              session = session,
              title = "Congratulations!",
              text = "The import was successful!",
              type = "success"
            )
          }
          
          
        } else {
          sendSweetAlert(
            session = session,
            title = "Error!",
            text = paste0("The table format is not correct. The number of columns is",ncol(database)," instead of 9."),
            type = "error"
          )
        }
      } else {
        
        # TABLE CREATION
        newTableName <- input$sup_name
        columnNewTable <- colnames(database)[-1]
        # columnNewTable <- gsub("[[:punct:]]", "",colnames(database)[-1] )
        # cat(paste(1,"  ", columnNewTable,"\n"), file = stderr())
        # columnNewTable <- gsub(" ", "_",columnNewTable)
        # cat(paste(2,"  ", columnNewTable,"\n"), file = stderr())
        
        REQUEST = paste("CREATE TABLE", newTableName, "(id SERIAL PRIMARY KEY, feature_name TEXT,",
                        paste(paste( columnNewTable, "TEXT"), collapse = ",")
                        ,", cfsource_id INTEGER, CONSTRAINT fkcfsource FOREIGN KEY (cfsource_id) REFERENCES CFSource (id), CONSTRAINT fkCF FOREIGN KEY (feature_name) REFERENCES ChromosomalFeature (feature_name));")
        
        
        
        tryCatch(dbSendQuery(con, REQUEST)
                 , error = function(c) {

                   sendSweetAlert(
                     session = session,
                     title = "Error : Table creation",
                     text = paste0(c,"\n The chevron shows you where the error is."),
                     type = "error"
                   )
                   
                   rv$ERROR = T
                   
                 },warning = function(c) {
                   sendSweetAlert(
                     session = session,
                     title = "Error : Table creation",
                     text = paste0(c,"\n The chevron shows you where the error is."),
                     type = "warning"
                   )
                   
                   rv$ERROR = T
                   
                 }
        )
        
        # insert in new table
        n <- nrow(database)
        rv$ERROR = F
        for(i in 1:nrow(database)){
          
          incProgress(1/n, detail = paste("Doing part", i))
          
          if(nrow(dbGetQuery(con,paste0("select * from chromosomalfeature where feature_name ='",gsub("\'"," Prime", database[i,1]),"';")))!=0){
            
            REQUEST_ANNOT = paste0("INSERT INTO ",newTableName," (feature_name ,",paste(columnNewTable, collapse = ","),",cfsource_id ) VALUES ( ", paste0("'", gsub("\'"," Prime", database[i,]),"'",collapse = ","),",'",default_db_id,"');")
            
            tryCatch(dbSendQuery(con, REQUEST_ANNOT)
                     , error = function(c) {
                       sendSweetAlert(
                         session = session,
                         title = "Error : Table creation",
                         text = paste0(c,"\n The chevron shows you where the error is."),
                         type = "error"
                       )
                       
                       
                       rv$ERROR = T
                       rv$ERROR_ALL = T
                       rv$linesNotSaved = c(rv$linesNotSaved , i)
                     },warning = function(c) {
                       sendSweetAlert(
                         session = session,
                         title = "Error : Table creation",
                         text = paste0(c,"\n The chevron shows you where the error is."),
                         type = "warning"
                       )
                       
                       rv$ERROR = T
                       rv$ERROR_ALL = T
                       rv$linesNotSaved = c(rv$linesNotSaved , i)
                     }
            )
            
            REQUEST_ANNOT = paste0("INSERT INTO annotation (feature_name , annot_table) VALUES ('",gsub("\'"," Prime", database[i,1]), "','",newTableName, "');")
            
            dbSendQuery(con, REQUEST_ANNOT)
            
          }
        }
        
        if(rv$ERROR == F){
          sendSweetAlert(
            session = session,
            title = "Congratulations!",
            text = "The import was successful!",
            type = "success"
          )
        }
      }  
    })
    
  })
  
  output$helpImportTypeCF <- renderText({ 
    if(input$importTypeCF == "main"){
      "By selecting 'Main information', you choose to enter new chromosomal 
      features or update existing ones. The table must be composed of 8 columns: feature name (i.e : YAL068C), gene name (i.e PAU8), chromosome, start coordinate, stop coordinate, strand, description, species and
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
  
  
  #=============================================================================
  # Quick search
  #=============================================================================
  #-----------------------------------------------------------------------------
  # Chromosomal feature
  #-----------------------------------------------------------------------------
  observeEvent(input$searchButtonCF,{
    if(input$searchCF != ""){
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      testCF = dbGetQuery(con,paste0("select feature_name, species.name from chromosomalfeature, species where lower(feature_name) ='",tolower(input$searchCF),"' or lower(gene_name) = '",tolower(input$searchCF),"' and species.id = species_id;"))
      dbDisconnect(con)
      if(nrow(testCF) == 1){
        CF$name = testCF[1,1]
        updateTextInput(session, "searchCF", value = "")
      } else if(nrow(testCF) == 0){
        sendSweetAlert(
          session = session,
          title = input$searchCF,
          text = "This chromosomal feature isn't in Pixel...",
          type = "error"
        )
      } else {
        inter = testCF[,1] 
        names(inter) = paste0(testCF[,1], " (", testCF[,2], ")") 
        confirmSweetAlert(
          session = session,
          inputId = "ConfSearch",
          type = "warning",
          text =  div(p("Pixel finds several entries for your search:"),
                      radioButtons("searchRefine", NULL,
                                   inter,inline = T),
                      p("Use one of the proposed feature names to refine your search.")),
          title = "Want to confirm ?",
          danger_mode = TRUE
        )
        
        observeEvent(input$ConfSearch, {
          if (isTRUE(input$ConfSearch)) {
            CF$name = input$searchRefine
          }
          updateTextInput(session, "searchCF", value = "")
        })
      }
    }
  })
  
  #-----------------------------------------------------------------------------
  # Tag
  #-----------------------------------------------------------------------------
  
  observeEvent(input$searchButtonTag,{
    if(input$searchTag != ""){
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      testTag = dbGetQuery(con,paste0("select name from tag where name = '",tolower(input$searchTag),"';"))
      dbDisconnect(con)
      if(nrow(testTag) != 0){
        TAG$NAME = testTag[1,1]
        updateTextInput(session, "searchTag", value = "")
      } else {
        sendSweetAlert(
          session = session,
          title = input$searchTag,
          text = "This tag isn't in Pixel...",
          type = "error"
        )
      } 
    }
  })
  
  #-----------------------------------------------------------------------------
  # Tag
  #-----------------------------------------------------------------------------
  
  observeEvent(input$searchButtonPS,{
    if(input$searchPS != ""){
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      testPS = dbGetQuery(con,paste0("select id from pixelset where lower(id) = '",tolower(input$searchPS),"';"))
      
      dbDisconnect(con)
      if(nrow(testPS) != 0){
        SEARCH_RV$PIXELSET =  testPS[1,1]
        updateTextInput(session, "testPS", value = "")
      } else {
        sendSweetAlert(
          session = session,
          title = input$testPS,
          text = "This pixelSet isn't in Pixel...",
          type = "error"
        )
      } 
    }
  })
  
  #=============================================================================
  # End Quick search
  #=============================================================================
  
  #=============================================================================
  # Chromosomal Feature
  #=============================================================================
  
  CF = reactiveValues()
  CF$sup_id = NULL 
  

  
  observeEvent(CF$name,{
    
    if(!is.null(CF$sup_id)){
      for(i in CF$sup_id){
        removeTab("tab_sup_annot", i)
      }
      CF$sup_id = NULL 
    }
    
    updateTabItems (session, "tabs", selected = "CF_item")
    shinyjs::runjs("window.scrollTo(0, 0)")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    CF$main_annotation = dbGetQuery(con, paste0("select feature_name, gene_name, chromosome, start_coordinate, stop_coordinate, strand, chromosomalfeature.description, species.name as Species_name, chromosomalfeature.url, cfsource.name from chromosomalfeature, species, cfsource where feature_name ='",CF$name,"' and cfsource.id = chromosomalfeature.default_db_id and species.id = chromosomalfeature.species_id;"))
    CF$main_annotation[1, 'url'] = paste0("<a href='",CF$main_annotation[1, 'url'] ,"'  target='_blank'>",CF$main_annotation[1, 'url'], "</a>")
    CF$main_annotation[1, 'species_name'] = paste0("<i>",CF$main_annotation[1, 'species_name'], "</i>")
    CF$main_annotation = paste("<b>", gsub("_", " " ,colnames(CF$main_annotation)),"</b> : ", CF$main_annotation[1,])
    CF$main_annotation = paste(CF$main_annotation, collapse = "<br>")
    
    
    CF$PIXELSET =  dbGetQuery(con, paste0("select * from pixel, pixelset where pixel.cf_feature_name = '",CF$name,"' and pixel.pixelset_id = pixelset.id;"))
    CF$PIXEL =  dbGetQuery(con, paste0("select value, quality_score as QS, pixelset_id, OmicsUnitType.name as OUT
                                       from pixel,OmicsUnitType 
                                       where cf_feature_name = '",CF$name,"'
                                       and omicsunittype_id = OmicsUnitType.id;"))
    
    
    CF$CF_Tag_analysis = dbGetQuery(con, paste0("select DISTINCT tag.name, tag.description from pixel, pixelset PS, analysis, Tag_Analysis, tag 
                                                  where pixel.cf_feature_name ='",CF$name,"' and
                                                  pixel.pixelset_id = PS.id 
                                                  and ps.id_analysis = analysis.id 
                                                  and Tag_Analysis.id_analysis = analysis.id 
                                                  and tag.id = Tag_Analysis.id_tag;"))
    
    CF$CF_Tag_experiment =  dbGetQuery(con, paste0("select DISTINCT tag.name, tag.description from pixel, pixelset PS, analysis, Tag_Experiment, tag, Analysis_Experiment
                                                  where pixel.cf_feature_name ='",CF$name,"'
                                                and pixel.pixelset_id = PS.id 
                                                and ps.id_analysis = analysis.id
                                                and Analysis_Experiment.id_analysis = analysis.id
                                                and Tag_Experiment.id_experiment = Analysis_Experiment.id_experiment 
                                                and tag.id = Tag_experiment.id_tag;"))
    
    CF$CF_OmicsArea =  dbGetQuery(con, paste0("select omicsarea.name, count(*)
    from pixel, pixelset PS, analysis,  Analysis_Experiment, omicsarea, Experiment
    where pixel.cf_feature_name ='",CF$name,"'
    and pixel.pixelset_id = PS.id 
    and ps.id_analysis = analysis.id
    and Analysis_Experiment.id_analysis = analysis.id
    and experiment.id = Analysis_Experiment.id_experiment 
    and omicsAreaid = omicsArea.id
    group by omicsarea.name;"))
    
    
    CF$CF_OmicsUnitType =  dbGetQuery(con, paste0("SELECT OmicsUnitType.name, count(*)
    from pixel, OmicsUnitType
    where pixel.cf_feature_name ='",CF$name,"'
    and OmicsUnitType_id = OmicsUnitType.id
    group by OmicsUnitType.name;"))
    
    CF$CF_Tag_Exp_graph = dbGetQuery(con, paste0("select tag.name, count(*) 
                                                  from pixel, pixelset PS, analysis, Tag_Experiment, tag, Analysis_Experiment
                                                  where pixel.cf_feature_name ='",CF$name,"'
                                                      and pixel.pixelset_id = PS.id 
                                                      and ps.id_analysis = analysis.id
                                                      and Analysis_Experiment.id_analysis = analysis.id
                                                      and Tag_Experiment.id_experiment = Analysis_Experiment.id_experiment 
                                                      and tag.id = Tag_experiment.id_tag
                                                      group by tag.name;"))
    
    
    CF$CF_Tag_Analysis_graph = dbGetQuery(con, paste0("select tag.name, count(*) 
                                                  from pixel, pixelset PS, analysis, Tag_Analysis, tag 
                                                  where pixel.cf_feature_name ='",CF$name,"' and
                                                      pixel.pixelset_id = PS.id 
                                                      and ps.id_analysis = analysis.id 
                                                      and Tag_Analysis.id_analysis = analysis.id 
                                                      and tag.id = Tag_Analysis.id_tag
                                                      group by tag.name;"))
    
    
    Sup_tab = dbGetQuery(con, paste0("select annot_table from annotation where feature_name ='",CF$name,"';"))
    
    if(nrow(Sup_tab) != 0){
      for(i in 1:nrow(Sup_tab)){
        
        result = dbGetQuery(con, paste0("select * from ",Sup_tab[i,1]," where feature_name ='",CF$name,"';"))
        result = paste("<b>", gsub("_", " " ,colnames(result)[-1]),"</b> : ", result[1,-1])
        result = paste(result, collapse = "<br>")
        
        if(i == 1){
          appendTab("tab_sup_annot", tabPanel(Sup_tab[i,1], HTML(result)),select = T)
        } else {
          appendTab("tab_sup_annot", tabPanel(Sup_tab[i,1], HTML(result)),select = F)
        }
        
        CF$sup_id = c(CF$sup_id, Sup_tab[i,1])
      }
    }
    
  })
  
  output$CF_title <- renderUI(
    if(!is.null(CF$name) & length(CF$name) != 0){
      h1(paste("Chromosomal feature - ",CF$name))
    } else {
      h1("Chromosomal feature")
    }
    
  )
  
  output$CF_information <- renderUI(
    tagList(
      h2(class="title-cf", "Main information"),
      HTML(CF$main_annotation)
    )
  )

  output$CF_OUT_graph <- renderGvis({
    if(!is.null(CF$CF_OmicsUnitType) && nrow(CF$CF_OmicsUnitType) != 0){
      gvisPieChart(CF$CF_OmicsUnitType,options=list(tooltip = "{text:'percentage'}"))
    } else {
      NULL
    }
    
  })
  
  output$CF_OmicsArea_graph <- renderGvis({
    if(!is.null(CF$CF_OmicsArea) && nrow(CF$CF_OmicsArea) != 0){
      gvisPieChart(CF$CF_OmicsArea,options=list(tooltip = "{text:'percentage'}"))
    } else {
      NULL
    }
  })
  
  output$CF_Tag_Analysis_graph <- renderGvis({
    if(!is.null(CF$CF_Tag_Analysis_graph) && nrow(CF$CF_Tag_Analysis_graph) != 0){
      gvisPieChart(CF$CF_Tag_Analysis_graph,options=list(tooltip = "{text:'percentage'}"))
    } else {
      NULL
    }
    
  })
  
  output$CF_Tag_Exp_graph <- renderGvis({
    if(!is.null(CF$CF_Tag_Exp_graph) && nrow(CF$CF_Tag_Exp_graph) != 0){
      gvisPieChart(CF$CF_Tag_Exp_graph,options=list(tooltip = "{text:'percentage'}"))
    } else {
      NULL
    }
  })
  
  output$CF_PixelSET <- renderDT(CF$PIXELSET, 
                                 selection = 'single', 
                                 editable = F,
                                 options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  output$CF_Pixel <- renderDT(CF$PIXEL, 
                              selection = 'none', 
                              editable = F,
                              options = list(scrollX = TRUE))
  
  output$CF_Tag_experiment <- renderDT(CF$CF_Tag_experiment, 
                                       selection = 'single', 
                                       editable = F,
                                       options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  output$CF_Tag_analysis <- renderDT(CF$CF_Tag_analysis, 
                                     selection = 'single', 
                                     editable = F,
                                     options = list(scrollX = TRUE, searchHighlight = TRUE))
  

  observeEvent(input$CF_Tag_experiment_rows_selected,{
    
    TAG$NAME = CF$CF_Tag_experiment[input$CF_Tag_experiment_rows_selected,"name"]
    
    proxy = dataTableProxy('CF_Tag_experiment')
    proxy %>% selectRows(NULL)
  })
  
  observeEvent(input$CF_Tag_analysis_rows_selected,{
    
    TAG$NAME = CF$CF_Tag_analysis[input$CF_Tag_analysis_rows_selected,"name"]
    
    proxy = dataTableProxy('CF_Tag_analysis')
    proxy %>% selectRows(NULL)
  })
  
  #=============================================================================
  # END Chromosomal Feature
  #=============================================================================
  
  
  #=============================================================================
  # PIXELSETLIST
  #=============================================================================
  PIXELSETLIST_RV = reactiveValues()
  pg <- dbDriver("PostgreSQL")
  con <- dbConnect(pg, user="docker", password="docker",
                   host=ipDB, port=5432)
  on.exit(dbDisconnect(con))
  
  REQUEST_Info = paste0("select DISTINCT PS.id as",'"',"ID",'"',", species.name as ",'"',"Species",'"',", OmicsUnitType.name as ",'"',"Omics Unit Type",'"',", OmicsArea.name as ",'"',"Omics Area",'"',", pixeler.user_name as ",'"',"Pixeler",'"',", analysis.description as ",'"',"Analysis",'"',", experiment.description as ",'"',"Experiment",'"',"
                          from pixelset PS, analysis, Analysis_Experiment AE, experiment, strain, species, OmicsArea, Submission, pixeler, pixel, OmicsUnitType
                          where PS.id_analysis = analysis.id
                          and PS.id = pixel.pixelset_id
                          and pixel.omicsunittype_id = OmicsUnitType.id
                          and analysis.id = AE.id_analysis
                          and AE.id_experiment = experiment.id
                          and experiment.strainId = strain.id
                          and strain.species_id = species.id
                          and experiment.omicsAreaid = OmicsArea.id
                          and PS.id_submission = Submission.id
                          and Submission.pixeler_user_id = pixeler.id
                          ;")
  
  PIXELSETLIST_RV$info=dbGetQuery(con,REQUEST_Info)
  
  # PIXELSETLIST_RV$tagAnalysis=dbGetQuery(con,"select tag.name, PS.id from pixelset PS, analysis, Tag_Analysis, tag 
  #                                 where  ps.id_analysis = analysis.id 
  #                                 and Tag_Analysis.id_analysis = analysis.id 
  #                                 and tag.id = Tag_Analysis.id_tag;
  #                                 ")
  # 
  # PIXELSETLIST_RV$tagExp=dbGetQuery(con,"select tag.name, PS.id from pixelset PS, analysis, Tag_Experiment, tag, Analysis_Experiment
  #                                   where ps.id_analysis = analysis.id
  #                                   and Analysis_Experiment.id_analysis = analysis.id
  #                                   and Tag_Experiment.id_experiment = Analysis_Experiment.id_experiment 
  #                                   and tag.id = Tag_experiment.id_tag;
  #                                 ")
  
  
  
  dbDisconnect(con)
  
  observeEvent(PIXELSETLIST_RV$info,{
    if(nrow(PIXELSETLIST_RV$info)!=0){
      PIXELSETLIST_RV$info[, 'Omics Unit Type' ] = as.factor(PIXELSETLIST_RV$info[, 'Omics Unit Type' ] )
      PIXELSETLIST_RV$info[, 'Omics Area' ] = as.factor(PIXELSETLIST_RV$info[, 'Omics Area' ] )
      PIXELSETLIST_RV$info[, 'Species' ] = as.factor(PIXELSETLIST_RV$info[, 'Species' ] )
      PIXELSETLIST_RV$info[, 'Pixeler' ] = as.factor(PIXELSETLIST_RV$info[, 'Pixeler' ] )
      PIXELSETLIST_RV$Selected = 1:nrow(PIXELSETLIST_RV$info)
      
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      PIXELSETLIST_RV$tags = rbind(dbGetQuery(con,"select tag.name, PS.id from pixelset PS, analysis, Tag_Analysis, tag 
                                    where  ps.id_analysis = analysis.id 
                                              and Tag_Analysis.id_analysis = analysis.id 
                                              and tag.id = Tag_Analysis.id_tag;
                                              "),
                                   dbGetQuery(con,"select tag.name, PS.id from pixelset PS, analysis, Tag_Experiment, tag, Analysis_Experiment
                                              where ps.id_analysis = analysis.id
                                              and Analysis_Experiment.id_analysis = analysis.id
                                              and Tag_Experiment.id_experiment = Analysis_Experiment.id_experiment 
                                              and tag.id = Tag_experiment.id_tag;
                                              ")
      )
      
      interList = list()
      for(i in 1:nrow(PIXELSETLIST_RV$tags)){
        interList[[PIXELSETLIST_RV$tags[i,"id"] ]] = c(interList[[PIXELSETLIST_RV$tags[i,"id"]]], PIXELSETLIST_RV$tags[i,"name"])
      }
      
      interList = lapply(interList, unique)
      interList = lapply(interList, sort)
      PIXELSETLIST_RV$tagsList = interList
      
      dbDisconnect(con)
    }
  })
  
  output$PIXELSETLIST_tab <- renderDT(PIXELSETLIST_RV$info[PIXELSETLIST_RV$Selected,],
                                      selection = 'multiple',server = FALSE,
                                      editable = F, filter = 'top',
                                      extensions = 'Buttons', options = list(
                                        scrollX = TRUE,searchHighlight = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                      )
  )
  
  output$PixelSetTags = renderUI({
    if(nrow(TAG$table) !=0){
      checkboxGroupButtons("PixelSetTags_CBG", NULL,
                           choices = PIXELSETLIST_RV$tags[!duplicated(PIXELSETLIST_RV$tags[,"name"]),"name"],
                           status = "default",
                           checkIcon = list(yes = icon("check-circle"), no = icon("times")))
    } else {
      p(class="warning","No saved tag")
    }
    
  })
  
  output$PixelSetRowSelected = renderPrint({
    if(!is.null(input$PIXELSETLIST_tab_rows_selected)){
      cat(paste0(PIXELSETLIST_RV$info[PIXELSETLIST_RV$Selected,][input$PIXELSETLIST_tab_rows_selected,1], '\tLine: ', input$PIXELSETLIST_tab_rows_selected), sep = '\n')
    }
  })
  
  observeEvent(is.null(input$PIXELSETLIST_tab_rows_selected),{
    if(!is.null(input$PIXELSETLIST_tab_rows_selected)){
      updateActionButton(session, "PixelSetExploreBtn", label = paste0("Explore (",length(input$PIXELSETLIST_tab_rows_selected),")"))
    }else{
      updateActionButton(session, "PixelSetExploreBtn", label = "Explore (0)")
    }
    
  })
  
  observeEvent(input$PixelSetExploreSelectAll,{
    dt_proxy <- DT::dataTableProxy("PIXELSETLIST_tab")
    DT::selectRows(dt_proxy, input$PIXELSETLIST_tab_rows_all)
  })
  
  observeEvent(input$PixelSetExploreDeselectAll,{
    dt_proxy <- DT::dataTableProxy("PIXELSETLIST_tab")
    DT::selectRows(dt_proxy, NULL)
  })
  
  observeEvent(is.null(input$PixelSetTags_CBG),{
    if(!is.null(input$PixelSetTags_CBG)){
      
      inter1 = names(PIXELSETLIST_RV$tagsList)[unlist(lapply(PIXELSETLIST_RV$tagsList, function(x, vec){sum(x%in%vec)==length(vec)}, vec = sort(input$PixelSetTags_CBG)))]
      inter2 = PIXELSETLIST_RV$info[,1] %in% inter1
      pos = which(inter2)
      PIXELSETLIST_RV$Selected = pos
      
    }else{
      PIXELSETLIST_RV$Selected = 1:nrow(PIXELSETLIST_RV$info)
    }
  })
  
  #=============================================================================
  # END PIXELSET List
  #=============================================================================
  
  #=============================================================================
  # PIXELSET EXPLORATION
  #=============================================================================
  
  PixelSetExploRV = reactiveValues()
  
  
  observeEvent(input$PixelSetExploreBtn,{
    PixelSetExploRV$UpsetR = list()
    
    PixelSetExploRV$PixelSetID = PIXELSETLIST_RV$info[PIXELSETLIST_RV$Selected,][input$PIXELSETLIST_tab_rows_selected,1]
    if(length(PixelSetExploRV$PixelSetID) !=0){
      updateTabItems (session, "tabs", selected = "PixelSetExplo")
      shinyjs::runjs("window.scrollTo(0, 0)")
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      PixelSetExploRV$TAB = NULL
      colnamesinter = NULL
      for(i in 1:length(PixelSetExploRV$PixelSetID)){
        inter = dbGetQuery(con,paste0("select cf_feature_name, value, quality_score from pixel where pixelset_id = '",PixelSetExploRV$PixelSetID[i],"'; "))
        if(is.null(PixelSetExploRV$TAB)){
          PixelSetExploRV$TAB = inter
        }else{
          PixelSetExploRV$TAB = merge(PixelSetExploRV$TAB, inter ,by = "cf_feature_name", all = T)
        }
        
        PixelSetExploRV$UpsetR[[paste0("PS",i)]] = inter[,'cf_feature_name']
        
        colnamesinter = c(colnamesinter, paste("PS",i, "Value",sep ="_" ), paste("PS",i, "QS",sep ="_" ))
        
      }
      
      colnames(PixelSetExploRV$TAB) = c("feature_name", 
                                        colnamesinter)
      
      PixelSetExploRV$InfoCF = dbGetQuery(con,paste0("select feature_name, gene_name,description from ChromosomalFeature where feature_name IN (",paste0("'",PixelSetExploRV$TAB[,1],"'", collapse = ","),")") )
      
      PixelSetExploRV$TAB = merge(PixelSetExploRV$InfoCF, PixelSetExploRV$TAB,by = "feature_name", all = T)
      
      colnames(PixelSetExploRV$TAB) = c("Feature name", "Gene name", "Description",
                                        colnamesinter)
      
      PixelSetExploRV$SEARCH = 1:nrow(PixelSetExploRV$TAB)
      dbDisconnect(con)
  
    }
  })
  
  
  output$PSExploUI <- renderUI({
    lapply(1:length(PixelSetExploRV$PixelSetID), function(i) {
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      file = dbGetQuery(con,paste0("select pixelset_file from pixelset where id = '",PixelSetExploRV$PixelSetID[i],"'; "))
      filename = as.character(unlist(strsplit(file[1,1], "/"))[length(unlist(strsplit(file[1,1], "/")))])
      
      dbDisconnect(con)
      
      box( 
        title = paste("> PS",i,":",PixelSetExploRV$PixelSetID[i]), 
        solidHeader = TRUE, collapsible = TRUE,collapsed = T,
        fluidRow(column(12,p("Original file :",a(filename, href=file, target="_blank")))), 
        fluidRow(column(6,uiOutput(paste0('PSExploValue', i))),
                 column(6,uiOutput(paste0('PSExploQS', i))))
      )
      
    })
    
  })
  
  observeEvent(PixelSetExploRV$PixelSetID,{
    
    lapply(1:length(PixelSetExploRV$PixelSetID), function(i) {
      output[[paste0('PSExploValue', i)]] <- renderGvis({
        gvisHistogram(data.frame(Value = PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,][input$PSExploTab_rows_all,paste("PS",i, "Value",sep ="_" )]), 
                      chartid = paste0('PSExploValue', i),
                      options=list(
                        colors="['#ff0000']",
                        legend="{ position: 'none'}",
                        title="Values",
                        width='100%', height=360))
        
      })
    })
    
    lapply(1:length(PixelSetExploRV$PixelSetID), function(i) {
      output[[paste0('PSExploQS', i)]] <- renderGvis({

        gvisHistogram(data.frame(QS = PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,][input$PSExploTab_rows_all,paste("PS",i, "QS",sep ="_" )] ), 
                      chartid = paste0('PSExploQS', i),
                      options=list(
                        colors="['#3366ff']",
                        legend="{ position: 'none'}",
                        title="Values",
                        width='100%', height=360)
        )
        
      })
    })
  })
  
  
  output$UpsetR <- renderPlot({
    upset(fromList(PixelSetExploRV$UpsetR), text.scale= 1.8)
  })
  
  output$MPS_export_csv <- downloadHandler(
      filename = function() {
        paste('Multi_PixelSet-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,][input$PSExploTab_rows_all,], con,
                  row.names= F)
      }
  )
  
  output$MPS_export_tsv <- downloadHandler(
    filename = function() {
      paste('Multi_PixelSet-', Sys.Date(), '.tsv', sep='')
    },
    content = function(con) {
      write.table(PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,][input$PSExploTab_rows_all,], con,
                row.names= F, sep = "\t", quote = F)
    }
  )
  
  output$MPS_export_excel <- downloadHandler(
    filename = function() {
      paste('Multi_PixelSet-', Sys.Date(), '.xlsx', sep='')
    },
    content = function(con) {
      write.xlsx(PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,][input$PSExploTab_rows_all,], con,
                 row.names= F)
 
    }
  )
  
  output$PSExploContent <- renderDT(PIXELSETLIST_RV$info[PIXELSETLIST_RV$Selected,][input$PIXELSETLIST_tab_rows_selected,],
                                    selection = 'single', options = list(
                                      scrollX = TRUE,searchHighlight = TRUE
                                    ))
  
  observeEvent(input$PSExploContent_rows_selected,{
    SEARCH_RV$PIXELSET = PIXELSETLIST_RV$info[PIXELSETLIST_RV$Selected,][input$PIXELSETLIST_tab_rows_selected,][input$PSExploContent_rows_selected,1]
    proxy = dataTableProxy('PSExploContent')
    proxy %>% selectRows(NULL)
  })
  
  output$PSExploTab <- renderDT(PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,],
                                selection = 'none',
                                # server = FALSE,
                                editable = F, filter = 'top',
                                #extensions = 'Buttons', 
                                options = list(
                                  scrollX = TRUE,searchHighlight = TRUE
                                  #,dom = 'Bfrtip',
                                  # buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                ) )
  
  #-----------------------------------------------------------------------------
  # Multi-PixelSet : Search
  #-----------------------------------------------------------------------------
  
  observeEvent(input$MPS_searchGenelist_btn,{
    
    PixelSetExploRV$SEARCH = which(PixelSetExploRV$TAB[, "Feature name"] %in% unlist(strsplit(gsub(" ","", input$MPS_searchGenelist), ";")))
    
    if( length(PixelSetExploRV$SEARCH) == 0){
      PixelSetExploRV$SEARCH = 1:nrow(PixelSetExploRV$TAB)
      sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "None of the genes were found in the Pixel table.",
        type = "error"
      )
    }
  })
  
  observeEvent(input$MPS_searchGenelist_clear_btn,{
    updateTextAreaInput(session,inputId = "MPS_searchGenelist", value = "")
    PixelSetExploRV$SEARCH = 1:nrow(PixelSetExploRV$TAB)
  })
  
  #=============================================================================
  # END PIXELSET Exploration
  #=============================================================================
  
  
  #=============================================================================
  # PIXELSET
  #=============================================================================
  
  
  PIXELSET_RV = reactiveValues()
  
  
  observeEvent(input$CF_PixelSET_rows_selected,{
    SEARCH_RV$PIXELSET = CF$PIXELSET[input$CF_PixelSET_rows_selected,"pixelset_id"]
    proxy = dataTableProxy('CF_PixelSET')
    proxy %>% selectRows(NULL)
  })
  
  
  observeEvent(SEARCH_RV$PIXELSET,{
    updateTabItems (session, "tabs", selected = "PixelSet")
    shinyjs::runjs("window.scrollTo(0, 0)")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    PIXELSET_RV$ID = SEARCH_RV$PIXELSET
    
    REQUEST_Info = paste0("with OUT AS (
                          select DISTINCT OmicsUnitType.name
                          from OmicsUnitType,pixel, PixelSet
                          where pixelset.id = '",PIXELSET_RV$ID,"'
                          and pixel.pixelSet_id = PixelSet.id
                          and pixel.OmicsUnitType_id = OmicsUnitType.id
    )
                          select PS.id as",'"',"ID",'"', ", PS.pixelSet_file as ",'"',"Filename",'"',", species.name as ",'"',"Species",'"',", OUT.name as ",'"',"Omics Unit Type",'"',", OmicsArea.name as ",'"',"Omics Area",'"',", pixeler.user_name as ",'"',"User name",'"',", analysis.description as ",'"',"Analysis",'"',", experiment.description as ",'"',"Experiment",'"',"
                          from pixelset PS, analysis, Analysis_Experiment AE, experiment, strain, species, OmicsArea, Submission, pixeler, OUT
                          where PS.id = '",PIXELSET_RV$ID,"'
                          and PS.id_analysis = analysis.id
                          and analysis.id = AE.id_analysis
                          and AE.id_experiment = experiment.id
                          and experiment.strainId = strain.id
                          and strain.species_id = species.id
                          and experiment.omicsAreaid = OmicsArea.id
                          and PS.id_submission = Submission.id
                          and Submission.pixeler_user_id = pixeler.id
                          ;")
    
    
    # Properties
    PIXELSET_RV$info = dbGetQuery(con,REQUEST_Info)
    inter = unlist(strsplit(PIXELSET_RV$info[1,"Filename"], "/"))
    inter = inter[length(inter)]
    PIXELSET_RV$info[1,"Filename"] = paste("<a href='",PIXELSET_RV$info[1,"Filename"],"' target='blank' >",inter ,"</a>")
    PIXELSET_RV$info[1,"Species"] = paste("<i>",PIXELSET_RV$info[1,"Species"],"</i>")
    PIXELSET_RV$info = paste("<tr><td><b>", colnames(PIXELSET_RV$info ),"</b> </td><td>", PIXELSET_RV$info [1,], "</td>")
    PIXELSET_RV$info = paste('<table class="table table-striped"><tbody>', paste(PIXELSET_RV$info, collapse = ""),"</tbody></table>")
    
    
    PIXELSET_RV$PS_Tag_analysis = dbGetQuery(con, paste0("select tag.name, tag.description from pixelset PS, analysis, Tag_Analysis, tag 
                                                         where  PS.id = '",PIXELSET_RV$ID,"'
                                                         and ps.id_analysis = analysis.id 
                                                         and Tag_Analysis.id_analysis = analysis.id 
                                                         and tag.id = Tag_Analysis.id_tag;"))
    
    
    PIXELSET_RV$PS_Tag_experiment =  dbGetQuery(con, paste0("select tag.name, tag.description from pixelset PS, analysis, Tag_Experiment, tag, Analysis_Experiment
                                                            where PS.id = '",PIXELSET_RV$ID,"' 
                                                            and ps.id_analysis = analysis.id
                                                            and Analysis_Experiment.id_analysis = analysis.id
                                                            and Tag_Experiment.id_experiment = Analysis_Experiment.id_experiment 
                                                            and tag.id = Tag_experiment.id_tag;"))
    
    PIXELSET_RV$Pixel = dbGetQuery(con,paste0("SELECT P.cf_feature_name as ",'"',"Feature name",'"',",CF.gene_name as ",'"',"Gene name",'"',", P.value as ",'"',"Value",'"',", P.quality_score as ",'"',"Quality score",'"', ", CF.description as ",'"',"Description",'"'," from pixel P, chromosomalfeature CF where cf_feature_name = feature_name and pixelSet_id ='",PIXELSET_RV$ID, "'"))
    PIXELSET_RV$SEARCH = 1:nrow(PIXELSET_RV$Pixel)
    dbDisconnect(con)
    
  })
  
  output$PixelSet_explo <- renderText({
    PIXELSET_RV$info
  })
  
  #-----------------------------------------------------------------------------
  # PixelSet : Pixel
  #-----------------------------------------------------------------------------
  
  output$PixelSet_explo_Pixel <- renderDT( PIXELSET_RV$Pixel[PIXELSET_RV$SEARCH,], server = FALSE,
                                           selection = 'single', 
                                           editable = F,
                                           extensions = 'Buttons',
                                           options = list(scrollX = TRUE, 
                                                          pageLength = 50, 
                                                          extensions = 'Buttons', 
                                                          searchHighlight = TRUE,
                                                          dom = 'Bfrtip',
                                                          buttons = c('csv', 'excel','print') ))
  
  observeEvent(input$PixelSet_explo_Pixel_rows_selected,{
    updateTabItems (session, "tabs", selected = "CF_item")
    shinyjs::runjs("window.scrollTo(0, 0)")
    CF$name = PIXELSET_RV$Pixel[input$PixelSet_explo_Pixel_rows_selected,"Feature name"]
    
    proxy = dataTableProxy('PixelSet_explo_Pixel')
    proxy %>% selectRows(NULL)
  })
  
  
  #-----------------------------------------------------------------------------
  # PixelSet : Histo
  #-----------------------------------------------------------------------------
  
  output$PixelSetHistoValue <- renderGvis({
    
    if (is.null(PIXELSET_RV$Pixel)){
      NULL
    } else if(!is.null(PIXELSET_RV$Pixel) & nrow(PIXELSET_RV$Pixel) != 0 ){
      Hist <-gvisHistogram(data.frame(Value = PIXELSET_RV$Pixel[input$PixelSet_explo_Pixel_rows_all,"Value"]), options=list(
        colors="['#ff0000']",
        legend="{ position: 'none'}",
        title="Values",
        width='100%', height=360),
        "PixelSetHistoValue")
      Hist
    } else{
      NULL
    }
    
    
  })
  
  output$PixelSetHistoQS <- renderGvis({
    
    if(is.null(PIXELSET_RV$Pixel)){
      NULL
    } else if(!is.null(PIXELSET_RV$Pixel) & nrow(PIXELSET_RV$Pixel) != 0 ){
      
      gvisHistogram(data.frame(QS = PIXELSET_RV$Pixel[input$PixelSet_explo_Pixel_rows_all,"Quality score"]), 
                    options=list(
                      colors="['#3366ff']",
                      legend="{ position: 'none'}",
                      title="Quality scores",
                      width='100%', height=360),
                    "PixelSetHistoQS")
      
    } else{
      NULL
    }
    
  })
  
  #-----------------------------------------------------------------------------
  # PixelSet : Search
  #-----------------------------------------------------------------------------
  
  observeEvent(input$PS_searchGenelist_btn,{
    
    PIXELSET_RV$SEARCH = which(PIXELSET_RV$Pixel[, "Feature name"] %in% unlist(strsplit(gsub(" ","", input$PS_searchGenelist), ";")))
    
    if( length(PIXELSET_RV$SEARCH) == 0){
      PIXELSET_RV$SEARCH = 1:nrow(PIXELSET_RV$Pixel)
      sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "None of the genes were found in the Pixel table.",
        type = "error"
      )
      
    }
    
  })
  
  observeEvent(input$PS_searchGenelist_clear_btn,{
    updateTextAreaInput(session,inputId = "PS_searchGenelist", value = "")
    PIXELSET_RV$SEARCH = 1:nrow(PIXELSET_RV$Pixel)
  })
  
  
  #-----------------------------------------------------------------------------
  # PixelSet : Tag
  #-----------------------------------------------------------------------------
  
  output$PS_Tag_experiment_UI <- renderUI({
    if(is.null(PIXELSET_RV$PS_Tag_experiment)) {
      NULL 
    } else if(nrow(PIXELSET_RV$PS_Tag_experiment) == 0){
      p("No tags for analysis.")
    } else {
      DTOutput("PS_Tag_experiment")
    }
  })
  
  output$PS_Tag_experiment <- renderDT(PIXELSET_RV$PS_Tag_experiment, 
                                       selection = 'single', 
                                       editable = F,
                                       options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  
  output$PS_Tag_analysis_UI <- renderUI({
    if(is.null(PIXELSET_RV$PS_Tag_analysis)) {
      NULL 
    } else if(nrow(PIXELSET_RV$PS_Tag_analysis) == 0){
      p("No tags for analysis.")
    } else {
      DTOutput("PS_Tag_analysis")
    }
  })
  
  output$PS_Tag_analysis <- renderDT(PIXELSET_RV$PS_Tag_analysis, 
                                     selection = 'single', 
                                     editable = F,
                                     options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  observeEvent(input$PS_Tag_experiment_rows_selected,{

    TAG$NAME = PIXELSET_RV$PS_Tag_experiment[input$PS_Tag_experiment_rows_selected,"name"]
    proxy = dataTableProxy('PS_Tag_experiment')
    proxy %>% selectRows(NULL)
  })
  
  observeEvent(input$PS_Tag_analysis_rows_selected,{

    
    TAG$NAME = PIXELSET_RV$PS_Tag_analysis[input$PS_Tag_analysis_rows_selected,"name"]
    
    proxy = dataTableProxy('PS_Tag_analysis')
    proxy %>% selectRows(NULL)
  })

  #=============================================================================
  # END PIXELSET
  #=============================================================================
  
  #=============================================================================
  # TAG
  #=============================================================================
  
  output$tagName<- renderText({
    TAG$NAME 
  })
  
  observeEvent(TAG$NAME,{
    updateTabItems (session, "tabs", selected = "Tags")
    shinyjs::runjs("window.scrollTo(0, 0)")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    TAG$PIXEL_SET_EXP = dbGetQuery(con,paste0("SELECT PS.*
                                              FROM pixelset PS, analysis, Tag_Experiment, tag, Analysis_Experiment
                                              WHERE tag.name ='",TAG$NAME,"' 
                                              AND ps.id_analysis = analysis.id
                                              AND Analysis_Experiment.id_analysis = analysis.id
                                              AND Tag_Experiment.id_experiment = Analysis_Experiment.id_experiment 
                                              AND tag.id = Tag_experiment.id_tag;"))
    
    TAG$PIXEL_SET_ANALYSIS = dbGetQuery(con,paste0("SELECT PS.* 
                                                   FROM  pixelset PS, analysis, Tag_Analysis, tag 
                                                   WHERE tag.name ='",TAG$NAME,"' 
                                                   AND ps.id_analysis = analysis.id 
                                                   AND Tag_Analysis.id_analysis = analysis.id 
                                                   AND tag.id = Tag_Analysis.id_tag;"))
    
    dbDisconnect(con)
  })
  
  output$Tag_analysis <- renderDT(TAG$PIXEL_SET_ANALYSIS, 
                                  selection = 'single', 
                                  editable = F,
                                  options = list(scrollX = TRUE))
  
  output$Tag_experiment <- renderDT(TAG$PIXEL_SET_EXP, 
                                    selection = 'single', 
                                    editable = F,
                                    options = list(scrollX = TRUE))
  
  observeEvent(input$Tag_analysis_rows_selected,{
    
    SEARCH_RV$PIXELSET = TAG$PIXEL_SET_ANALYSIS[input$Tag_analysis_rows_selected,"id"]
    proxy = dataTableProxy('Tag_analysis')
    proxy %>% selectRows(NULL)
  })
  
  observeEvent(input$Tag_experiment_rows_selected,{
    
    SEARCH_RV$PIXELSET = TAG$PIXEL_SET_EXP[input$Tag_experiment_rows_selected,"id"]
    proxy = dataTableProxy('Tag_experiment')
    proxy %>% selectRows(NULL)
  })
  
  #=============================================================================
  # END TAG
  #=============================================================================

  #=============================================================================
  # Add information
  #=============================================================================
  
  AddRV = reactiveValues()
  pg <- dbDriver("PostgreSQL")
  con <- dbConnect(pg, user="docker", password="docker",
                   host=ipDB, port=5432)
  on.exit(dbDisconnect(con))
  AddRV$OUT = dbGetQuery(con,"SELECT * from omicsunittype;")
  
  AddRV$DataSource = dbGetQuery(con,"SELECT * from DataSource;")
  AddRV$OmicsArea = dbGetQuery(con,"SELECT * from OmicsArea ORDER BY path;")
  AddRV$Species = dbGetQuery(con,"SELECT * from species;")
  AddRV$Strain = dbGetQuery(con,"SELECT * from strain;")
  AddRV$StrainSpecies = dbGetQuery(con,"select strain.name as Strain, species.name as Species from strain, species where species.id = strain.species_id;")
  
  #-----------------------------------------------------------------------------
  # Add OUT
  #-----------------------------------------------------------------------------
  
  output$DT_AddOUT <- renderDT(AddRV$OUT, selection = 'none', 
                               editable = TRUE,
                               options = list(scrollX = TRUE))
  
  
  # Edit OUT
  
  proxyOUT = dataTableProxy('DT_AddOUT')
  
  observeEvent(input$DT_AddOUT_cell_edit, {
    info = input$DT_AddOUT_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    confirmSweetAlert(
      session = session,
      inputId = "confirm_modif_OUT",
      type = "warning",
      title = "Want to confirm ?",
      text = paste(AddRV$OUT[i, j], "->", v ),
      danger_mode = TRUE
    )
    
    observeEvent(input$confirm_modif_OUT, {
      if(j != 1){
        if (isTRUE(input$confirm_modif_OUT)) {
          AddRV$OUT[i, j] <<- DT::coerceValue(v, AddRV$OUT[i, j])
          replaceData(proxyOUT, AddRV$OUT, resetPaging = F)  # important
          
          REQUEST = paste0("UPDATE omicsunittype SET ",colnames(AddRV$OUT)[j] ," = '",
                           AddRV$OUT[i, j],"' WHERE id =",AddRV$OUT[i, 1],";")
          
          pg <- dbDriver("PostgreSQL")
          con <- dbConnect(pg, user="docker", password="docker",
                           host=ipDB, port=5432)
          dbGetQuery(con, REQUEST)
          dbDisconnect(con)
        } 
      }else {
        AddRV$OUT[i, j] <<- DT::coerceValue(AddRV$OUT[i, j], AddRV$OUT[i, j])
        replaceData(proxyOUT, AddRV$OUT, resetPaging = F)  # important
      }
    }, ignoreNULL = TRUE)

  })
  
  
  observeEvent(input$addOUT_btn, {
    REQUEST_EXISTING = paste0("SELECT *
                              FROM omicsunittype
                              WHERE name = '",input$Name_OUT,"';")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    
    if(nrow(dbGetQuery(con, REQUEST_EXISTING)) != 0 ){
      sendSweetAlert(
        session = session,
        title = "Oops !!",
        text = "This OmicsUnitType is already in the database.",
        type = "error"
      )
    } else {
      REQUESTE_ADD = paste0("INSERT INTO omicsunittype (name, description) VALUES (
                            '",input$Name_OUT, "',
                            '",input$Description_OUT, "');")
      
      dbGetQuery(con, REQUESTE_ADD)
      dbDisconnect(con)
      
      sendSweetAlert(
        session = session,
        title = "Nice !!",
        text = "A new OmicsUnitType is in the database",
        type = "success"
      )
      
      REQUEST = "SELECT * FROM OmicsUnitType;"
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      AddRV$OUT = dbGetQuery(con, REQUEST)
      dbDisconnect(con)
      updateTextInput(session, "Name_OUT", value = "")
      updateTextInput(session, "Description_OUT", value = "")
    }
  })

  #-----------------------------------------------------------------------------
  # Add Datasource
  #-----------------------------------------------------------------------------
  
  output$DT_AddDataSource <- renderDT(AddRV$DataSource, selection = 'none', 
                                      editable = TRUE,
                                      options = list(scrollX = TRUE))
  
  
  # Edit OUT
  
  proxyDS = dataTableProxy('DT_AddDataSource')
  
  observeEvent(input$DT_AddDataSource_cell_edit, {
    info = input$DT_AddDataSource_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    confirmSweetAlert(
      session = session,
      inputId = "confirm_modif_DS",
      type = "warning",
      title = "Want to confirm ?",
      text = paste(AddRV$DataSource[i, j], "->", v ),
      danger_mode = TRUE
    )
    
    observeEvent(input$confirm_modif_DS, {
      if(j != 1){
        if (isTRUE(input$confirm_modif_DS)) {
          AddRV$DataSource[i, j] <<- DT::coerceValue(v, AddRV$DataSource[i, j])
          replaceData(proxyDS, AddRV$DataSource, resetPaging = F)  # important
          
          REQUEST = paste0("UPDATE datasource SET ",colnames(AddRV$DataSource)[j] ," = '",
                           AddRV$DataSource[i, j],"' WHERE id =",AddRV$DataSource[i, 1],";")
          
          pg <- dbDriver("PostgreSQL")
          con <- dbConnect(pg, user="docker", password="docker",
                           host=ipDB, port=5432)
          dbGetQuery(con, REQUEST)
          dbDisconnect(con)
        } 
      }else {
        AddRV$DataSource[i, j] <<- DT::coerceValue(AddRV$DataSource[i, j], AddRV$DataSource[i, j])
        replaceData(proxyDS, AddRV$DataSource, resetPaging = F)  # important
      }
    }, ignoreNULL = TRUE)
  })
  
  observeEvent(input$addDataSource_btn, {
    REQUEST_EXISTING = paste0("SELECT *
                              FROM datasource
                              WHERE name = '",input$Name_DataSource,"';")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    
    if(nrow(dbGetQuery(con, REQUEST_EXISTING)) != 0 ){
      sendSweetAlert(
        session = session,
        title = "Oops !!",
        text = "This datasource is already in the database",
        type = "error"
      )
      
    } else {
      REQUESTE_ADD = paste0("INSERT INTO datasource (name, description, published, url) VALUES (
                            '",input$Name_DataSource, "','",input$Description_DataSource, "','",input$Published_DataSource, "','",input$URL_DataSource, "');")
      
      dbGetQuery(con, REQUESTE_ADD)
      dbDisconnect(con)
      
      sendSweetAlert(
        session = session,
        title = "Nice !",
        text = "A new datasource is in the database",
        type = "success"
      ) 
      
      REQUEST = "SELECT * FROM datasource;"
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      AddRV$DataSource = dbGetQuery(con, REQUEST)
      dbDisconnect(con)
      
      updateTextInput(session, "Name_DataSource", value = "")
      updateTextInput(session, "Description_DataSource", value = "")
      updateTextInput(session, "URL_DataSource", value = "")
    }
  })
  
  
  #-----------------------------------------------------------------------------
  # Add OmicsArea
  #-----------------------------------------------------------------------------
  
  output$Add_OmicsArea_path = renderUI({
    choices = AddRV$OmicsArea[,'path']
    namesChoices = NULL
    for(c in choices){
      inter = unlist(strsplit(c, "\\."))
      if(length(inter) != 1){
        inter[1:(length(inter)-1)] = " - "
        
      }
      namesChoices = c(namesChoices, paste(inter, collapse = ""))
    }
    names(choices) = namesChoices
    selectInput('Add_OmicsArea_path_SI', NULL, choices)
  })
  
  
  observeEvent(input$Add_OmicsArea_btn,{
    REQUEST_EXISTING = paste0("SELECT *
                              FROM OmicsArea
                              WHERE name = '",input$Add_OmicsArea_name,"';")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    
    if(nrow(dbGetQuery(con, REQUEST_EXISTING)) != 0 ){
      sendSweetAlert(
        session = session,
        title = "Oops !!",
        text = "This datasource is already in the database",
        type = "error"
      )
    } else {
      REQUESTE_ADD = paste0("INSERT INTO omicsarea (id, name, description, path) VALUES ('",gsub(' ', '', input$Add_OmicsArea_name),"',
                            '",input$Add_OmicsArea_name, "','",input$Add_OmicsArea_description, "','",paste(input$Add_OmicsArea_path_SI,gsub(' ', '', input$Add_OmicsArea_name) , sep='.'),"');")
      
      dbGetQuery(con, REQUESTE_ADD)
      dbDisconnect(con)
      
      sendSweetAlert(
        session = session,
        title = "Nice !!",
        text = "A new datasource is in the database",
        type = "success"
      )
      
      REQUEST = "SELECT * FROM OmicsArea ORDER BY path;"
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      AddRV$OmicsArea = dbGetQuery(con, REQUEST)
      
      choices = AddRV$OmicsArea[,'path']
      namesChoices = NULL
      for(c in choices){
        inter = unlist(strsplit(c, "\\."))
        if(length(inter) != 1){
          inter[1:(length(inter)-1)] = " - "
          
        }
        namesChoices = c(namesChoices, paste(inter, collapse = ""))
      }
      names(choices) = namesChoices
      
      updateSelectInput(session, 'Modify_OmicsArea_name_SI', choices = AddRV$OmicsArea[,'name'])
      updateSelectInput(session, 'Modify_OmicsArea_path_SI', choices = choices)
      updateSelectInput(session, 'Add_OmicsArea_path_SI', choices = choices)
      updateTextInput(session, "Add_OmicsArea_name", value = "")
      updateTextInput(session, "Add_OmicsArea_description", value = "")
      textInput('Modify_OmicsArea_description_TI', NULL, value = AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Modify_OmicsArea_name_SI),'description'] )
      
      dbDisconnect(con)
    }
  })
  
  output$Modify_OmicsArea_name = renderUI({
    selectInput('Modify_OmicsArea_name_SI', NULL, AddRV$OmicsArea[,'name'])
  })
  
  output$Modify_OmicsArea_path = renderUI({
    choices = AddRV$OmicsArea[,'path']
    namesChoices = NULL
    for(c in choices){
      inter = unlist(strsplit(c, "\\."))
      if(length(inter) != 1){
        inter[1:(length(inter)-1)] = " - "
        
      }
      namesChoices = c(namesChoices, paste(inter, collapse = ""))
    }
    names(choices) = namesChoices
    selectInput('Modify_OmicsArea_path_SI', NULL, choices)
  })
  
  
  output$Modify_OmicsArea_description = renderUI({
    textInput('Modify_OmicsArea_description_TI', NULL, value = AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Modify_OmicsArea_name_SI),'description'] )
  })
  
  observeEvent(input$Modify_OmicsArea_name_SI,{
    updateSelectInput(session, 'Modify_OmicsArea_path_SI', NULL, selected  = AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Modify_OmicsArea_name_SI),'path'])
    
    updateTextInput(session, 'Modify_OmicsArea_description_TI', value = AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Modify_OmicsArea_name_SI),'description'])
  })
  
  observeEvent(input$Modify_OmicsArea_btn,{
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    if( input$Modify_OmicsArea_description_TI != AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Modify_OmicsArea_name_SI),'description'] ){
      dbGetQuery(con, paste0("update OmicsArea set description = '",input$Modify_OmicsArea_description_TI,"' where name = '",input$Modify_OmicsArea_name_SI,"';"))
    }
    
    SOURCE_PATH = AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Modify_OmicsArea_name_SI),'path']
    
    if(input$Modify_OmicsArea_path_SI != SOURCE_PATH){
      dbGetQuery(con, paste0("update OmicsArea set path = '",input$Modify_OmicsArea_path_SI,"' || subpath(path, nlevel('",SOURCE_PATH,"')-1)
                where path <@ '",SOURCE_PATH,"';"))
    }
    
    sendSweetAlert(
      session = session,
      title = "Nice !!",
      text = "Data modified !",
      type = "success"
    )
    
    REQUEST = "SELECT * FROM OmicsArea ORDER BY path;"
    AddRV$OmicsArea = dbGetQuery(con, REQUEST)
    
    choices = AddRV$OmicsArea[,'path']
    namesChoices = NULL
    for(c in choices){
      inter = unlist(strsplit(c, "\\."))
      if(length(inter) != 1){
        inter[1:(length(inter)-1)] = " - "
        
      }
      namesChoices = c(namesChoices, paste(inter, collapse = ""))
    }
    names(choices) = namesChoices
    
    updateSelectInput(session, 'Delete_branch_OmicsArea_SI', choices = AddRV$OmicsArea[,'name'])
    updateSelectInput(session, 'Modify_OmicsArea_name_SI', choices = AddRV$OmicsArea[,'name'])
    updateSelectInput(session, 'Modify_OmicsArea_path_SI', choices = choices)
    updateSelectInput(session, 'Add_OmicsArea_path_SI', choices = choices)
    textInput('Modify_OmicsArea_description_TI', NULL, value = AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Modify_OmicsArea_name_SI),'description'] )
    
  })
  
  observeEvent(input$Delete_branch_OmicsArea_btn,{
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    
    
    confirmSweetAlert(
      session = session,
      inputId = "confirm_delete_OA",
      type = "warning",
      title = "Want to confirm ?",
      text = paste("Delete branch :",AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Delete_branch_OmicsArea_SI),'path'] , "?" ),
      danger_mode = TRUE
    )
    
    observeEvent(input$confirm_delete_OA, {
      if (isTRUE(input$confirm_delete_OA)) {
        dbGetQuery(con,paste0("delete from OmicsArea where '",AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Delete_branch_OmicsArea_SI),'path'],"' @> path;"))
        
        REQUEST = "SELECT * FROM OmicsArea ORDER BY path;"
        AddRV$OmicsArea = dbGetQuery(con, REQUEST)
        
        choices = AddRV$OmicsArea[,'path']
        namesChoices = NULL
        for(c in choices){
          inter = unlist(strsplit(c, "\\."))
          if(length(inter) != 1){
            inter[1:(length(inter)-1)] = " - "
            
          }
          namesChoices = c(namesChoices, paste(inter, collapse = ""))
        }
        names(choices) = namesChoices
        
        updateSelectInput(session, 'Delete_branch_OmicsArea_SI', choices = AddRV$OmicsArea[,'name'])
        updateSelectInput(session, 'Modify_OmicsArea_name_SI', choices = AddRV$OmicsArea[,'name'])
        updateSelectInput(session, 'Modify_OmicsArea_path_SI', choices = choices)
        updateSelectInput(session, 'Add_OmicsArea_path_SI', choices = choices)
        textInput('Modify_OmicsArea_description_TI', NULL, value = AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Modify_OmicsArea_name_SI),'description'] )
        
      } else {
        sendSweetAlert(
          session = session,
          title = "Cancellation...",
          text = "Deletion cancelled !",
          type = "warning"
        )
      }
      
    })
    
  }
  )
  output$Delete_branch_OmicsArea = renderUI({
    selectInput('Delete_branch_OmicsArea_SI', NULL, AddRV$OmicsArea[,'name'])
  })
  
  output$treeOmicsArea <- renderGvis({
    firstAncestor = NULL
    for(p in AddRV$OmicsArea[,'path']){
      inter = unlist(strsplit(p,"\\."))
      if(length(inter) == 1){
        firstAncestor = c(firstAncestor, NA)
      }else{
        firstAncestor = c(firstAncestor,AddRV$OmicsArea[which(AddRV$OmicsArea[,'id'] == inter[length(inter)-1]),'name'] )
      }
    }
    
    dataplot = as.data.frame(cbind(AddRV$OmicsArea[,'name'], firstAncestor ,AddRV$OmicsArea[,'description']))
    colnames(dataplot) = c("Omicsarea", "Parent", "Description")
    
    gvisOrgChart(dataplot, 
                 options=list(size='large', color = "#ffb3b3"))
    
  })
  
  
  #-----------------------------------------------------------------------------
  # Add Species
  #-----------------------------------------------------------------------------
  
  output$DT_AddSpecies <- renderDT(AddRV$Species, selection = 'none',
                                   editable = TRUE,
                                   options = list(scrollX = TRUE))
  
  
  # Edit OUT
  
  proxySpecies = dataTableProxy('DT_AddSpecies')
  
  observeEvent(input$DT_AddSpecies_cell_edit, {
    info = input$DT_AddSpecies_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    confirmSweetAlert(
      session = session,
      inputId = "confirm_modif_Species",
      type = "warning",
      title = "Want to confirm ?",
      text = paste(AddRV$Species[i, j], "->", v ),
      danger_mode = TRUE
    )
    
    observeEvent(input$confirm_modif_Species, {
      if(j != 1){
        if (isTRUE(input$confirm_modif_Species)) {
          AddRV$Species[i, j] <<- DT::coerceValue(v, AddRV$Species[i, j])
          replaceData(proxySpecies, AddRV$Species, resetPaging = F)  # important
          
          REQUEST = paste0("UPDATE species SET ",colnames(AddRV$Species)[j] ," = '",
                           AddRV$Species[i, j],"' WHERE id =",AddRV$Species[i, 1],";")
          
          pg <- dbDriver("PostgreSQL")
          con <- dbConnect(pg, user="docker", password="docker",
                           host=ipDB, port=5432)
          dbGetQuery(con, REQUEST)
          dbDisconnect(con)
        }
      }else {
        AddRV$Species[i, j] <<- DT::coerceValue(AddRV$Species[i, j], AddRV$Species[i, j])
        replaceData(proxySpecies, AddRV$Species, resetPaging = F)  # important
      }
    }, ignoreNULL = TRUE)
  })
  
  
  observeEvent(input$addSpecies_btn, {
    REQUEST_EXISTING = paste0("SELECT *
                              FROM species
                              WHERE name = '",input$Name_Species,"';")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    
    if(nrow(dbGetQuery(con, REQUEST_EXISTING)) != 0 ){
      sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "This species is already in the database",
        type = "error"
      )
      
    } else {
      REQUESTE_ADD = paste0("INSERT INTO species (name, description, url) VALUES (
                            '",input$Name_Species, "',
                            '",input$Description_Species, "',
                            '",input$URL_Species, "');")
      
      dbGetQuery(con, REQUESTE_ADD)
      dbDisconnect(con)
      
      sendSweetAlert(
        session = session,
        title = "Nice !",
        text = "A new species is in the database",
        type = "success"
      )
      
      REQUEST = "SELECT * FROM species;"
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      AddRV$Species = dbGetQuery(con, REQUEST)
      dbDisconnect(con)
      
      updateTextInput(session, "Name_Species", value = "")
      updateTextInput(session, "Description_Species", value = "")
      updateTextInput(session, "URL_Species", value = "")
      updateSelectInput(session, "Species_Strain_SI", choices = AddRV$Species[,'name'])
    }
  })
  
  
  
  #-----------------------------------------------------------------------------
  # Add Strain
  #-----------------------------------------------------------------------------
  
  output$DT_AddStrain <- renderDT(AddRV$Strain, selection = 'none',
                                  editable = TRUE,
                                  options = list(scrollX = TRUE))
  
  # Edit OUT
  
  proxyStrain = dataTableProxy('DT_AddStrain')
  
  observeEvent(input$DT_AddStrain_cell_edit, {
    info = input$DT_AddStrain_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    confirmSweetAlert(
      session = session,
      inputId = "confirm_modif_Strain",
      type = "warning",
      title = "Want to confirm ?",
      text = paste(AddRV$Strain[i, j], "->", v ),
      danger_mode = TRUE
    )
    
    observeEvent(input$confirm_modif_Strain, {
      if(j != 1){
        if (isTRUE(input$confirm_modif_Strain)) {
          AddRV$Strain[i, j] <<- DT::coerceValue(v, AddRV$Strain[i, j])
          replaceData(proxyStrain, AddRV$Strain, resetPaging = F)  # important
          
          REQUEST = paste0("UPDATE strain SET ",colnames(AddRV$Strain)[j] ," = '",
                           AddRV$Strain[i, j],"' WHERE id =",AddRV$Strain[i, 1],";")
          
          pg <- dbDriver("PostgreSQL")
          con <- dbConnect(pg, user="docker", password="docker",
                           host=ipDB, port=5432)
          dbGetQuery(con, REQUEST)
          dbDisconnect(con)
        }
      }else {
        AddRV$Strain[i, j] <<- DT::coerceValue(AddRV$Strain[i, j], AddRV$Strain[i, j])
        replaceData(proxyStrain, AddRV$Strain, resetPaging = F)  # important
      }
    }, ignoreNULL = TRUE)
  })
  
  
  observeEvent(input$addStrain_btn, {
    REQUEST_EXISTING = paste0("SELECT *
                              FROM strain
                              WHERE name = '",input$Name_Strain,"';")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    
    species_ID = dbGetQuery(con, paste0("SELECT id from species where name ='",input$Species_Strain_SI,"';"))[1,1] 
    
    if(nrow(dbGetQuery(con, REQUEST_EXISTING)) != 0 ){
      sendSweetAlert(
        session = session,
        title = "Oops !",
        text = "This strain is already in the database",
        type = "error"
      )
      
    } else {
      REQUESTE_ADD = paste0("INSERT INTO strain (name, description, ref, species_id) VALUES (
                            '",input$Name_Strain, "',
                            '",input$Description_Strain, "',
                            '",input$Ref_Strain, "',
                            '",species_ID, "');")
      
      dbGetQuery(con, REQUESTE_ADD)
      dbDisconnect(con)
      
      sendSweetAlert(
        session = session,
        title = "Nice !",
        text = "A new strain is in the database",
        type = "success"
      )
      
      REQUEST = "SELECT * FROM strain;"
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      AddRV$Strain = dbGetQuery(con, REQUEST)
      AddRV$StrainSpecies = dbGetQuery(con,"select strain.name as Strain, species.name as Species from strain, species where species.id = strain.species_id;")
      dbDisconnect(con)
      
      updateTextInput(session, "Name_Strain", value = "")
      updateTextInput(session, "Description_Strain", value = "")
      updateTextInput(session, "Ref_Strain", value = "")
    }
  })
  
  output$Species_Strain = renderUI({
    selectInput('Species_Strain_SI', NULL, AddRV$Species[,'name'])
  })
  
  #=============================================================================
  # Submission
  #=============================================================================
  
  submissionRV = reactiveValues()
  submissionRV$Read = F

  observe({
    if(is.null(input$fileCF)){
      disable("ImportCF")
    } else if(input$importTypeCF =="sup"){
      if(is.null(input$sup_name) ){
        disable("ImportCF")
      }else {
        if(input$sup_name == ""){
          disable("ImportCF")
        } else {
          enable("ImportCF")
        }
      }
      
    } else{
      enable("ImportCF")
    }
  })
  
  #-----------------------------------------------------------------------------
  # Experiment
  #-----------------------------------------------------------------------------
  
  output$Submission_Exp_omicsArea = renderUI({
    if(nrow(AddRV$OmicsArea) != 0){
      selectInput('Submission_Exp_omicsArea_SI', NULL, AddRV$OmicsArea[,'name'])
    } else {
      p(class="warning","No saved Omicsarea")
    }
    
  })
  
  output$Submission_Exp_dataSource = renderUI({
    if(nrow(AddRV$DataSource) !=0){
      selectInput('Submission_Exp_dataSource_SI', NULL, AddRV$DataSource[,'name'])
    } else {
      p(class="warning","No saved data sources")
    }
  })
  
  
  observeEvent(input$Submission_Exp_Species_SI,{
    updateSelectInput(session,"Submission_Exp_Strain_SI", choices = unique(AddRV$StrainSpecies[which(AddRV$StrainSpecies[,"species"] == input$Submission_Exp_Species_SI),'strain'] ))
  })
  
  output$Submission_Exp_Species = renderUI({
    
    if(nrow(AddRV$StrainSpecies) !=0){
      div(class="italic",selectInput('Submission_Exp_Species_SI', label = NULL, choices =  unique(AddRV$StrainSpecies[,'species'])))
    } else {
      p(class="warning","No saved link between species and strain.")
    }
    
  })
  
  output$Submission_Exp_Strain = renderUI({
    
    if(nrow(AddRV$Strain) !=0){
      selectInput('Submission_Exp_Strain_SI', NULL, AddRV$StrainSpecies[which(AddRV$StrainSpecies[,"species"] == input$Submission_Exp_Species_SI),'strain'])
    } else {
      p(class="warning","No saved strain")
    }
    
  })
  
  TAG = reactiveValues()
  pg <- dbDriver("PostgreSQL")
  con <- dbConnect(pg, user="docker", password="docker",
                   host=ipDB, port=5432)
  REQUEST = paste0("SELECT * FROM tag ORDER BY name;")
  TAG$table = dbGetQuery(con, REQUEST)
  dbDisconnect(con)
  
  output$Submission_Exp_tags = renderUI({
    if(nrow(TAG$table) !=0){
      checkboxGroupInput("Submission_Exp_tags_CBG", NULL,
                         choices = TAG$table[,"name"], inline = T)
    } else {
      p(class="warning","No saved tag")
    }
    
  })
  
  output$Submission_Analysis_tags = renderUI({
    if(nrow(TAG$table) !=0){
      checkboxGroupInput("Submission_Analysis_tags_CBG", NULL,
                         choices = TAG$table[,"name"], inline = T)
    } else {
      p(class="warning","No saved tag")
    }
    
  })
  
  
  observeEvent(input$Submission_Exp_tags_Newbtn,{
    
    REQUEST_EXISTING = paste0("SELECT *
                                FROM tag
                              WHERE name = '",tolower(input$Submission_tags_NewName),"';")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    
    if(nrow(dbGetQuery(con, REQUEST_EXISTING)) != 0 ){
      sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "This tag is already in the database",
        type = "error"
      )
      
    } else {
      REQUESTE_ADD = paste0("INSERT INTO tag (name, description) VALUES (
                            '",tolower(input$Submission_tags_NewName), "',
                            '",input$Submission_tags_NewDescription, "');
                            ")
      dbGetQuery(con, REQUESTE_ADD)
      
      sendSweetAlert(
        session = session,
        title = "Nice !",
        text = "A new tag is in the database",
        type = "success"
      )
      
      REQUEST = paste0("select * from tag ORDER BY name;")
      TAG$table = dbGetQuery(con, REQUEST)
      dbDisconnect(con)
    }
    
    updateTextInput(session,"Submission_tags_NewName",value = "")
    updateTextInput(session,"Submission_tags_NewDescription",value = "")
    dbDisconnect(con)
  })
  
  observeEvent(input$Submission_Exp_tags_Newbtn_analysis,{
    
    REQUEST_EXISTING = paste0("SELECT *
                              FROM tag
                              WHERE name = '",tolower(input$Submission_tags_NewName_analysis),"';")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    
    if(nrow(dbGetQuery(con, REQUEST_EXISTING)) != 0 ){
      sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "This user is already in the database",
        type = "error"
      )
      
    } else {
      REQUESTE_ADD = paste0("INSERT INTO tag (name, description) VALUES (
                            '",tolower(input$Submission_tags_NewName_analysis), "',
                            '",input$Submission_tags_NewDescription_analysis, "');
                            ")
      dbGetQuery(con, REQUESTE_ADD)
      
      sendSweetAlert(
        session = session,
        title = "Nice !",
        text = "A new pixeler is in the database",
        type = "success"
      )
      
      REQUEST = paste0("select * from tag ORDER BY name;")
      TAG$table = dbGetQuery(con, REQUEST)
      dbDisconnect(con)
    }
    
    updateTextInput(session,"Submission_tags_NewName_analysis",value = "")
    updateTextInput(session,"Submission_tags_NewDescription_analysis",value = "")
    dbDisconnect(con)
  })
  
  
  
  observeEvent(input$SubmissionClear,{
    reset('submission_Analysis_notebook')
    reset('submission_Analysis_secondary_data')
    updateTextAreaInput(session, "submission_Exp_description", value = "")
    updateTextAreaInput(session, "submission_Analysis_description", value = "")
    updateCheckboxGroupInput(session,"Submission_Exp_tags_CBG", selected = character(0))
    updateCheckboxGroupInput(session,"Submission_Analysis_tags_CBG", selected = character(0))
    updateSelectInput(session, "submission_pixelSet_nbr", selected = 2)
    updateSelectInput(session, "submission_pixelSet_nbr", selected = 1)
    submissionRV$Read = F
    
    shinyjs::runjs("window.scrollTo(0, 0)")
  })
  
  observeEvent(input$Submission,{
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    
    #---------------------------------------------------------------------------
    # Check
    #---------------------------------------------------------------------------
    
    CF_Temp = dbGetQuery(con,paste0("SELECT feature_name from chromosomalfeature;"))
    
    
    warning_sub = NULL
    for( i in 1:input$submission_pixelSet_nbr){
      inter_warning <- read.csv2(eval(parse(text = paste0("input$submission_pixelSet_file",i,"$datapath"))),
                                 header = as.logical(input$header_PS),
                                 sep = input$sep_PS,
                                 quote = input$quote_PS
      )
      pos = !(inter_warning[,1] %in% CF_Temp[,1])
      refused = inter_warning[pos,1]
      
      warning_sub= c(warning_sub, paste0("<b>PixelSet ", i, "</b> <br/>", paste(refused, collapse = "\t")))
    }
    # cat(warning_sub, file = stderr())
    
    dbDisconnect(con)
    
    confirmSweetAlert(
      session = session,
      inputId = "confirm_submission_warning",
      type = "warning",
      title = "Want to confirm ?",
      text = HTML("<p><i>NOTE : If you confirm, the pixels associated with the genes below will not be imported into the database.</i></p>",paste("<br/><p>",warning_sub,"</p>", collapse = "<br/>")),
      danger_mode = TRUE,  html = TRUE
    )
  })
  
  
  observeEvent(input$confirm_submission_warning, {
    
    if (isTRUE(input$confirm_submission_warning)) {
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      
      time = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      
      omicsAreaid = dbGetQuery(con, paste0("SELECT id from omicsarea where name = '",input$Submission_Exp_omicsArea_SI,"';"))[1,1]
      strainId = dbGetQuery(con, paste0("SELECT id from strain where name = '",input$Submission_Exp_Strain_SI,"';"))[1,1]
      DataSourceId = dbGetQuery(con, paste0("SELECT id from datasource where name = '",input$Submission_Exp_dataSource_SI,"';"))[1,1]
      
      # Add experiment
      id_exp = paste0("Exp_",time )
      REQUEST_Exp =  paste0("INSERT INTO experiment (id, omicsAreaid, description, completionDate,strainId, DataSourceId) values ('",id_exp,"','",omicsAreaid,"','",input$submission_Exp_description,"',	to_date('",input$submission_Exp_completionDate,"', 'YYYY')",",'",strainId,"','",DataSourceId,"');")
      dbGetQuery(con, REQUEST_Exp)
      
      # Add tag associated 
      if(length(input$Submission_Exp_tags_CBG) !=0){
        REQUEST_Exp_tag = paste0("INSERT INTO Tag_Experiment (id_tag, id_experiment) SELECT id, '",id_exp,"' from tag where name in (",paste0("'",input$Submission_Exp_tags_CBG,"'", collapse = ","),");")
        dbGetQuery(con, REQUEST_Exp_tag)
      }
      
      # Create a submission 
      id_Submission = paste0("Submission_",time)
      dir.create(paste0("www/Submissions/", id_Submission))
      
      # Add analysis
      id_analysis = paste0("Analysis_",time)
      adresse_analysis = paste0("Submissions/",id_Submission,"/")
      adresse_analysis_notebook = paste0(adresse_analysis,input$submission_Analysis_notebook$name)
      adresse_analysis_SD = paste0(adresse_analysis,input$submission_Analysis_secondary_data$name)
      
      file.copy(input$submission_Analysis_notebook$datapath, paste0("www/", adresse_analysis_notebook))
      file.copy(input$submission_Analysis_secondary_data$datapath, paste0("www/",adresse_analysis_SD))
      
      REQUEST_Analysis =  paste0("insert into analysis (id, description, completionDate, notebook_file,secondary_data_file) values ('",id_analysis,"','",input$submission_Analysis_description,"',	to_date('",input$submission_Exp_completionDate,"', 'YYYY')",",'",adresse_analysis_notebook,"','",adresse_analysis_SD,"');")
      dbGetQuery(con, REQUEST_Analysis)
      
      # Add tag associated 
      if(length(input$Submission_Analysis_tags_CBG) !=0){
        REQUEST_Analysis_tag = paste0("insert into Tag_Analysis (id_tag, id_analysis) SELECT id, '",id_analysis,"' from tag where name in (",paste0("'",input$Submission_Analysis_tags_CBG,"'", collapse = ","),");")
        dbGetQuery(con, REQUEST_Analysis_tag)
        
      }
      
      # add link analysis / experiment
      REQUEST_Analysis_Exp = paste0("insert into Analysis_Experiment (id_experiment, id_analysis) values('",id_exp,"','",id_analysis,"');")
      dbGetQuery(con, REQUEST_Analysis_Exp)
      
      # Add Submission

      pixeler_user_id = dbGetQuery(con, paste0("SELECT id from pixeler where user_name='",isolate(input$USER),"';"))[1,1]
      REQUEST_Submission = paste0("insert into Submission (id, submission_date, status, pixeler_user_id) values('",id_Submission,"', to_date('",Sys.Date(),"', 'YYYY-MM-DD'), FALSE, '",pixeler_user_id,"');")
      dbGetQuery(con, REQUEST_Submission)
      
      # Add PixelSet
      withProgress(message = 'PixelSet imported', value = 0, {
        m = as.numeric(input$submission_pixelSet_nbr)
        for( i in 1:input$submission_pixelSet_nbr){
          incProgress(1/m, detail = paste0("Imported :", floor(i/m*100),"%"))
          
          id_PixelSets = paste0("PixelSet_",time,"_",i)
          
          adresse_PixelSet = paste0("Submissions/",id_Submission,"/",id_PixelSets,"/")
          dir.create(paste0("www/",adresse_PixelSet))
          
          adresse_analysis_file = paste0(adresse_PixelSet,eval(parse(text = paste0("input$submission_pixelSet_file",i,"$name"))))
          
          file.copy(eval(parse(text = paste0("input$submission_pixelSet_file",i,"$datapath"))), paste0("www/", adresse_analysis_file))   
          
          REQUEST_PixelSet = paste0("insert into PixelSet (id, name, pixelSet_file, description, id_analysis, id_submission) values('",id_PixelSets,"', '",eval(parse(text = paste0("input$submission_pixelSet_name_",i))),"','",adresse_analysis_file,"','",eval(parse(text = paste0("input$submission_pixelSet_description_",i))),"','",id_analysis,"','",id_Submission,"');")
          dbGetQuery(con, REQUEST_PixelSet)
          
          inter <- read.csv2(eval(parse(text = paste0("input$submission_pixelSet_file",i,"$datapath"))),
                             header = as.logical(input$header_PS),
                             sep = input$sep_PS,
                             quote = input$quote_PS
          )
          
          # Add pixel
          
          withProgress(message = 'Pixel', value = 0, {
            n = nrow(inter)
            for(j in 1:nrow(inter)){
              incProgress(1/n, detail = paste0("Imported :", floor(j/n*100),"%")) 
              REQUEST_Pixel = paste0("insert into Pixel (value, quality_score, pixelSet_id, cf_feature_name, OmicsUnitType_id) values(",inter[j,2],",", inter[j,3],",'",id_PixelSets,"','",inter[j,1],"','",input$submission_pixelSet_OUT,"');")
              dbGetQuery(con, REQUEST_Pixel)
            }
          })
        }
      })
      setwd("www/Submissions/")
      zip(id_Submission, id_Submission)
      setwd("../..")
      #system(paste0("zip -r ",paste0("www/Submissions/", id_Submission,".zip")," ", paste0("www/Submissions/", id_Submission)))
      submissionRV$ZIP = paste0("www/Submissions/", id_Submission,".zip")
      
      DASHBOARD_RV$PIXELSET = dbGetQuery(con,"SELECT count(*) from pixelset;")[1,1]
      DASHBOARD_RV$PIXEL = dbGetQuery(con,"SELECT count(*) from pixel;")[1,1]
      DASHBOARD_RV$CF = dbGetQuery(con,"SELECT count(*) from chromosomalfeature;")[1,1]
      DASHBOARD_RV$PixelSetTABLE = dbGetQuery(con,"SELECT * FROM pixelset order by id DESC LIMIT 10;")
      DASHBOARD_RV$OUT = dbGetQuery(con, "SELECT OUT.name, count(*) FROM Pixel, omicsunittype OUT WHERE out.id = omicsunittype_id group by OUT.name;")
      DASHBOARD_RV$Species = dbGetQuery(con, "SELECT species.name, count(*) FROM pixel, chromosomalfeature CF, species WHERE cf_feature_name = feature_name and CF.species_id = species.id group by species.name;")
      
      # Clear after submission
      reset('submission_Analysis_notebook')
      reset('submission_Analysis_secondary_data')
      updateTextAreaInput(session, "submission_Exp_description", value = "")
      updateTextAreaInput(session, "submission_Analysis_description", value = "")
      updateCheckboxGroupInput(session,"Submission_Exp_tags_CBG", selected = character(0))
      updateCheckboxGroupInput(session,"Submission_Analysis_tags_CBG", selected = character(0))
      updateSelectInput(session, "submission_pixelSet_nbr", selected = 2)
      updateSelectInput(session, "submission_pixelSet_nbr", selected = 1)
      submissionRV$Read = F
      
      updateTabItems (session, "tabs", selected = "Dashboard")
      sendSweetAlert(
        session = session,
        title = "Successfully imported!",
        text = div("The pixels have been successfully imported. Click on the button to download the zip folder of submission ",br(),
        downloadButton("submissionZip", "Download")),
        html = T,
        type = "success"
      )
      
      dbDisconnect(con)
      shinyjs::runjs("window.scrollTo(0, 0)")
    } else {
      sendSweetAlert(
        session = session,
        title = "Cancellation !",
        text = "Submission is cancelled",
        type = "warning"
      )
    }
  })
  
  output$submissionZip <- downloadHandler(
    filename <- function() {
      submissionRV$ZIP 
    },
    
    content <- function(file) {
      file.copy(submissionRV$ZIP, file)
    },
    contentType = "application/zip"
  )
  
  
  observeEvent(input$col, {
    js$pageCol(input$col)
  })
  
  observeEvent(input$submission_pixelSet_nbr,{
    
    if(!is.null(submissionRV$nbrPixelSet)){
      
      for(i in 1: submissionRV$nbrPixelSet){
        removeTab("tab_PixelSets", paste("PixelSet",i))
      }
    }
    
    submissionRV$nbrPixelSet = input$submission_pixelSet_nbr
    
    choices = AddRV$OUT[,1]
    names(choices) = AddRV$OUT[,2]
    
    for(i in 1:input$submission_pixelSet_nbr){
      if(i == 1){
        appendTab("tab_PixelSets", tabPanel(paste("PixelSet",i), 
                                            h4("Name") ,
                                            textInput(paste0('submission_pixelSet_name_',i), NULL),
                                            h4("Description"),
                                            textAreaInput(paste0('submission_pixelSet_description_',i), NULL, resize = "vertical"),
                                            fileInput(paste0("submission_pixelSet_file",i),label = NULL,
                                                      buttonLabel = "Browse...",
                                                      placeholder = "No file selected")
                                            
        ),select = T)
        
      }else{
        appendTab("tab_PixelSets",  tabPanel(paste("PixelSet",i), 
                                             h4("Name") ,
                                             textInput(paste0('submission_pixelSet_name_',i), NULL),
                                             h4("Description"),
                                             textAreaInput(paste0('submission_pixelSet_description_',i), NULL, resize = "vertical"),
                                             fileInput(paste0("submission_pixelSet_file",i),label = NULL,
                                                       buttonLabel = "Browse...",
                                                       placeholder = "No file selected")
        ),select = F)
        
      }
      
    }
    
  })
  
  output$submission_pixelSet_OUT_UI <- renderUI({
    choices = AddRV$OUT[,1]
    names(choices) = AddRV$OUT[,2]
    selectInput("submission_pixelSet_OUT",label = NULL,
                choices)
    
  })
  
  observeEvent(input$submission_pixelSet_file1,{
    if(input$submission_pixelSet_file1$datapath != ""){
      submissionRV$Read = T
    } else {
      submissionRV$Read = F
    }
  })
  
  output$contents_PS <-  renderDataTable({
    
    req(input$submission_pixelSet_file1)
    
    if(submissionRV$Read){
      df <- read.csv2(input$submission_pixelSet_file1$datapath,
                      header = as.logical(input$header_PS),
                      sep = input$sep_PS,
                      quote = input$quote_PS,
                      nrows=5
      ) 
    } else {
      NULL
    }
    
    
  },  options = list(scrollX = TRUE , dom = 't'))
  
}

################################################################################
# APP
################################################################################

shinyApp(ui, server)

