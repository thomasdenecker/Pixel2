################################################################################
# Pixel2
# Thomas DENECKER
# 09 & 10 /2018
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

header <- dashboardHeader(title = "Pixel2")
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(useShinyjs(), 
                      useShinyalert(),
                      tags$head(tags$link(href = "Images/pixel_icon.png",
                                          rel ="icon", type="image/png")),
                      # Add css style
                      tags$head(HTML('<link rel="stylesheet" type="text/css"
                                     href="style.css" />')), 
                      tags$style(HTML(".sliderStyle .irs-single, .sliderStyle .irs-bar-edge, .sliderStyle .irs-bar, .sliderStyle .irs-from, .sliderStyle .irs-to, .sliderStyle .irs-single {background: red}
                                      .sliderStyle .irs-bar {border-color: red;}")), 
                      tags$head(tags$script(HTML("$(document).on('click', '.autoname', function () {
                                Shiny.onInputChange('last_btn',this.id);
                                                  });"))),
                      tags$head(tags$script(HTML("$(document).on('change', '.dynamicSI select', function () { 
                                Shiny.onInputChange('last_SI',this.id);
                                Shiny.onInputChange('lastSelect', Math.random());
                                                 });"))),
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
  datasourceRV = reactiveValues()
  rv <- reactiveValues()
  SEARCH_RV <- reactiveValues()
  TAG <- reactiveValues()
  TAG$MODIF_PIXELSET_TABLE_SELECTED = cbind(row = NA, col = NA)
  MAJ <- reactiveValues()
  file <- reactiveValues()
  file$extract = F
  
  MAJ$value = 0
  
  #=============================================================================
  # MAJ Values
  #=============================================================================
  
  observeEvent(MAJ$value, {
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    REQUEST_Info = paste0("select DISTINCT PS.id as",'"',"ID",'"',", PS.name as ",'"',"Name",'"', ", species.name as ",'"',"Species",'"',", OmicsUnitType.name as ",'"',"Omics Unit Type",'"',", OmicsArea.name as ",'"',"Omics Area",'"',", pixeler.user_name as ",'"',"Pixeler",'"', ", datasource.name as ",'"',"Datasource",'"', ", analysis.description as ",'"',"Analysis",'"',", experiment.description as ",'"',"Experiment",'"',"
                            from pixelset PS, analysis, Analysis_Experiment AE, experiment, strain, species, OmicsArea, Submission, pixeler, pixel, OmicsUnitType, datasource
                          where PS.id_analysis = analysis.id
                          and PS.id = pixel.pixelset_id
                          and pixel.omicsunittype_id = OmicsUnitType.id
                          and analysis.id = AE.id_analysis
                          and AE.id_experiment = experiment.id
                          and experiment.strainId = strain.id
                          and experiment.DataSourceId = datasource.id
                          and strain.species_id = species.id
                          and experiment.omicsAreaid = OmicsArea.id
                          and PS.id_submission = Submission.id
                          and Submission.pixeler_user_id = pixeler.id
                          ;")
    
    PIXELSETLIST_RV$info=dbGetQuery(con,REQUEST_Info)
    datasourceRV$tab = dbGetQuery(con, "SELECT DataSource.name, DataSource.description, DataSource.published, DataSource.url,
                                      pixelset.id, pixelset.name, PixelSet.description, omicsarea.name
                                FROM Datasource, experiment, analysis_experiment,pixelset, OmicsArea
                                WHERE experiment.omicsareaid = OmicsArea.id 
                                AND experiment.DataSourceId = DataSource.id
                                AND analysis_experiment.id_experiment = experiment.id
                                AND analysis_experiment.id_analysis = pixelset.id_analysis
                                ;")
    
    DASHBOARD_RV$PIXELSET = dbGetQuery(con,"SELECT count(*) from pixelset;")[1,1]
    DASHBOARD_RV$PIXEL = dbGetQuery(con,"SELECT count(*) from pixel;")[1,1]
    DASHBOARD_RV$CF = dbGetQuery(con,"SELECT count(*) from chromosomalfeature;")[1,1]
    DASHBOARD_RV$PixelSetTABLE = dbGetQuery(con,"SELECT * FROM pixelset order by id DESC LIMIT 10;")
    DASHBOARD_RV$OUT = dbGetQuery(con, "SELECT OUT.name, count(*) FROM Pixel, omicsunittype OUT WHERE out.id = omicsunittype_id group by OUT.name;")
    DASHBOARD_RV$Species = dbGetQuery(con, "SELECT species.name, count(*) FROM pixel, chromosomalfeature CF, species WHERE cf_feature_name = feature_name and CF.species_id = species.id group by species.name;")
    
    SubFolder$Tab = dbGetQuery(con,"select DISTINCT submission.id, analysis.description, experiment.description, pixeler.user_name 
                                 from submission, pixelset, experiment, analysis_experiment, analysis, pixeler 
                                 where pixelset.id_submission = submission.id 
                                 and submission.pixeler_user_id = pixeler.id
                                 and pixelset.id_analysis = analysis.id
                                 and analysis_experiment.id_analysis = analysis.id
                                 and analysis_experiment.id_experiment = experiment.id ;")
    
    SubFolder$TabModif =  dbGetQuery(con,"select DISTINCT submission.id, analysis.description, 
                                            experiment.description,submission.status, strain.name, omicsunittype.name, omicsarea.name
                                     from submission, pixelset, experiment, analysis_experiment, analysis, strain, pixel, omicsunittype, omicsArea
                                     where pixelset.id_submission = submission.id 
                                     and pixelset.id_analysis = analysis.id
                                     and analysis_experiment.id_analysis = analysis.id
                                     and analysis_experiment.id_experiment = experiment.id
                                     and experiment.strainId = strain.id
                                     and pixel.pixelSet_id = pixelset.id
                                     and omicsunittype.id = pixel.OmicsUnitType_id
                                     and experiment.omicsAreaid = omicsarea.id;")
    
    if(!is.null(SubFolder$TabModif) && nrow(SubFolder$TabModif) !=0){
      colnames(SubFolder$TabModif) = c("ID", "Analysis description", "Experiment description", "Validated?", "Strain", "OmicsUnitType", "OmicsArea")
    }
    
    # Reinit search values
    CF$name = NULL
    TAG$NAME = NULL
    SEARCH_RV$PIXELSET = NULL
    
    dbDisconnect(con)
  })
  
  #=============================================================================
  # END Reactive values 
  #=============================================================================
  
  login <- div( class="authenfiion", 
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
      on.exit(dbDisconnect(con))
      
      if(input$USER != "" & input$PW != ""){
        RESULT_REQUEST = dbGetQuery(con, REQUEST)
        
        if(nrow(RESULT_REQUEST) != 0 ){
          USER$Logged <- TRUE
          USER$infos <- RESULT_REQUEST[1,]
          USER$UserType <- RESULT_REQUEST[1,7]
          REQUEST = "SELECT * FROM pixeler;"
          pg <- dbDriver("PostgreSQL")
          con <- dbConnect(pg, user="docker", password="docker",
                           host=ipDB, port=5432)
          on.exit(dbDisconnect(con))
          
          USERS$infos = dbGetQuery(con, REQUEST)
          dbDisconnect(con)
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
                      menuItem("Information", tabName = "Information", icon = icon("info-circle"),
                               startExpanded = F,
                               menuSubItem("A PixelSet", tabName = "PixelSet"),
                               menuSubItem("A chromosomal feature", tabName = "CF_item"),
                               menuSubItem("A tag", tabName = "Tags")
                      ),
                      
                      menuItem("Database exploration", tabName = "Explorer", icon = icon("search"),
                               startExpanded = F,
                               menuSubItem("Search by PixelSets", tabName = "PixelSetList"),
                               menuSubItem("Search by submissions", tabName = "submissionFolder"),
                               menuSubItem("Search by datasource", tabName = "datasourceList")
                      ),
                      menuItem("Data integration", tabName = "PixelSetExplo", icon = icon("align-justify")),
                      
                      menuItem("Manage PixelSets", tabName = "ManagePixelSets", icon = icon("folder"),
                               startExpanded = F,
                               menuSubItem("Manage PixelSets", tabName = "PixelSetsAdmin"), 
                               menuSubItem("Manage submissions", tabName = "SubmissionsAdmin")     
                      ),
                      
                      menuItem("Manage annotations", tabName = "ManageAnnotations", icon = icon("pencil"),
                               startExpanded = F,
                               menuSubItem("Chromosomal feature", tabName = "Annotation"),
                               menuSubItem("Data source", tabName = "AddDataSource"),
                               menuSubItem("Omics area", tabName = "AddOmicsArea"),
                               menuSubItem("Omics unit type", tabName = "AddOUT"), 
                               menuSubItem("Species & strains", tabName = "AddSpecies"),
                               menuSubItem("Tags", tabName = "TagAdmin")
                      ),
                      
                      menuItem("Administration", tabName = "Administration", icon = icon("wrench"),
                               startExpanded = F,
                               menuSubItem("Manage Pixelers", tabName = "Pixeler") 
                      ),
                      menuItem("Pixeler information", tabName = "Profile", icon = icon("user")),
                      h4(class ='sideBar',"Quick searches"),
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
          sidebarMenu(id = "tabs",
                      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard"), selected = T),
                      menuItem("Information", tabName = "Information", icon = icon("info-circle"),
                               startExpanded = F,
                               menuSubItem("A PixelSet", tabName = "PixelSet"),
                               menuSubItem("A chromosomal feature", tabName = "CF_item"),
                               menuSubItem("A tag", tabName = "Tags")
                      ),
                      
                      menuItem("Database exploration", tabName = "Explorer", icon = icon("search"),
                               startExpanded = F,
                               menuSubItem("Search by PixelSets", tabName = "PixelSetList"),
                               menuSubItem("Search by submissions", tabName = "submissionFolder")
                      ),
                      menuItem("Data integration", tabName = "PixelSetExplo", icon = icon("align-justify")),
                      menuItem("Pixeler information", tabName = "Profile", icon = icon("user")),
                      h4(class ='sideBar',"Quick searches"),
                      tags$hr(class= "sideBar"),
                      h5(class= "sideBar", "Chromosomal feature"),
                      sidebarSearchForm(textId = "searchCF", buttonId = "searchButtonCF",label = "CAGL0A01243g..."),
                      h5(class= "sideBar", "Tag"),
                      sidebarSearchForm(textId = "searchTag", buttonId = "searchButtonTag",label = "Limma"),
                      h5(class= "sideBar", "PixelSet"),
                      sidebarSearchForm(textId = "searchPS", buttonId = "searchButtonPS",label = "PixelSet_2018-10-22_09-36-37_1...")
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
        # DASHBOARD
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
                        h2(class="center","Pixeler information"), br(),
                        DT::dataTableOutput('pixelerInfo')
                    )
                )
            )
          ),
          
          
          fluidRow(
            div(class="col-sm-12",
                div(class="box box-primary",
                    div(class="box-body",
                        h2(class="center","Last imported PixelSets"), br(),
                        DT::dataTableOutput('PixelSetInfo')
                    )
                )
            )
          )
          
        ),
        
        ########################################################################
        # END DASHBOARD
        ########################################################################
        
        ########################################################################
        # SUBMISSION
        ########################################################################
        
        tabItem(
          tabName = "Submissions", 
          
          h2("Submissions"),
          fluidRow( class="box-submission",
                    box( 
                      title = "Import zip", status = "danger", solidHeader = TRUE,
                      collapsible = T, collapsed =T, width = 12,
                      p(class="info","In this section, you can select a zip generated by a Pixel2 instance. All the information will be 
                        added to the submission sheet. All that remains is to add the tags, make corrections if necessary and submit."),
                      h4("Select a zip folder"),
                      fileInput("zip",NULL,
                                multiple = FALSE,
                                accept = ".zip"),
                      disabled(actionButton("extractZip",label = "Extract",icon = icon('unlock')))
                    )
          ),
          fluidRow( class="box-submission",
                    box( 
                      title = "1- Experiment", status = "danger", solidHeader = TRUE,
                      collapsible = TRUE,
                      p(class="info","This section describes the experimental conditions that were applied to obtain the secondary 
                        datafile (see section 'Analysis' below). Note that these experiments can be already published (in this situation a DOI is required) 
                        or not (in this situation a laboratory has to be specified)."),
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
                      textOutput("submission_Analysis_notebook_name"),
                      
                      h4('Secondary data file'),
                      fileInput("submission_Analysis_secondary_data",label = NULL,
                                buttonLabel = "Browse...",
                                placeholder = "No file selected"),
                      textOutput("submission_Analysis_secondary_data_name"),
                      
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
        # END SUBMISSION
        ########################################################################
        
        ########################################################################
        # PIXELSET
        ########################################################################
        
        #=======================================================================
        # Tab content : PixelSet
        #=======================================================================
        tabItem(
          tabName = "PixelSet", 
          h2("PixelSet"),
          div( class = "PixelSet",
               fluidRow(
                 
                 column(6,
                        fluidRow(
                          h3("Annotations from Pixeler"),
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
                        fluidRow(div(class= "search",h3("Graphics"))), 
                        fluidRow(
                          htmlOutput("PixelSetHistoValue")
                        ), 
                        fluidRow(
                          htmlOutput("PixelSetHistoQS")
                        ),
                        fluidRow(div(class= "search",
                                     h3("Search for a list of chromosomal features"),
                                     p("To search a list of chromosomal features, separate them by';' To return to the complete list of genes, click on the clear button."),
                                     tags$textarea(id = "PS_searchGenelist", rows = 5),
                                     actionButton("PS_searchGenelist_clear_btn",label = "Clear"),
                                     actionButton("PS_searchGenelist_btn",label = "Search")
                        )
                        
                        )
                 )
               ),
               fluidRow(
                 h3("Pixels"),
                 DTOutput("PixelSet_explo_Pixel")
               )
          )),
        
        #=======================================================================
        # Tab content : PixelSetList
        #=======================================================================
        
        tabItem(
          tabName = "PixelSetList", 
          h2("PixelSet List"),
          fluidRow(
            column(12,
                   h3(class="title-pixelset","Current selection"),
                   verbatimTextOutput('PixelSetRowSelected'),
                   actionButton(class="pull-right PS-btn","PixelSetExploreBtn", "Integration of Multi PixelSets"),
                   actionButton(class="pull-right PS-btn","PixelSetExploreSelectAll", "Select all"),
                   actionButton(class="pull-right PS-btn", "PixelSetExploreDeselectAll", "Deselect all")
            )
          ),
          fluidRow(
            column(12,
                   h3(class="title-pixelset","Filter by tags"),
                   uiOutput("PixelSetTags")
            )
          ),
          fluidRow(
            column(12,
                   h3(class="title-pixelset","Available pixelset"),
                   p(class="info", "Click on the lines to select one or more Pixelsets."),
                   DTOutput("PIXELSETLIST_tab")
            )
          )
        ),
        
        #=======================================================================
        # Tab content : PixelSet Exploration
        #=======================================================================
        
        tabItem(
          tabName = "PixelSetExplo", 
          h2("Integration of Multi PixelSets"),
          fluidRow(
            column(12,
                   h3("List of selected PixelSets",  class= "title-pixelset"),
                   DTOutput("PSExploContent"),
                   h3("Graphics",  class= "title-pixelset"),
                   uiOutput("PSExploUI")      
            )),
          fluidRow(
            column(12,
                   h3("Intersection between chromosomal features",  class= "title-pixelset"),
                   plotOutput("UpsetR")
            )
          ),
          
          fluidRow(
            column(12, 
                   h3("Filter",  class= "title-pixelset"),
                   p(class="info","Filter with regex (qualitative data) and threshold (quantitative data)."),
                   sidebarLayout(
                     sidebarPanel(
                       actionButton("add_filter_btn", "Add new filter",icon = icon("plus-circle")),
                       actionButton("rm_filter_btn", "Remove last filter", icon = icon("minus-circle")),
                       tags$br(),
                       radioGroupButtons(inputId = "joinType", 
                                         label = "Join type :", choices = c("Full", 
                                                                            "Inner"), justified = TRUE)
                     ),
                     mainPanel(
                       uiOutput("textbox_ui")
                     )
                   ),
                   actionButton("filter_clear_btn", "Clear", icon = icon("trash")),
                   actionButton("filter_btn", "Filter", icon = icon("filter"))
            )
          ),
          
          fluidRow(
            column(12,
                   h3("Search for a list of chromosomal features",  class= "title-pixelset"),
                   p("To search a list of chromosomal features, separate them by';' To return to the complete list of genes, click on the clear button."),
                   tags$textarea(id = "MPS_searchGenelist", rows = 5),
                   actionButton("MPS_searchGenelist_clear_btn",label = "Clear"),
                   actionButton("MPS_searchGenelist_btn",label = "Search")
            )
          ),
          
          fluidRow(
            column(12, 
                   h3("Pixels",  class= "title-pixelset"),
                   downloadButton('MPS_export_csv', 'CSV'),
                   downloadButton('MPS_export_tsv', 'TSV'),
                   downloadButton('MPS_export_excel', 'EXCEL'),
                   br(),br(),
                   DTOutput("PSExploTab")
            )
          ),
          fluidRow(
            column(12, 
                   h3("Extract gene list",  class= "title-pixelset"),
                   p(class="info","List of unique genes selected in the table.The list is formatted for Pixel2 search areas."), 
                   textOutput('geneListMPS'),
                   tags$br(),
                   htmlOutput('geneListSizeMPS')
            )
          )
          
        ),
        
        ########################################################################
        # END PIXELSET
        ########################################################################
        
        ########################################################################
        # EXPLORE
        ########################################################################
        
        #=======================================================================
        # Tab content : Submission folder
        #=======================================================================
        tabItem(
          tabName = "datasourceList", 
          h2("Datasource list"),
          h3(class = "h3-style","Datasource overview"), 
          div( class = "margeProfile",
               fluidRow(
                 DTOutput("DatasourceTab")
               )
          )
        ),
        
        
        #=======================================================================
        # Tab content : Submission folder
        #=======================================================================
        tabItem(
          tabName = "submissionFolder", 
          h2("Submission list"),
          h3(class = "h3-style","Submission overview"), 
          div( class = "margeProfile",
               fluidRow(
                 DTOutput("submissionFolderTab")
               )
          ),
          div( class = "margeProfile",
               fluidRow(
                 uiOutput("submissionFolderInfo"),
                 tabsetPanel(id = "tab_sub_PS")
               )
          )
        ),
        
        #=======================================================================
        # Tab content : Tags
        #=======================================================================
        tabItem(
          tabName = "Tags", 
          htmlOutput("tagName"),
          div( class = "margeProfile",
               fluidRow(
                 h3(class="h3-style","Description"),
                 textOutput("TagDecription"),     
                 h3(class="h3-style","Analysis with this tag"),
                 DTOutput("Tag_analysis"),
                 h3(class="h3-style","Experiment with this tag"),
                 DTOutput("Tag_experiment"),
                 h3(class="h3-style","All tags"),
                 DTOutput("Tag_All"),
                 h3(class="h3-style","Graphical representation"),
                 htmlOutput("TagBar")
                 
               ))),
        
        #=======================================================================
        # Tab content : Chromosomal feature
        #=======================================================================
        tabItem(
          tabName = "CF_item", 
          uiOutput("CF_title"),
          fluidRow(
            div(class="col-lg-6",
                uiOutput("CF_information")
            ),
            div(class="col-lg-6",
                h2(class="title-cf","Supplementary information"),
                #tabsetPanel(id = "tab_sup_annot")
                htmlOutput("sup_annot")
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
        
        ########################################################################
        # END EXPLORE
        ########################################################################
        
        ########################################################################
        # Add information
        ########################################################################
        
        #=======================================================================
        # Tab content : Add omicsUnitType
        #=======================================================================
        
        tabItem(
          tabName = "AddOUT", 
          h2("Omics unit type"),
          fluidRow(
            div(class = "table_style",
                h3(class ="h3-style","Add a new type of omics unit"),
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
                
                h3(class ="h3-style","Modify existing types of omics units"),
                DTOutput('DT_AddOUT'),
                h3(class ="h3-style", "Delete omics unit types"),
                fluidRow(class= "tableTitle-left",
                         column(3, "Omics unit types to be deleted"), 
                         column(3, ""), 
                         column(3, ""),
                         column(3, "")
                ),
                fluidRow(
                  column(3,div(class = "inputNew",uiOutput("Delete_OUT"))),
                  column(3,div(class = "inputNew",actionButton('Delete_OUT_btn','Remove Omics unit type', icon = icon("minus")))),
                  column(3,""),
                  column(3,"")
                ) 
            )
          )),
        
        #=======================================================================
        # Tab content : Add dataSource
        #=======================================================================
        
        tabItem(
          tabName = "AddDataSource", 
          h2("Source of dataset"),
          fluidRow(
            div(class = "table_style",
                h3(class ="h3-style","Add a new source of dataset"),
                fluidRow(class= "tableTitle",
                         column(2, "Name"), 
                         column(2, "Description"), 
                         column(2, "Published"),
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
                
                h3(class ="h3-style","Modify existing sources of dataset"),
                DTOutput('DT_AddDataSource'))
            
          )),
        
        #=======================================================================
        # Tab content : Add Omics Area
        #=======================================================================
        
        tabItem(
          tabName = "AddOmicsArea", 
          h2("Omics area"),
          fluidRow(
            div(class = "table_style",
                h3(class ="h3-style","Add a new omics area"),
                fluidRow(class= "tableTitle-left",
                         column(3, "Name"), 
                         column(3, "Select path"), 
                         column(3, "Description"),
                         column(3, "")
                ),
                fluidRow(
                  column(3,div(class = "inputNew",textInput("Add_OmicsArea_name", NULL, placeholder = "Name"))),
                  column(3,div(class = "inputNew",uiOutput("Add_OmicsArea_path"))),
                  column(3,div(class = "inputNew",textInput("Add_OmicsArea_description", NULL, placeholder = "Description"))),
                  column(3,div(class = "inputNew",actionButton('Add_OmicsArea_btn','Add OmicsArea', icon = icon("plus-circle"))))
                ),
                
                h3(class ="h3-style","Modify existing Omics area"),
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
                h3(class ="h3-style", "Manage omics area"),
                fluidRow(class= "tableTitle-left",
                         column(3, "Omics area to be deleted"), 
                         column(3, ""), 
                         column(3, ""),
                         column(3, "")
                ),
                fluidRow(
                  column(3,div(class = "inputNew",uiOutput("Delete_branch_OmicsArea"))),
                  column(3,div(class = "inputNew",actionButton('Delete_branch_OmicsArea_btn','Remove OmicsArea', icon = icon("minus")))),
                  column(3,""),
                  column(3,"")
                ) ,
                
                div(class= "tree",
                    h3(class = 'center', "Organization of omics area"),
                    htmlOutput("treeOmicsArea")
                )
                
                
            )
          )),
        
        
        #=======================================================================
        # Tab content : Annotation
        #=======================================================================
        
        tabItem(
          tabName = "Annotation", 
          h2("Chromosomal feature"),
          h3(class ="h3-style","Help"),
          fluidRow(
            column(3,
                   h4("Datatable composition"),
                   p("The table must be composed of 8 columns: 
                      feature name (i.e : YAL068C), gene name (i.e PAU8), chromosome, start 
                      coordinate, stop coordinate, strand, description, species and
                      url (ie. for SGD it's ' https://www.yeastgenome.org/locus/' + SGD id : 'https://www.yeastgenome.org/locus/S000002142'). 
                      The imported chromosomal features will have as a reference database the source you selected above.")
            ),
            column(9, 
                   h4("Example"),
                   dataTableOutput(outputId = "example_CF")
            )
          ),
          
          h3(class ="h3-style","1- Sources of annotations"),
          fluidRow( class='border-between ', 
                    column(6,align="center",
                           h3("Existing annotation sources"),
                           uiOutput("SSUI"),
                           htmlOutput("DescriCFSource")
                    ),
                    column(6,align="center",
                           h3("Add a new source of annotations"),
                           h4("Name"),
                           textInput("CFSourceName",NULL ),
                           h4("Abbreviation"),
                           textInput("CFAbbreviation",NULL ),
                           h4("Description"),
                           textAreaInput("CFSourceDescription", NULL, resize = "vertical"),
                           h4("URL"),
                           textInput("CFSourceURL", NULL),
                           actionButton("addCFSource", "Add source")
                    )
          ),
          
          
          h3(class ="h3-style","2- Import annotation file"),
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
                   h3("File preview"),
                   dataTableOutput(outputId = "contents_CF"))
          ),
          actionButton(inputId = "ImportCF", label = "Import", class= "myBtn" , icon = icon("upload"))
          
        ),
        
        #=======================================================================
        # Tab content : Species & strains
        #=======================================================================
        
        tabItem(
          tabName = "AddSpecies",
          h2("Species"),
          fluidRow(
            div(class = "table_style",
                h3(class ="h3-style","Add a new species"),
                fluidRow(class= "tableTitle",
                         column(2, "Name"),
                         column(2, "Description"),
                         column(2, "URL"),
                         column(6, "")
                ),
                fluidRow(
                  column(2,div(class = "inputNew",textInput("Name_Species", NULL, placeholder = "Name"))),
                  column(2,div(class = "inputNew",textInput("Description_Species", NULL, placeholder = "Description"))),
                  column(2,div(class = "inputNew",textInput("URL_Species", NULL, placeholder = "url"))),
                  column(6,div(class = "inputNew",actionButton('addSpecies_btn','Add species', icon = icon("plus-circle"))))
                ),
                h3(class ="h3-style","Modify existing species"),
                DTOutput('DT_AddSpecies'),
                
                h3(class ="h3-style", "Delete species"),
                fluidRow(class= "tableTitle-left",
                         column(3, "Species to be deleted"), 
                         column(3, ""), 
                         column(3, ""),
                         column(3, "")
                ),
                fluidRow(
                  column(3,div(class = "inputNew",uiOutput("Delete_species"))),
                  column(3,div(class = "inputNew",actionButton('Delete_species_btn','Remove species', icon = icon("minus")))),
                  column(3,""),
                  column(3,"")
                )
            )
            
          ),
          
          h2("Strains"),
          fluidRow(
            div(class = "table_style",
                h3(class ="h3-style","Add a new strain"),
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
                h3(class ="h3-style","Modify existing strains"),
                DTOutput('DT_AddStrain'),
                
                h3(class ="h3-style", "Delete strain"),
                fluidRow(class= "tableTitle-left",
                         column(3, "Strain to be deleted"), 
                         column(3, ""), 
                         column(3, ""),
                         column(3, "")
                ),
                fluidRow(
                  column(3,div(class = "inputNew",uiOutput("Delete_strain"))),
                  column(3,div(class = "inputNew",actionButton('Delete_strain_btn','Remove strain', icon = icon("minus")))),
                  column(3,""),
                  column(3,"")
                )
                
            )
            
          )
          
        ),
        
        ########################################################################
        # END ADD INFORMATION
        ########################################################################
        
        ########################################################################
        # ADMINISTRATION
        ########################################################################
        
        #=======================================================================
        # Tab content : Pixeler
        #=======================================================================
        
        tabItem(
          tabName = "Pixeler", 
          h2("Pixeler"),
          div(class = "table_style", 
              h3(class ="h3-style","Modify pixeler information"),
              DTOutput('adminUsers'),
              br(),
              actionButton('removeUser', class = "pull-right",
                           label = "Remove user (0)", 
                           icon = icon("minus-circle")),
              br(),
              h3(class ="h3-style","New pixeler"),
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
        
        #=======================================================================
        # Tab content : SubmissionsAdmin
        #=======================================================================
        
        
        tabItem(
          tabName = "SubmissionsAdmin", 
          h2("Submission"),
          fluidRow(
            column(12,
                   h3(class ="h3-style","Delete previous submission(s)"),
                   p(class="info", "Select one or more submissions from the table and click Remove."),
                   DTOutput('SubmissionsAdminTab'),
                   br(),
                   actionButton('removeSubmission', class = "pull-right",
                                label = "Remove submission (0)", 
                                icon = icon("minus-circle"))
            )
          ),
          
          fluidRow(
            column(12,
                   h3(class ="h3-style","Modify previous submission"),
                   p(class="info", "Select one of the lines to activate the modifiion"),
                   sidebarLayout(
                     sidebarPanel(
                       h5("Submission id"),
                       uiOutput("SubmissionsAdminTab_modify_ID"),
                       h5("Analysis description"),
                       uiOutput("SubmissionsAdminTab_modify_DescriptionAnalysis"),
                       h5("Experiment description"),
                       uiOutput("SubmissionsAdminTab_modify_DescriptionExperiment"),
                       h5("Status"),
                       uiOutput("SubmissionsAdminTab_modify_STATUS"),
                       h5("Strain"),
                       uiOutput("SubmissionsAdminTab_modify_Strain"), 
                       h5("OmicsUnitType"),
                       uiOutput("SubmissionsAdminTab_modify_OmicsUnitType"),
                       h5("OmicsArea"),
                       uiOutput("SubmissionsAdminTab_modify_OmicsArea"),
                       div(class="all-size",actionButton("SubmissionsAdminTab_modify_CHANGE", "Modify", class="right"))
                       
                     ),
                     mainPanel(
                       DTOutput('SubmissionsAdminTab_modify')
                     )
                   )
            )
          ),
          
          fluidRow(
            column(12,
                   h3(class ="h3-style","Update all meta"),
                   p(class="info", "After modifications in Add information, you can update all meta."),
                   actionButton("updateAllMeta", "Update")
            )
          )
        ), 
        #=======================================================================
        # Tab content : TagAdmin
        #=======================================================================
        
        tabItem(
          tabName = "TagAdmin", 
          h2("Tag"), 
          fluidRow(
            column(12,
                   h3(class ="h3-style","Add a new tag"),
                   div(class="inline-block-element",textInput("ManageTag_NewName", "Name", placeholder = "Name")),
                   div(class="inline-block-element",textInput("ManageTag_NewDescription", "Description", placeholder = "Description")),
                   div(class="inline-block-element",actionButton("ManageTag_Newbtn", "Add a new tag")),
                   
                   h3(class ="h3-style","Association between Tags and PixelSets"),
                   fluidRow(
                     column(4,
                            h4("Select a tag"),
                            uiOutput("tagModifUi"), 
                            h4("Tag description"),
                            verbatimTextOutput("tagModifDescription")
                     ),
                     column(8,
                            DTOutput('TagAdmin_Modify_tab')
                     )
                   ),
                   tags$br(),
                   actionButton("TagAdmin_Modify_btn", "Modify", class="modifif-btn pull-right"),
                   tags$br(class="clearBoth"),
                   h3(class ="h3-style","Remove a tag"),
                   div(class="inline", uiOutput("tagDeleteUi")),
                   div(class="inline",actionButton("AdminTag_delete_btn", "Delete", class="modifif-btn")),
                   tags$br(class="clearBoth"),
                   h3(class ="h3-style","Modify existing tags"),
                   p(class="info", "Select one of the lines to activate the modification"),
                   sidebarLayout(
                     sidebarPanel(
                       h5(class="bold","Tag id"),
                       uiOutput("TagAdminTab_modify_ID"), 
                       h5(class="bold","Name"),
                       uiOutput("TagAdminTab_modify_name"),
                       h5(class="bold","Description"),
                       uiOutput("TagAdminTab_modify_Description"),
                       
                       div(class="all-size",actionButton("TagAdminTab_modify_CHANGE", "Modify", class="right"))
                       
                     ),
                     mainPanel(
                       DTOutput('TagAdminTab_Modify')
                     )
                     
                   ) 
            )
          )
        ),
        
        #=======================================================================
        # Tab content : PixelSetsAdmin
        #=======================================================================
        
        
        tabItem(
          tabName = "PixelSetsAdmin", 
          h2("PixelSets"),
          fluidRow(
            column(12,
                   h3(class ="h3-style","Delete PixelSet(s)"),
                   p(class="info", "Select one or more PixelSets from the table and click Remove."),
                   DTOutput('PixelSetsAdminTab'), 
                   br(),
                   actionButton('removePixelSets', class = "pull-right",
                                label = "Remove PixelSets (0)", 
                                icon = icon("minus-circle"))
            )
          ),
          
          fluidRow(
            column(12,
                   h3(class ="h3-style","Modify existing PixelSets"),
                   p(class="info", "Select one of the lines to activate the modification"),
                   sidebarLayout(
                     sidebarPanel(
                       h5(class="bold","PixelSet id"),
                       uiOutput("PixelSetAdminTab_modify_ID"),
                       h5(class="bold","Name"),
                       uiOutput("PixelSetAdminTab_modify_name"),
                       h5(class="bold","Description"),
                       uiOutput("PixelSetAdminTab_modify_Description"),
                       
                       div(class="inline", h5(class="bold","Tags")),
                       div(class="inline tag-inline",dropdownButton(
                         h4("Add new tag"), 
                         textInput("PSModify_tags_NewName", NULL, placeholder = "Name"),
                         textInput("PSModify_tags_NewDescription", NULL, placeholder = "Description"),
                         actionButton("PSModify_Exp_tags_Newbtn", "Add tag"), size = "xs",
                         circle = TRUE, status = "danger", icon = icon("plus"), width = "300px",
                         tooltip = tooltipOptions(title = "Add new tag")
                       )),
                       tags$br(class="clearBoth"),
                       
                       p(class="italic", "All tags of pixelSet with the same experience and analysis will be modified."),
                       fluidRow(
                         column(6,
                                h5(class="bold","Tag analysis"),
                                uiOutput("PixelSetAdminTab_modify_TagA")),
                         column(6,
                                h5(class="bold","Tag experiment"),
                                uiOutput("PixelSetAdminTab_modify_TagE"))
                       ),
                       div(class="all-size",actionButton("PixelSetAdminTab_modify_CHANGE", "Modify", class="right"))
                       
                     ),
                     mainPanel(
                       DTOutput('PixelSetsAdminTab_Modify')
                     )
                   )
            )
          )
        ), 
        
        #=======================================================================
        # Tab content : Profile
        #=======================================================================
        
        tabItem(
          tabName = "Profile", 
          h2("Profile"),
          fluidRow(
            uiOutput("Profile")
          )
        )
        
        
        ########################################################################
        # END ADMINISTRATION
        ########################################################################
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
    if(!is.null(DASHBOARD_RV$OUT) && nrow(DASHBOARD_RV$OUT) != 0){
      gvisPieChart(DASHBOARD_RV$OUT,options=list(tooltip = "{text:'percentage'}"))
    }else {
      NULL
    }
    
  })
  
  output$PixelsBySpecies <- renderGvis({
    if(!is.null(DASHBOARD_RV$Species) && nrow(DASHBOARD_RV$Species) != 0){
      gvisPieChart(DASHBOARD_RV$Species,options=list(tooltip = "{text:'percentage'}"))
    }else {
      NULL
    }
  })
  
  output$UsersMap <- renderGvis({
    REQUEST_MAP = "SELECT lab_country, count(*) FROM pixeler GROUP BY lab_country ;"
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    MapTable = dbGetQuery(con, REQUEST_MAP)
    dbDisconnect(con)
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
           h3(class ="h3-style","General information"),
           p(tags$b("First names :"), USER$infos[1,2]),
           p(tags$b("Last names :"), USER$infos[1,3]),
           p(tags$b("User names :"), USER$infos[1,4]),
           p(tags$b("Email :"), USER$infos[1,5]),
           p(tags$b("Password (encrypted) :"), USER$infos[1,6]),
           p(tags$b("User type :"), USER$infos[1,7]),
           p(tags$b("Country :"), USER$infos[1,8]),
           p(tags$b("Creation date :"), USER$infos[1,9]),
           
           h3(class ="h3-style","Change the password"),
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
      on.exit(dbDisconnect(con))
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
  # Admin Submission
  #-----------------------------------------------------------------------------
  
  #.............................................................................
  # Modify
  #.............................................................................
  submissionModify = reactiveValues()
  
  #.  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
  # Get Submission ID
  #.  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
  
  output$SubmissionsAdminTab_modify <- renderDT(SubFolder$TabModif, selection = 'single',
                                                options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  observeEvent(is.null(input$SubmissionsAdminTab_modify_rows_selected),{
    if(!is.null(input$SubmissionsAdminTab_modify_rows_selected)){
      submissionModify$id = SubFolder$TabModif[input$SubmissionsAdminTab_modify_rows_selected,1]
      enable("SubmissionsAdminTab_modify_CHANGE")
    } else {
      submissionModify$id = ""
      disable("SubmissionsAdminTab_modify_CHANGE")
    }
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    submissionModify$strainChoices = dbGetQuery(con,paste0("select name 
                                        from strain 
                                        where species_id = (
                                        select DISTINCT species.id
                                        from pixelset PS , analysis_experiment AE , experiment E, strain S, species
                                        where PS.id_submission = '",submissionModify$id,"'
                                        and PS.id_analysis = AE.id_analysis
                                        and AE.id_experiment = E.id
                                        and E.strainId = S.id
                                        and species.id = S.species_id
                                        );"))
    
    submissionModify$Omicsarea = dbGetQuery(con,"Select name from omicsArea;")
    submissionModify$OUT = dbGetQuery(con,"Select name from omicsunittype;")
    dbDisconnect(con)
    
  })
  
  observeEvent(submissionModify$id,{
    if(!is.null(SubFolder$TabModif) && nrow(SubFolder$TabModif) != 0){
      updateTextAreaInput(session,"SubmissionsAdminTab_modify_DescriptionAnalysis_TA", value = SubFolder$TabModif[which(SubFolder$TabModif[,1] == submissionModify$id),2] )  
      updateTextAreaInput(session,"SubmissionsAdminTab_modify_DescriptionExperiment_TA", value = SubFolder$TabModif[which(SubFolder$TabModif[,1] == submissionModify$id),3] )  
      updateSelectInput(session, "SubmissionsAdminTab_modify_Strain_SI", selected = SubFolder$TabModif[which(SubFolder$TabModif[,1] == submissionModify$id),5] )
      updateSelectInput(session, "SubmissionsAdminTab_modify_OmicsUnitType_SI", selected = SubFolder$TabModif[which(SubFolder$TabModif[,1] == submissionModify$id),6] )
      updateSelectInput(session, "SubmissionsAdminTab_modify_OmicsArea_SI", selected = SubFolder$TabModif[which(SubFolder$TabModif[,1] == submissionModify$id),7] )
      
      if(!is.null(SubFolder$TabModif[which(SubFolder$TabModif[,1] == submissionModify$id),4]) && length(SubFolder$TabModif[which(SubFolder$TabModif[,1] == submissionModify$id),4]) != 0){
        if(SubFolder$TabModif[which(SubFolder$TabModif[,1] == submissionModify$id),4]){
          status = "true"
        } else if( ! SubFolder$TabModif[which(SubFolder$TabModif[,1] == submissionModify$id),4]){
          status = "false"
        }
        updateSelectInput(session, "SubmissionsAdminTab_modify_STATUS_SI", choices = c(Validated = "true","Not valideted" ="false"))
      } else {
        status = ""
      }
      updateSelectInput(session, "SubmissionsAdminTab_modify_STATUS_SI", selected = status )
    }
  })
  
  
  #.  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
  # Elements to be modified 
  #.  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
  
  output$SubmissionsAdminTab_modify_ID<-renderUI({
    verbatimTextOutput("SubmissionsAdminTab_modify_ID_VTO", placeholder = TRUE)
  })
  
  output$SubmissionsAdminTab_modify_ID_VTO <- renderText(
    submissionModify$id
  )
  
  output$SubmissionsAdminTab_modify_STATUS<-renderUI({
    selectInput("SubmissionsAdminTab_modify_STATUS_SI", NULL,choices = "" )
  })
  
  output$SubmissionsAdminTab_modify_DescriptionAnalysis<-renderUI({
    if(!is.null(SubFolder$TabModif) && nrow(SubFolder$TabModif) != 0){
      textAreaInput("SubmissionsAdminTab_modify_DescriptionAnalysis_TA",NULL, value = SubFolder$TabModif[which(SubFolder$TabModif[,1] == submissionModify$id),2])
    }
  })
  
  output$SubmissionsAdminTab_modify_DescriptionExperiment<-renderUI({
    if(!is.null(SubFolder$TabModif) && nrow(SubFolder$TabModif) != 0){
      textAreaInput("SubmissionsAdminTab_modify_DescriptionExperiment_TA",NULL, value = SubFolder$TabModif[which(SubFolder$TabModif[,1] == submissionModify$id),3])
    }
  })
  
  output$SubmissionsAdminTab_modify_Strain<-renderUI({
    
    if(nrow(submissionModify$strainChoices) != 0){
      selectInput("SubmissionsAdminTab_modify_Strain_SI",NULL, choices = submissionModify$strainChoices[,"name"] )
    } else {
      selectInput("SubmissionsAdminTab_modify_Strain_SI",NULL, choices = "" )
    }
    
  })
  
  output$SubmissionsAdminTab_modify_OmicsUnitType<-renderUI({
    selectInput("SubmissionsAdminTab_modify_OmicsUnitType_SI",NULL, choices = submissionModify$OUT[,"name"] )
  })
  
  output$SubmissionsAdminTab_modify_OmicsArea<-renderUI({
    selectInput("SubmissionsAdminTab_modify_OmicsArea_SI",NULL, choices = submissionModify$Omicsarea[,"name"] )
  })
  
  # SubmissionsAdminTab_modify_Strain SubmissionsAdminTab_modify_OmicsUnitType SubmissionsAdminTab_modify_OmicsArea
  
  #.  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
  # Run modification
  #.  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
  observeEvent(input$SubmissionsAdminTab_modify_CHANGE,{
    confirmSweetAlert(
      session = session,
      inputId = "confirm_modify_submission",
      type = "warning",
      title = "Want to modify this submission ?",
      text = submissionModify$id ,
      danger_mode = TRUE
    )
  })
  
  observeEvent(input$confirm_modify_submission, {
    if (isTRUE(input$confirm_modify_submission)) {
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      dbGetQuery(con,paste0("update submission set status ='",input$SubmissionsAdminTab_modify_STATUS_SI,"' where id = '",submissionModify$id, "';"))
      
      dbGetQuery(con,paste0("update analysis set description  = '",input$SubmissionsAdminTab_modify_DescriptionAnalysis_TA,"' 
                            where id IN (select DISTINCT id_analysis from pixelset where id_submission = '",submissionModify$id,"')"))
      dbGetQuery(con,paste0("update experiment set description  = '",input$SubmissionsAdminTab_modify_DescriptionExperiment_TA,"' 
                            where id IN (select DISTINCT id_experiment from pixelset , analysis_experiment, experiment 
                            where id_submission = '",submissionModify$id,"' 
                            and pixelset.id_analysis = analysis_experiment.id_analysis 
                            and analysis_experiment.id_experiment = experiment.id );"))
      
      idStrain = dbGetQuery(con,paste0("Select id from strain where name ='",input$SubmissionsAdminTab_modify_Strain_SI,"';"))
      
      dbGetQuery(con,paste0("update experiment set strainId = ",idStrain[1,1]," where id = (select DISTINCT analysis_experiment.id_experiment
                              from pixelset, analysis_experiment
                              where pixelset.id_submission='",submissionModify$id,"'
                              and pixelset.id_analysis = analysis_experiment.id_analysis);"))
      
      idOmicsArea = dbGetQuery(con,paste0("Select id from omicsarea where name ='",input$SubmissionsAdminTab_modify_OmicsArea_SI,"';"))
      
      dbGetQuery(con,paste0("update experiment set omicsAreaid = '",idOmicsArea[1,1],"' where id = (select DISTINCT analysis_experiment.id_experiment
                              from pixelset, analysis_experiment
                              where pixelset.id_submission='",submissionModify$id,"'
                              and pixelset.id_analysis = analysis_experiment.id_analysis);"))
      
      idOmicsUnitType = dbGetQuery(con,paste0("Select id from omicsunittype where name ='",input$SubmissionsAdminTab_modify_OmicsUnitType_SI,"';"))
      dbGetQuery(con,paste0("update pixel set OmicsUnitType_id = ",idOmicsUnitType," where id in (
        select pixel.id from pixel, pixelSet
        where pixel.pixelSet_id = pixelset.id
        and pixelset.id_submission = '",submissionModify$id,"'
      );"))
      
      # Update All
      MAJ$value = MAJ$value + 1
      
      SubFolder$Tab = dbGetQuery(con,"select DISTINCT submission.id, analysis.description, experiment.description, pixeler.user_name 
                                  from submission, pixelset, experiment, analysis_experiment, analysis, pixeler 
                                 where pixelset.id_submission = submission.id 
                                 and submission.pixeler_user_id = pixeler.id
                                 and pixelset.id_analysis = analysis.id
                                 and analysis_experiment.id_analysis = analysis.id
                                 and analysis_experiment.id_experiment = experiment.id ;")
      
      SubFolder$TabModif =  dbGetQuery(con,"select DISTINCT submission.id, analysis.description, 
                                            experiment.description,submission.status, strain.name, omicsunittype.name, omicsarea.name
                                            from submission, pixelset, experiment, analysis_experiment, analysis, strain, pixel, omicsunittype, omicsArea
                                            where pixelset.id_submission = submission.id 
                                             and pixelset.id_analysis = analysis.id
                                             and analysis_experiment.id_analysis = analysis.id
                                             and analysis_experiment.id_experiment = experiment.id
                                       and experiment.strainId = strain.id
                                       and pixel.pixelSet_id = pixelset.id
                                       and omicsunittype.id = pixel.OmicsUnitType_id
                                       and experiment.omicsAreaid = omicsarea.id;")
      
      colnames(SubFolder$TabModif) = c("ID", "Analysis description", "Experiment description", "Validated?", "Strain", "OmicsUnitType", "OmicsArea")
      
      updateMeta(submissionModify$id)
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = "Submission(s) is modified!",
        type = "success"
      )
      
      dbDisconnect(con)
    }
  })
  
  updateMeta <-function(idSubmission){
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    # submissionModify$id
    OUT_inter = dbGetQuery(con,paste0("select DISTINCT omicsunittype.name from pixelset, pixel, omicsunittype 
                                      where pixelset.id_submission ='",idSubmission,"' 
                                      and pixel.pixelSet_id = pixelset.id 
                                      and pixel.OmicsUnitType_id = omicsunittype.id;"))
    
    Info_inter= dbGetQuery(con,paste0("select E.description, EXTRACT(YEAR FROM E.completionDate), OA.name, DS.name, species.name, strain.name, 
                                      A.description, EXTRACT(YEAR FROM a.completionDate), a.notebook_file, a.secondary_data_file, PS.name, PS.description,PS.pixelSet_file, PS.id
                                      from pixelset PS, analysis A, analysis_experiment AE, experiment E, DataSource DS, strain , species, omicsarea OA
                                      where PS.id_submission = '",idSubmission,"'
                                      and PS.id_analysis = A.id
                                      and A.id = AE.id_analysis
                                      and AE.id_experiment = E.id
                                      and E.strainId = strain.id
                                      and E.omicsareaid = OA.id
                                      and E.DataSourceId = DS.id
                                      and strain.species_id = species.id;"))
    
    time = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    
    if( Info_inter[1,9] != ""){
      fileNB = unlist(strsplit(Info_inter[1,9],split = "/"))[length(unlist(strsplit(Info_inter[1,9],split = "/")))]
    } else {
      fileNB = ""
    }
    
    if( Info_inter[1,10] != ""){
      fileSD = unlist(strsplit(Info_inter[1,10],split = "/"))[length(unlist(strsplit(Info_inter[1,10],split = "/")))]
    } else {
      fileSD = ""
    }
    
    md5_notebook = md5sum(paste0("www/",idSubmission,"/", fileNB))
    md5_notebook[is.na(md5_notebook)] = ""
    
    md5_SD = md5sum(paste0("www/",idSubmission,"/", fileSD))
    md5_SD[is.na(md5_SD)] = ""
    
    meta = rbind(
      c("Experiment_description", Info_inter[1,1]),
      c("Experiment_completionDate",Info_inter[1,2]	),
      c("Experiment_omicsArea",	Info_inter[1,3]),
      c("Experiment_omicsUnitType",OUT_inter[1,1]),
      c("Experiment_dataSource",Info_inter[1,4]	),
      c("Experiment_species",Info_inter[1,5]	),
      c("Experiment_strain",Info_inter[1,6]	),
      c("Analysis_description",Info_inter[1,7]	),
      c("Analysis_completionDate",Info_inter[1,8]	),
      c("Analysis_notebookFile",fileNB),
      c("md5_notebook", md5_notebook),
      c("Analysis_secondaryDataFile",	fileSD),
      c("md5_secondaryDataFile", md5_SD)
    )
    
    for(i in 1:nrow(Info_inter)){
      meta = rbind(meta,
                   c(paste0("PixelSet",i,"_name"),Info_inter[i,11]	),
                   c(paste0("PixelSet",i,"_description"),Info_inter[i,12]	),
                   c(paste0("PixelSet",i,"_file"),unlist(strsplit(Info_inter[i,13], "/"))[3]),
                   c(paste0("PixelSet",i,"_md5"),md5sum(paste0("www/",Info_inter[i,13])))
      )          
    }
    
    write.table(meta, paste0("www/Submissions/", idSubmission,"/meta_",time,".txt") ,
                sep = "\t", row.names = F, col.names= F,quote = F)
    
    setwd("www/Submissions/")
    file.remove(paste0(idSubmission,".zip"))
    zip(idSubmission, idSubmission)
    setwd("../..")
    
    dbDisconnect(con)
    
  }
  
  observeEvent(input$updateAllMeta,{
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    submission = dbGetQuery(con,"Select id from submission")
    if(nrow(submission) != 0){
      for(s in submission[,1]){
        updateMeta(s)
      }
      
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = "All Submissions are updated!",
        type = "success"
      )
      
    } else {
      sendSweetAlert(
        session = session,
        title = "Ops!",
        text = "No Submission to update!",
        type = "warning"
      )
    }
    
    dbDisconnect(con)
  })
  
  #.............................................................................
  # Remove submission
  #.............................................................................
  
  output$SubmissionsAdminTab <- renderDT(SubFolder$Tab, selection = 'multiple', 
                                         editable = F ,escape = 3,
                                         options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  
  observeEvent(is.null(input$SubmissionsAdminTab_rows_selected),{
    if(!is.null(input$SubmissionsAdminTab_rows_selected)){
      updateActionButton(session, "removeSubmission", 
                         label = paste0('Remove Submission (',length(input$SubmissionsAdminTab_rows_selected),')'),
                         icon = icon("minus-circle"))
      enable("removeSubmission")
    }else{
      updateActionButton(session, "removeSubmission", 
                         label = 'Remove submission (0)', icon = icon("minus-circle"))
      disable("removeSubmission")
    }
    
  })
  
  observeEvent(input$removeSubmission,{
    
    confirmSweetAlert(
      session = session,
      inputId = "confirm_del_submission",
      type = "warning",
      title = "Want to confirm the following submission(s) ?",
      text = paste(SubFolder$Tab[input$SubmissionsAdminTab_rows_selected,1], collapse = ",") ,
      danger_mode = TRUE
    )
    
  })
  
  observeEvent(input$confirm_del_submission, {
    if (isTRUE(input$confirm_del_submission)) {
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      for(i in input$SubmissionsAdminTab_rows_selected){
        idSubmission = SubFolder$Tab[i,1]
        idExperience = dbGetQuery(con, paste0("select DISTINCT Analysis_Experiment.id_experiment from pixelset,  Analysis_Experiment
                                              where id_Submission = '",idSubmission,"'
                                              and Analysis_Experiment.id_analysis = pixelset.id_analysis;"))
        idAnalysis = dbGetQuery(con, paste0("select DISTINCT id_analysis from pixelset
                                            where id_Submission = '",idSubmission,"'"))
        
        dbGetQuery(con, paste0("delete from Tag_Experiment where id_experiment = '",idExperience,"';"))
        dbGetQuery(con, paste0("delete from Tag_Analysis where id_analysis = '",idAnalysis,"';"))
        dbGetQuery(con, paste0("delete from Analysis_Experiment where id_analysis = '",idAnalysis,"' and id_experiment = '",idExperience,"' ;"))
        dbGetQuery(con, paste0("delete from experiment where id  = '",idExperience,"';"))
        dbGetQuery(con, paste0("delete from pixel where pixelSet_id IN (select id from pixelset where id_Submission = '",idSubmission ,"');"))
        dbGetQuery(con, paste0("delete from pixelset where id IN (select id from pixelset where id_Submission = '",idSubmission ,"');"))
        dbGetQuery(con, paste0("delete from analysis where id  = '",idAnalysis,"';"))
        dbGetQuery(con, paste0("delete from submission where id = '",idSubmission ,"';"))
      }
      
      MAJ$value = MAJ$value + 1
      
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = "Submission(s) deleted !",
        type = "success"
      )
      
      dbDisconnect(con)
    }
  })
  
  #-----------------------------------------------------------------------------
  # Admin Tag
  #-----------------------------------------------------------------------------
  
  
  observeEvent(input$ManageTag_Newbtn,{
    
    REQUEST_EXISTING = paste0("SELECT *
                              FROM tag
                              WHERE name = '",tolower(input$ManageTag_NewName),"';")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    if(nrow(dbGetQuery(con, REQUEST_EXISTING)) != 0 ){
      sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "This tag is already in the database",
        type = "error"
      )
      
    } else {
      
      REQUESTE_ADD = paste0("INSERT INTO tag (name, description) VALUES (
                            '",tolower(input$ManageTag_NewName), "',
                            '",input$ManageTag_NewDescription, "');
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
      
      pixelsetModify$TagAll = dbGetQuery(con,paste0("select id, name from tag order by name;"))
      namesTag = pixelsetModify$TagAll[,2] 
      pixelsetModify$TagAll = pixelsetModify$TagAll [,1]
      names(pixelsetModify$TagAll) = namesTag
      
      updateCheckboxGroupInput(session, "PixelSetAdminTab_modify_TagA", choices = pixelsetModify$TagAll,
                               selected = pixelsetModify$TagAChoices )
      updateCheckboxGroupInput(session, "PixelSetAdminTab_modify_TagE", choices = pixelsetModify$TagAll,
                               selected = pixelsetModify$TagEChoices )
      
      dbDisconnect(con)
    }
    
    updateTextInput(session,"ManageTag_NewName",value = "")
    updateTextInput(session,"ManageTag_NewDescription",value = "")
    dbDisconnect(con)
  })
  
  
  
  output$tagModifUi <- renderUI({
    if(!is.null(TAG$table) && nrow(TAG$table)!=0){
      choices = TAG$table[,1]
      names(choices) = TAG$table[,2]
      
    } else {
      choices = ""
    }
    
    selectInput("tagModifSI", NULL, choices = choices)
    
  })
  
  output$tagModifDescription <- renderText({
    TAG$MODIF_Description
  })
  
  observeEvent(input$tagModifSI,{
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432) 
    on.exit(dbDisconnect(con))
    
    if(!is.null(input$tagModifSI) && input$tagModifSI != ""){
      TAG$MODIF_Description = dbGetQuery(con,paste0("select description from tag where tag.id = ",input$tagModifSI,";"))[1,1]
      TAG$MODIF_PIXELSET_ANALYSIS = dbGetQuery(con,paste0("select pixelset.id, 'Saved' Analysis
                                                          from pixelSet, tag_analysis, tag
                                                          where tag.id = ",input$tagModifSI,"
                                                          and pixelset.id_analysis = tag_analysis.id_analysis
                                                          and tag_analysis.id_tag = tag.id;"))
      
      TAG$MODIF_PIXELSET_EXP = dbGetQuery(con,paste0("select pixelset.id, 'Saved' Experiment
                                                     from pixelSet, analysis_experiment, Tag_Experiment, tag
                                                     where tag.id = ",input$tagModifSI,"
                                                     and pixelset.id_analysis = analysis_experiment.id_analysis
                                                     and analysis_experiment.id_experiment = Tag_Experiment.id_experiment
                                                     and Tag_Experiment.id_tag = tag.id;"))
      
      TAG$MODIF_PIXELSET_TABLE = dbGetQuery(con,"Select id from pixelset;")
      
      if(nrow(TAG$MODIF_PIXELSET_TABLE) != 0){
        if(nrow(TAG$MODIF_PIXELSET_ANALYSIS) == 0){
          TAG$MODIF_PIXELSET_ANALYSIS = cbind(id = TAG$MODIF_PIXELSET_TABLE[,1],
                                              analysis = rep(NA, nrow(TAG$MODIF_PIXELSET_TABLE)))
        }
        
        if(nrow(TAG$MODIF_PIXELSET_EXP) == 0){
          TAG$MODIF_PIXELSET_EXP = cbind(id = TAG$MODIF_PIXELSET_TABLE[,1],
                                         experiment = rep(NA, nrow(TAG$MODIF_PIXELSET_TABLE)))
        }
        
        TAG$MODIF_PIXELSET_TABLE = merge(TAG$MODIF_PIXELSET_TABLE,TAG$MODIF_PIXELSET_ANALYSIS ,by = "id", all = T)
        TAG$MODIF_PIXELSET_TABLE = merge(TAG$MODIF_PIXELSET_TABLE, TAG$MODIF_PIXELSET_EXP ,by = "id", all = T)
        
        TAG$MODIF_PIXELSET_TABLE_SELECTED = which(TAG$MODIF_PIXELSET_TABLE == "Saved", arr.ind = T)
        TAG$MODIF_PIXELSET_TABLE_SELECTED = data.matrix(TAG$MODIF_PIXELSET_TABLE_SELECTED)
        proxy = dataTableProxy('TagAdmin_Modify_tab')
        proxy %>% DT::selectCells(TAG$MODIF_PIXELSET_TABLE_SELECTED)
      }
    }
    
    
    dbDisconnect(con)
  })
  
  
  output$TagAdmin_Modify_tab = renderDT(TAG$MODIF_PIXELSET_TABLE, server = TRUE,
                                        selection = list(target = 'cell', 
                                                         selected = data.matrix(TAG$MODIF_PIXELSET_TABLE_SELECTED) ))
  
  observeEvent(input$TagAdmin_Modify_tab_cells_selected,{
    
    if(nrow(input$TagAdmin_Modify_tab_cells_selected) != 0){
      pos = which(input$TagAdmin_Modify_tab_cells_selected[,2] == "1")
      
      if(length(pos) != 0){
        TAG$MODIF_PIXELSET_TABLE_SELECTED = data.matrix(input$TagAdmin_Modify_tab_cells_selected[-pos,])
        proxy = dataTableProxy('TagAdmin_Modify_tab')
        proxy %>% DT::selectCells(data.matrix(TAG$MODIF_PIXELSET_TABLE_SELECTED))
      }
    }
  })
  
  observeEvent(input$TagAdmin_Modify_btn,{
    confirmSweetAlert(
      session = session,
      inputId = "confirm_modify_TagAssociation",
      type = "warning",
      title = "Want to modify tag association ?",
      text = "",
      danger_mode = TRUE
    )
  })
  
  observeEvent(input$confirm_modify_TagAssociation, {
    if (isTRUE(input$confirm_modify_TagAssociation)) {
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432) 
      on.exit(dbDisconnect(con))
      
      InBase =  dbGetQuery(con,paste0("delete from tag_analysis where id_tag = ",input$tagModifSI,";"))
      
      # Step 1 : remove all links
      dbGetQuery(con,paste0("delete from tag_analysis where id_tag = '",input$tagModifSI,"';"))
      dbGetQuery(con,paste0("delete from tag_experiment where id_tag = '",input$tagModifSI,"';"))
      
      # Step 2 : create links
      if(nrow(input$TagAdmin_Modify_tab_cells_selected) != 0){
        for(cpt in 1:nrow(input$TagAdmin_Modify_tab_cells_selected)){
          i = input$TagAdmin_Modify_tab_cells_selected[cpt, 1]
          j = input$TagAdmin_Modify_tab_cells_selected[cpt, 2]
          
          if(j == 2){
            
            idAnalysis = dbGetQuery(con,paste0("select DISTINCT id_analysis from pixelset where id = '",TAG$MODIF_PIXELSET_TABLE[i,1],"';"))[1,1]
            
            if (nrow(dbGetQuery(con,paste0("select * from tag_analysis where id_analysis ='",idAnalysis,"' and id_tag ='",input$tagModifSI,"';"))) == 0){
              dbGetQuery(con,paste0("insert into tag_analysis (id_analysis, id_tag) Values ('",idAnalysis ,"',",input$tagModifSI,")"))
            }
            
          } else {
            
            idexperiment = dbGetQuery(con,paste0("select DISTINCT id_experiment from pixelset,analysis_experiment where pixelset.id_analysis = analysis_experiment.id_analysis and id = '",TAG$MODIF_PIXELSET_TABLE[i,1],"';"))[1,1]
            
            if (nrow(dbGetQuery(con,paste0("select * from tag_experiment where id_experiment ='",idexperiment,"' and id_tag ='",input$tagModifSI,"';"))) == 0){
              dbGetQuery(con,paste0("insert into tag_experiment (id_experiment, id_tag) Values ('",idexperiment ,"',",input$tagModifSI,")"))
            }
          }
        }
      }
      
      selectSI = input$tagModifSI
      
      updateSelectInput(session, "tagModifSI", selected = "")
      updateSelectInput(session, "tagModifSI", selected = selectSI)
      dbDisconnect(con)
      
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = "The changes have been saved ! ",
        type = "success"
      )
    }
  }
  )
  
  #.............................................................................
  # Remove  Tag
  #.............................................................................
  
  output$tagDeleteUi <- renderUI({
    if(!is.null(TAG$table) && nrow(TAG$table) != 0){
      choices = TAG$table[,1]
      names(choices) = TAG$table[,2]
      
    } else {
      choices = ""
    }
    
    selectInput("tagDeleteSI", NULL, choices = choices)
    
  })
  
  observeEvent(input$AdminTag_delete_btn,{
    confirmSweetAlert(
      session = session,
      inputId = "confirm_AdminTag_delete",
      type = "warning",
      title = "Delete this tag ?",
      text = names(input$tagDeleteSI) ,
      danger_mode = TRUE
    )
  })
  
  observeEvent(input$confirm_AdminTag_delete, {
    if (isTRUE(input$confirm_AdminTag_delete)) {
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432) 
      on.exit(dbDisconnect(con))
      
      Association = F
      
      if(nrow(dbGetQuery(con,paste0("Select id_tag from tag_analysis where id_tag =",input$tagDeleteSI,";"))) != 0){
        Association = T
      }
      
      if(nrow(dbGetQuery(con,paste0("Select id_tag from tag_experiment where id_tag =",input$tagDeleteSI,";"))) != 0){
        Association = T
      }
      
      if(Association){
        sendSweetAlert(
          session = session,
          title = "Oups!",
          text = "This tag is used in association ! Remove before association.",
          type = "error"
        )
      } else {
        
        dbGetQuery(con,paste0("Delete from tag where id = ",input$tagDeleteSI, ";"))
        
        sendSweetAlert(
          session = session,
          title = "Done!",
          text = "The tag is removed ! ",
          type = "success"
        )
        
        TAG$table = dbGetQuery(con, "select * from tag ORDER BY name;")
      }
      dbDisconnect(con)
      
    }
  })
  
  #.............................................................................
  # Modify  Tag
  #.............................................................................
  
  tagModif =reactiveValues()
  
  output$TagAdminTab_Modify <- renderDT(TAG$table, selection = 'single', 
                                        editable = F,
                                        options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  output$TagAdminTab_modify_ID<-renderUI({
    verbatimTextOutput("TagAdminTab_modify_ID_VTO", placeholder = TRUE)
  })
  
  output$TagAdminTab_modify_ID_VTO <- renderText(
    tagModif$id
  )
  
  observeEvent(is.null(input$TagAdminTab_Modify_rows_selected),{
    if(!is.null(input$TagAdminTab_Modify_rows_selected)){
      tagModif$id = TAG$table[input$TagAdminTab_Modify_rows_selected,1]
      enable("TagAdminTab_modify_CHANGE")
    } else {
      tagModif$id = ""
      disable("TagAdminTab_modify_CHANGE")
    }
  })
  
  output$TagAdminTab_modify_name<-renderUI({
    if(!is.null(TAG$table) && nrow(TAG$table) != 0){
      textAreaInput("TagAdminTab_modify_name_TA",NULL, value = TAG$table[which(TAG$table[,1] == tagModif$id),2])
    }
  })
  
  output$TagAdminTab_modify_Description<-renderUI({
    if(!is.null(TAG$table) && nrow(TAG$table) != 0){
      textAreaInput("PTagAdminTab_modify_Description_TA",NULL, value = TAG$table[which(TAG$table[,1] == tagModif$id),3])
    }
  })
  
  observeEvent(input$TagAdminTab_modify_CHANGE,{
    confirmSweetAlert(
      session = session,
      inputId = "confirm_modify_tagModification",
      type = "warning",
      title = "Modify this tag ?",
      text = as.character(tagModif$id),
      danger_mode = TRUE
    )
  })
  
  observeEvent(input$confirm_modify_tagModification, {
    if (isTRUE(input$confirm_modify_tagModification)) {
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432) 
      on.exit(dbDisconnect(con))
      
      dbGetQuery(con,paste0("update tag set description ='",input$PTagAdminTab_modify_Description_TA,"' where id = ",tagModif$id , ";"))
      dbGetQuery(con,paste0("update tag set name ='",input$TagAdminTab_modify_name_TA,"' where id = ",tagModif$id , ";"))
      
      TAG$table = dbGetQuery(con, "select * from tag ORDER BY name;")
      dbDisconnect(con)
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = "The tag is modified ! ",
        type = "success"
      )
    } else {
      sendSweetAlert(
        session = session,
        title = "Cancellation...",
        text = "The modification is cancelled ! ",
        type = "warning"
      )
    }
  }
  )
  
  #-----------------------------------------------------------------------------
  # END Admin Tag
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Admin PixelSet
  #-----------------------------------------------------------------------------
  
  observeEvent(PIXELSETLIST_RV$info,{
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432) 
    on.exit(dbDisconnect(con))
    
    PIXELSETLIST_RV$infoMin = dbGetQuery(con,"Select id, name, description from pixelset order by id;")
    
    dbDisconnect(con)
  })
  
  
  output$PixelSetsAdminTab <- renderDT(PIXELSETLIST_RV$info, selection = 'multiple', 
                                       editable = F,
                                       options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  #.............................................................................
  # Remove Pixelsets
  #.............................................................................
  
  pixelsetModify = reactiveValues()
  
  output$PixelSetsAdminTab_Modify <- renderDT(PIXELSETLIST_RV$infoMin, selection = 'single', 
                                              editable = F,
                                              options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  output$PixelSetAdminTab_modify_ID<-renderUI({
    verbatimTextOutput("PixelSetAdminTab_modify_ID_VTO", placeholder = TRUE)
  })
  
  output$PixelSetAdminTab_modify_ID_VTO <- renderText(
    pixelsetModify$id
  )
  
  observeEvent(is.null(input$PixelSetsAdminTab_Modify_rows_selected),{
    if(!is.null(input$PixelSetsAdminTab_Modify_rows_selected)){
      pixelsetModify$id = PIXELSETLIST_RV$infoMin[input$PixelSetsAdminTab_Modify_rows_selected,1]
      enable("PixelSetAdminTab_modify_CHANGE")
    } else {
      pixelsetModify$id = ""
      disable("PixelSetAdminTab_modify_CHANGE")
    }
  })
  
  observeEvent(pixelsetModify$id,{
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432) 
    on.exit(dbDisconnect(con))
    
    pixelsetModify$id_analysis_experiment = dbGetQuery(con,paste0("select DISTINCT pixelset.id_analysis, id_experiment
                                                                  from pixelSet, analysis_experiment
                                                                  where pixelSet.id = '",pixelsetModify$id,"'
                                                                  and analysis_experiment.id_analysis = pixelSet.id_analysis;"))
    
    pixelsetModify$TagA = dbGetQuery(con,paste0("select tag_analysis.id_analysis, tag.name, tag.id
                                                from pixelSet, tag_analysis, tag
                                                where pixelSet.id = '",pixelsetModify$id,"'
                                                and pixelset.id_analysis = tag_analysis.id_analysis
                                                and tag_analysis.id_tag = tag.id;"))
    
    pixelsetModify$TagE = dbGetQuery(con,paste0("select analysis_experiment.id_experiment, tag.name , tag.id
                                                from pixelSet, analysis_experiment, Tag_Experiment, tag
                                                where pixelSet.id = '",pixelsetModify$id,"'
                                                and pixelset.id_analysis = analysis_experiment.id_analysis
                                                and analysis_experiment.id_experiment = Tag_Experiment.id_experiment
                                                and Tag_Experiment.id_tag = tag.id;"))
    
    pixelsetModify$TagAll = dbGetQuery(con,paste0("select id, name from tag order by name;"))
    
    if(nrow(pixelsetModify$TagAll) != 0){
      namesTag = pixelsetModify$TagAll[,2] 
      pixelsetModify$TagAll = pixelsetModify$TagAll [,1]
      names(pixelsetModify$TagAll) = namesTag
    }
    
    dbDisconnect(con)
    
    if(nrow(pixelsetModify$TagE) != 0){
      pixelsetModify$TagEChoices = pixelsetModify$TagE[,"id"]
    } else {
      pixelsetModify$TagEChoices = ""
    }
    
    if(nrow(pixelsetModify$TagA) != 0){
      pixelsetModify$TagAChoices = pixelsetModify$TagA[,"id"]
    } else {
      pixelsetModify$TagAChoices = ""
    }
    
    updateCheckboxGroupInput(session, "PixelSetAdminTab_modify_TagA", choices = pixelsetModify$TagAll,
                             selected = pixelsetModify$TagAChoices )
    updateCheckboxGroupInput(session, "PixelSetAdminTab_modify_TagE", choices = pixelsetModify$TagAll,
                             selected = pixelsetModify$TagEChoices )
    
    if(!is.null(PIXELSETLIST_RV$infoMin) && nrow(PIXELSETLIST_RV$infoMin) != 0) {
      updateTextAreaInput(session,"PixelSetAdminTab_modify_name_TA", value = PIXELSETLIST_RV$infoMin[which(PIXELSETLIST_RV$infoMin[,1] == pixelsetModify$id),2] )  
      updateTextAreaInput(session,"PixelSetAdminTab_modify_Description_TA", value = PIXELSETLIST_RV$infoMin[which(PIXELSETLIST_RV$infoMin[,1] == pixelsetModify$id),3] )  
    }
  })
  
  output$PixelSetAdminTab_modify_TagA<-renderUI({
    checkboxGroupInput("PixelSetAdminTab_modify_TagA_CBG", NULL, choices =pixelsetModify$TagAll,
                       selected = pixelsetModify$TagAChoices)
  })
  
  
  
  observeEvent(input$PSModify_Exp_tags_Newbtn,{
    
    REQUEST_EXISTING = paste0("SELECT *
                              FROM tag
                              WHERE name = '",tolower(input$PSModify_tags_NewName),"';")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    if(nrow(dbGetQuery(con, REQUEST_EXISTING)) != 0 ){
      sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "This tag is already in the database",
        type = "error"
      )
      
    } else {
      
      REQUESTE_ADD = paste0("INSERT INTO tag (name, description) VALUES (
                            '",tolower(input$PSModify_tags_NewName), "',
                            '",input$PSModify_tags_NewDescription, "');
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
      
      pixelsetModify$TagAll = dbGetQuery(con,paste0("select id, name from tag order by name;"))
      namesTag = pixelsetModify$TagAll[,2] 
      pixelsetModify$TagAll = pixelsetModify$TagAll [,1]
      names(pixelsetModify$TagAll) = namesTag
      updateCheckboxGroupInput(session, "PixelSetAdminTab_modify_TagA", choices = pixelsetModify$TagAll,
                               selected = pixelsetModify$TagAChoices )
      updateCheckboxGroupInput(session, "PixelSetAdminTab_modify_TagE", choices = pixelsetModify$TagAll,
                               selected = pixelsetModify$TagEChoices )
      
      dbDisconnect(con)
    }
    
    updateTextInput(session,"PSModify_tags_NewName",value = "")
    updateTextInput(session,"PSModify_tags_NewDescription",value = "")
    dbDisconnect(con)
  })
  
  output$PixelSetAdminTab_modify_TagE<-renderUI({
    checkboxGroupInput("PixelSetAdminTab_modify_TagE_CBG", NULL, choices =pixelsetModify$TagAll,
                       selected =pixelsetModify$TagEChoices )
  })
  
  output$PixelSetAdminTab_modify_name<-renderUI({
    if(!is.null(PIXELSETLIST_RV$infoMin) && nrow(PIXELSETLIST_RV$infoMin) != 0){
      textAreaInput("PixelSetAdminTab_modify_name_TA",NULL, value = PIXELSETLIST_RV$infoMin[which(PIXELSETLIST_RV$infoMin[,1] == pixelsetModify$id),2])
    } else {
      NULL
    }
  })
  
  output$PixelSetAdminTab_modify_Description<-renderUI({
    if(!is.null(PIXELSETLIST_RV$infoMin) && nrow(PIXELSETLIST_RV$infoMin) != 0){
      textAreaInput("PixelSetAdminTab_modify_Description_TA",NULL, value = PIXELSETLIST_RV$infoMin[which(PIXELSETLIST_RV$infoMin[,1] == pixelsetModify$id),3])
    } else {
      NULL
    }
  })
  
  observeEvent(input$PixelSetAdminTab_modify_CHANGE,{
    confirmSweetAlert(
      session = session,
      inputId = "confirm_modify_pixelset",
      type = "warning",
      title = "Want to modify this PixelSet ?",
      text = pixelsetModify$id ,
      danger_mode = TRUE
    )
  })
  
  observeEvent(input$confirm_modify_pixelset, {
    if (isTRUE(input$confirm_modify_pixelset)) {
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      dbGetQuery(con,paste0("update pixelset set description ='",input$PixelSetAdminTab_modify_Description_TA,"' where id = '",pixelsetModify$id, "';"))
      dbGetQuery(con,paste0("update pixelset set name ='",input$PixelSetAdminTab_modify_name_TA,"' where id = '",pixelsetModify$id, "';"))
      
      if(nrow(pixelsetModify$TagA) != 0){
        for(i in 1:nrow(pixelsetModify$TagA)){
          dbGetQuery(con,paste0("delete from tag_analysis where id_analysis = '",pixelsetModify$TagA[i,1],"' and id_tag = ",pixelsetModify$TagA[i,3],";"))
        }
        
      }
      
      if(nrow(pixelsetModify$TagE) != 0){
        for(i in 1:nrow(pixelsetModify$TagE)){
          dbGetQuery(con,paste0("delete from Tag_Experiment where id_experiment = '",pixelsetModify$TagE[i,1],"' and id_tag = ",pixelsetModify$TagE[i,3],";"))
        }
      }
      
      if(length(input$PixelSetAdminTab_modify_TagA_CBG) != 0){
        for(i in input$PixelSetAdminTab_modify_TagA_CBG ){
          dbGetQuery(con,paste0("insert into tag_analysis (id_analysis, id_tag) Values ('",pixelsetModify$id_analysis_experiment[1,1] ,"',",i,")"))
        }
      }
      
      if(length(input$PixelSetAdminTab_modify_TagE_CBG) != 0){
        for(i in input$PixelSetAdminTab_modify_TagE_CBG ){
          dbGetQuery(con,paste0("insert into Tag_Experiment (id_experiment, id_tag) Values ('",pixelsetModify$id_analysis_experiment[1,2] ,"',",i,")"))
        }
      }
      
      # Update All
      MAJ$value = MAJ$value + 1
      
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
      datasourceRV$tab = dbGetQuery(con, "SELECT DataSource.name, DataSource.description, DataSource.published, DataSource.url,
                                      pixelset.id, pixelset.name, PixelSet.description, omicsarea.name
                                    FROM Datasource, experiment, analysis_experiment,pixelset, OmicsArea
                                    WHERE experiment.omicsareaid = OmicsArea.id 
                                    AND experiment.DataSourceId = DataSource.id
                                    AND analysis_experiment.id_experiment = experiment.id
                                    AND analysis_experiment.id_analysis = pixelset.id_analysis
                                    ;")
      
      updateMeta(dbGetQuery(con,paste0("Select id_submission from pixelset where id = '",pixelsetModify$id,"'"))[1,1])
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = "PixelSet is modified!",
        type = "success"
      )
      
      dbDisconnect(con)
    }
  })
  
  #.............................................................................
  # Remove Pixelsets
  #.............................................................................
  observeEvent(is.null(input$PixelSetsAdminTab_rows_selected),{
    if(!is.null(input$PixelSetsAdminTab_rows_selected)){
      updateActionButton(session, "removePixelSets", 
                         label = paste0('Remove PixelSets (',length(input$PixelSetsAdminTab_rows_selected),')'),
                         icon = icon("minus-circle"))
      enable("removePixelSets")
    }else{
      updateActionButton(session, "removePixelSets", 
                         label = 'Remove PixelSets (0)', icon = icon("minus-circle"))
      disable("removePixelSets")
    }
    
  })
  
  observeEvent(input$removePixelSets,{
    
    confirmSweetAlert(
      session = session,
      inputId = "confirm_del_PixelSets",
      type = "warning",
      title = "Want to confirm the following PixelSet(s) ?",
      text = paste(PIXELSETLIST_RV$info[input$PixelSetsAdminTab_rows_selected,1], collapse = ",") ,
      danger_mode = TRUE
    )
  })
  
  observeEvent(input$confirm_del_PixelSets, {
    if (isTRUE(input$confirm_del_PixelSets)) {
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      for(i in input$PixelSetsAdminTab_rows_selected){
        idSubmission = dbGetQuery(con, paste0("select id_submission from pixelset where id ='",PIXELSETLIST_RV$info[i,1],"';"))
        
        nbPS = dbGetQuery(con, paste0("select count(*) from pixelset where id_submission = '",idSubmission,"';"))[1,1]
        
        idExperience = dbGetQuery(con, paste0("select DISTINCT Analysis_Experiment.id_experiment from pixelset,  Analysis_Experiment
                                              where id_Submission = '",idSubmission,"'
                                              and Analysis_Experiment.id_analysis = pixelset.id_analysis;"))
        idAnalysis = dbGetQuery(con, paste0("select DISTINCT id_analysis from pixelset
                                            where id_Submission = '",idSubmission,"'"))
        
        if(nbPS > 1){
          dbGetQuery(con, paste0("delete from pixel where pixelSet_id  = '",PIXELSETLIST_RV$info[i,1] ,"';"))
          dbGetQuery(con, paste0("delete from pixelset where id  = '",PIXELSETLIST_RV$info[i,1] ,"';"))
        } else {
          dbGetQuery(con, paste0("delete from Tag_Experiment where id_experiment = '",idExperience,"';"))
          dbGetQuery(con, paste0("delete from Tag_Analysis where id_analysis = '",idAnalysis,"';"))
          dbGetQuery(con, paste0("delete from Analysis_Experiment where id_analysis = '",idAnalysis,"' and id_experiment = '",idExperience,"' ;"))
          dbGetQuery(con, paste0("delete from experiment where id  = '",idExperience,"';"))
          dbGetQuery(con, paste0("delete from pixel where pixelSet_id IN (select id from pixelset where id_Submission = '",idSubmission ,"');"))
          dbGetQuery(con, paste0("delete from pixelset where id IN (select id from pixelset where id_Submission = '",idSubmission ,"');"))
          dbGetQuery(con, paste0("delete from analysis where id  = '",idAnalysis,"';"))
          dbGetQuery(con, paste0("delete from submission where id = '",idSubmission ,"';"))
        }
        
      }
      
      # Update all
      MAJ$value = MAJ$value + 1
      
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = "PixelSet(s) deleted !",
        type = "success"
      )
      
      dbDisconnect(con)
    }
  })  
  
  
  #-----------------------------------------------------------------------------
  # Admin user table 
  #-----------------------------------------------------------------------------
  
  output$adminUsers <- renderDT(USERS$infos, selection = 'multiple', 
                                editable = TRUE,
                                options = list(scrollX = TRUE, searchHighlight = TRUE))
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
    on.exit(dbDisconnect(con))
    
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
        on.exit(dbDisconnect(con))
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
    on.exit(dbDisconnect(con))
    
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
      on.exit(dbDisconnect(con))
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
  
  output$example_CF <-  renderDataTable({
    
    df <- read.csv("Data/CF_CGLAB.txt",
                   header = T,
                   sep = "\t",
                   quote = "",
                   nrows=5, 
                   col.names = c( "Feature name", "Gene name", "Chromosome",
                                  "Start coordinate", "Stop coordinate", 
                                  "Strand", "Description", "Species", "URL")
    )
    
  }, selection = 'none', options = list(scrollX = TRUE , dom = 't')) 
  
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
      
      rv$Source = dbGetQuery(con, "select * from CFSource;")
      
      updateSelectInput(session, "selectSource", choices = rv$Source[,2], selected = input$CFSourceName)
      
      sendSweetAlert(
        session = session,
        title = "Congratulations!",
        text = "The import was successful!",
        type = "success"
      )
      
    }
    dbDisconnect(con)
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
    
    species_id  = dbGetQuery(con, paste0("Select id from species where name = '",database[1,8],"';"))[1,1]
    default_db_id = dbGetQuery(con, paste0("Select id from CFSource where name = '",input$selectSource,"';"))[1,1]
    
    withProgress(message = 'Import in Database', value = 0, {
      # if(input$importTypeCF == "main"){
      if(ncol(database) == 9){
        n <- nrow(database)
        rv$ERROR = F
        for(i in 1:nrow(database)){
          
          incProgress(1/n, detail = paste("Doing part", i))
          
          REQUEST_INDB = paste0("SELECT * from ChromosomalFeature WHERE feature_name = '",database[i,1],"'" );
          if(nrow(dbGetQuery(con, REQUEST_INDB)) != 0){
            REQUEST_ANNOT = paste0("UPDATE ChromosomalFeature SET gene_name = '",database[i,2],"', chromosome = '",database[i,3],
                                   "', start_coordinate = ",database[i,4],", stop_coordinate =",database[i,5],", strand ='",database[i,6],
                                   "',description ='",gsub("\'","\'\'",database[i,7]),"',species_id ='",species_id, "', url ='",database[i,9] ,"',default_db_id ='",default_db_id 
                                   ,"' WHERE Feature_name = '",database[i,1],"';" );
          } else {
            REQUEST_ANNOT = paste0("INSERT INTO ChromosomalFeature (feature_name , gene_name,  chromosome, start_coordinate, stop_coordinate, strand, description,species_id, url, default_db_id) VALUES ( ",paste(c(paste0("'",database[i,1:3],"'"), database[i,4:5], paste0("'",database[i,6],"'"), paste0("'",gsub("\'","\'\'",database[i,7]),"'"),paste0("'",species_id,"'"), paste0("'",database[i,9],"'"),paste0("'",default_db_id,"'")),collapse = ","),
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
    })
    MAJ$value = MAJ$value + 1
    dbDisconnect(con)
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
  
  output$DescriCFSource <- renderUI({
    HTML(paste(rv$Source[which(rv$Source[, 2] == input$selectSource), 4],
               tags$br(),
               a("Link to source", href= rv$Source[which(rv$Source[, 2] == input$selectSource), 5], target="_blank")))
  })
  
  
  #=============================================================================
  # Quick searches
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
      testCF = dbGetQuery(con,paste0("select feature_name, species.name from chromosomalfeature, species where (lower(feature_name) ='",tolower(input$searchCF),"' or lower(gene_name) = '",tolower(input$searchCF),"') and species.id = species_id;"))
      dbDisconnect(con)
      if(nrow(testCF) == 1){
        CF$name = testCF[1,1]
        updateTextInput(session, "searchCF", value = "")
        updateTabItems (session, "tabs", selected = "CF_item")
        shinyjs::runjs("window.scrollTo(0, 0)")
      } else if(nrow(testCF) == 0){
        sendSweetAlert(
          session = session,
          title = input$searchCF,
          text = "This chromosomal feature isn't in Pixel...",
          type = "error"
        )
        updateTextInput(session, "searchCF", value = "")
        
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
            updateTabItems (session, "tabs", selected = "CF_item")
            shinyjs::runjs("window.scrollTo(0, 0)")
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
        
        updateTabItems (session, "tabs", selected = "Tags")
        shinyjs::runjs("window.scrollTo(0, 0)")
        
        updateTextInput(session, "searchTag", value = "")
      } else {
        sendSweetAlert(
          session = session,
          title = input$searchTag,
          text = "This tag isn't in Pixel...",
          type = "error"
        )
      } 
      updateTextInput(session, "searchTag",value = "" )
    }
  })
  
  #-----------------------------------------------------------------------------
  # PixelSet
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
        updateTabItems (session, "tabs", selected = "PixelSet")
        shinyjs::runjs("window.scrollTo(0, 0)")
      } else {
        sendSweetAlert(
          session = session,
          title = input$testPS,
          text = "This pixelSet isn't in Pixel...",
          type = "error"
        )
      } 
      
      updateTextInput(session, "searchPS",value = "" )
    }
  })
  
  #=============================================================================
  # End Quick searches
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
    
    CF$PIXEL_quantitative = NULL
    CF$PIXEL_qualitative = NULL
    CF$PIXELSET_qualitative = NULL 
    CF$PIXELSET_quantitative= NULL
    
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
    
    CF$PIXELSET =  dbGetQuery(con, paste0("SELECT  pixel.value, pixel.quality_score, pixelset.id, pixelset.name, 
                                          pixelset.description, pixelset.id_analysis, pixelset.id_submission, OmicsArea.name 
                                          FROM pixel, pixelset, OmicsUnitType,  analysis_experiment, experiment, omicsarea
                                          WHERE pixel.cf_feature_name = '",CF$name,"' 
                                          AND pixel.OmicsUnitType_id = OmicsUnitType.id
                                          AND pixel.pixelset_id = pixelset.id
                                          AND pixelset.id_analysis = analysis_experiment.id_analysis
                                          AND analysis_experiment.id_experiment = experiment.id
                                          AND experiment.omicsAreaid = omicsarea.id
                                          ;"))
    
    CF$PIXEL =  dbGetQuery(con, paste0("select value, quality_score as QS, pixelset_id, OmicsUnitType.name as OUT
                                       from pixel,OmicsUnitType 
                                       where cf_feature_name = '",CF$name,"'
                                       and omicsunittype_id = OmicsUnitType.id;"))
    
    
    if(!is.null(CF$PIXELSET) && nrow(CF$PIXELSET) != 0){
      colnames(CF$PIXEL) = c("Value", "Quality score", "Pixelset ID", "Omics unit type")
      colnames(CF$PIXELSET) = c("Value", "Quality score", "Pixelset ID", "Pixelset name", "Pixelset description", "Analysis ID", "Submission ID", "Omicsarea name")
      
      CF$PIXEL_quantitative = CF$PIXEL[!is.na(as.numeric(CF$PIXEL[,"Value"])), ]
      CF$PIXEL_qualitative = CF$PIXEL[is.na(as.numeric(CF$PIXEL[,"Value"])), ]
      CF$PIXELSET_quantitative = CF$PIXELSET[!is.na(as.numeric(CF$PIXELSET[,"Value"])),]
      CF$PIXELSET_qualitative = CF$PIXELSET[is.na(as.numeric(CF$PIXELSET[,"Value"])),]
      
      CF$Sup_tab = NULL 
      CF$PIXELSET_qualitative = CF$PIXELSET_qualitative[order(CF$PIXELSET_qualitative[,"Pixelset name"]), ]
      
      # Sequence 
      
      posS = which(CF$PIXELSET_qualitative[,"Omicsarea name"] ==  "Sequence")
      if(length(posS) != 0){
        CF$Sup_tab = c(CF$Sup_tab,paste("<h3>Sequence</h3><p style ='font-family: monospace;'> >",CF$name,"<br>", 
                                        paste(strsplit(CF$PIXELSET_qualitative[posS,"Value"], "(?<=.{60})", perl = TRUE)[[1]], collapse = "<br>"), "</p>"))
      }
      
      # Go terms 
      posGO = which(CF$PIXELSET_qualitative[,"Omicsarea name"] ==  "GO Terms")
      if (length(posGO) != 0){
        table = matrix(unlist(strsplit(CF$PIXELSET_qualitative[posGO,"Value"]," # ")), ncol = 3, byrow = T)
        table[,1] = paste0("<a href ='https://www.ebi.ac.uk/QuickGO/GTerm?id=",table[,1], "' target='_blank'>", table[,1], "</a>" )
        table = apply(table, 1, paste, collapse= "</td><td>")
        table = paste(table, collapse = "</td></tr><tr><td>")
        
        CF$Sup_tab = c(CF$Sup_tab, paste( '<h3>Go terms</h3><table class="table table-striped"><thead>
                                          <tr><th scope="col">Go term</th><th scope="col">Term</th><th scope="col">Definition</th></tr>
                                          </thead><tbody>',"<tr><td>",table, "</td></tr></tbody></table>"))
      }
      
      if(nrow(CF$PIXELSET_qualitative[-c(posS, posGO),]) != 0){
        CF$Sup_tab = c(CF$Sup_tab,"<h3>Qualitative information</h3>")
        CF$Sup_tab = c(CF$Sup_tab,paste("<b>",CF$PIXELSET_qualitative[-c(posS, posGO),"Pixelset name"],"</b> : ", CF$PIXELSET_qualitative[-c(posS, posGO),"Value"], "<br>"))
      }
    }
    
    
    dbDisconnect(con)
  })
  
  output$sup_annot <- renderUI({
    HTML(CF$Sup_tab)
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
  
  output$CF_PixelSET <- renderDT(CF$PIXELSET_quantitative, 
                                 selection = 'single', 
                                 editable = F,
                                 options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  output$CF_Pixel <- renderDT(CF$PIXEL_quantitative, 
                              selection = 'none', 
                              editable = F,
                              options = list(scrollX = TRUE, searchHighlight = TRUE))
  
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
  datasourceRV$tab = dbGetQuery(con, "SELECT DataSource.name, DataSource.description, DataSource.published, DataSource.url,
                                      pixelset.id, pixelset.name, PixelSet.description, omicsarea.name
                                FROM Datasource, experiment, analysis_experiment,pixelset, OmicsArea
                                WHERE experiment.omicsareaid = OmicsArea.id 
                                AND experiment.DataSourceId = DataSource.id
                                AND analysis_experiment.id_experiment = experiment.id
                                AND analysis_experiment.id_analysis = pixelset.id_analysis
                                ;")
  
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
      if(nrow(PIXELSETLIST_RV$tags) != 0){
        for(i in 1:nrow(PIXELSETLIST_RV$tags)){
          interList[[PIXELSETLIST_RV$tags[i,"id"] ]] = c(interList[[PIXELSETLIST_RV$tags[i,"id"]]], PIXELSETLIST_RV$tags[i,"name"])
        }
        
        interList = lapply(interList, unique)
        interList = lapply(interList, sort)
      }
      
      PIXELSETLIST_RV$tagsList = interList
      
      if(!is.null(PIXELSETLIST_RV$tagsList)  && length(PIXELSETLIST_RV$tagsList) != 0){
        PIXELSETLIST_RV$tagsTab = cbind(names(PIXELSETLIST_RV$tagsList),unlist(lapply(PIXELSETLIST_RV$tagsList, paste, collapse = " | ")) )
        colnames(PIXELSETLIST_RV$tagsTab) = c("ID", "Tags")
        PIXELSETLIST_RV$infoWithTags = merge(PIXELSETLIST_RV$info, PIXELSETLIST_RV$tagsTab,by = "ID", all = T)
      } else {
        PIXELSETLIST_RV$infoWithTags = PIXELSETLIST_RV$info
      }
      
      dbDisconnect(con)
    }
  })
  
  output$PIXELSETLIST_tab <- renderDT( PIXELSETLIST_RV$infoWithTags[PIXELSETLIST_RV$Selected,],
                                       selection = 'multiple',server = FALSE,
                                       editable = F, filter = 'top',
                                       extensions = 'Buttons', options = list(
                                         scrollX = TRUE,searchHighlight = TRUE,
                                         dom = 'Bfrtip',
                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                       )
  )
  
  output$PixelSetTags = renderUI({
    if(nrow(TAG$table) !=0 && length(PIXELSETLIST_RV$tags[!duplicated(PIXELSETLIST_RV$tags[,"name"]),"name"]) != 0){
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
      updateActionButton(session, "PixelSetExploreBtn", label = paste0("Integration of Multi PixelSets (",length(input$PIXELSETLIST_tab_rows_selected),")"))
    }else{
      updateActionButton(session, "PixelSetExploreBtn", label = "Integration of Multi PixelSets (0)")
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
      inter2 = PIXELSETLIST_RV$infoWithTags[,1] %in% inter1
      pos = which(inter2)
      PIXELSETLIST_RV$Selected = pos
      
    }else{
      if(!is.null(PIXELSETLIST_RV$infoWithTags) && nrow(PIXELSETLIST_RV$infoWithTags) != 0 ){
        PIXELSETLIST_RV$Selected = 1:nrow(PIXELSETLIST_RV$infoWithTags)
      }
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
    
    PixelSetExploRV$PixelSetID = PIXELSETLIST_RV$infoWithTags[PIXELSETLIST_RV$Selected,][input$PIXELSETLIST_tab_rows_selected,1]
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
      
      for(i in 1:ncol(PixelSetExploRV$TAB)){
        vec = as.character(PixelSetExploRV$TAB[,i])
        vec = vec[!vec == ""]
        vec = na.omit(vec)
        
        if( all(!is.na(as.numeric(vec))) ){
          PixelSetExploRV$TAB[,i] = as.numeric(as.character(PixelSetExploRV$TAB[,i]))
        } 
      }
      
      sendSweetAlert(
        session = session,
        title = "Duplicated data! ",
        text = HTML(paste("Selected pixelsets contain duplicate lines. There is redundancy in the table for the following chromosomal features: <br>", 
                          paste(unique(PixelSetExploRV$TAB[duplicated(PixelSetExploRV$TAB[,1]),1]), collapse = " "))),
        type = "warning", html = T
      )
      
      
      PixelSetExploRV$SEARCH = 1:nrow(PixelSetExploRV$TAB)
      dbDisconnect(con)
      
    }
  })
  
  
  output$PSExploUI <- renderUI({
    if(!is.null(PixelSetExploRV$PixelSetID) && length(PixelSetExploRV$PixelSetID) != 0){
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
    } else {
      NULL
    }
  })
  
  #-----------------------------------------------------------------------------
  # MultiPixelSets : Histogram
  #-----------------------------------------------------------------------------
  
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
  
  #-----------------------------------------------------------------------------
  # MultiPixelSets : extract gene list
  #-----------------------------------------------------------------------------
  
  
  output$geneListMPS = renderText({
    if(!is.null(input$PSExploTab_rows_all) && length(input$PSExploTab_rows_all) != nrow(PixelSetExploRV$TAB)){
      paste(unique(PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,][input$PSExploTab_rows_all,1]), collapse = " ; ")
    }
  })
  
  output$geneListSizeMPS = renderUI({
    if(!is.null(input$PSExploTab_rows_all) && length(input$PSExploTab_rows_all) != nrow(PixelSetExploRV$TAB)){
      HTML(paste("<b>Size list</b>: ",length(unique(PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,][input$PSExploTab_rows_all,1]))))
    }
  })
  
  #-----------------------------------------------------------------------------
  # MultiPixelSets : UpserR
  #-----------------------------------------------------------------------------
  
  output$UpsetR <- renderPlot({
    if(!is.null(PixelSetExploRV$UpsetR) && length(PixelSetExploRV$UpsetR) > 1){
      upset(fromList(PixelSetExploRV$UpsetR), text.scale= 1.8)
    } else {
      NULL
    }
    
  })
  
  
  #-----------------------------------------------------------------------------
  # MultiPixelSets : FILTERS
  #-----------------------------------------------------------------------------
  
  # Track the number of input boxes to render
  counter <- reactiveValues(n = 0)
  MPS_RV <- reactiveValues(val = list())
  
  #Track the number of input boxes previously
  prevcount <-reactiveValues(n = 0)
  
  observeEvent(input$add_filter_btn, {
    if (counter$n > 0) {
      if(eval(parse(text = paste0("input$col_",counter$n))) == "-"){
        sendSweetAlert(
          session = session,
          title = "Oops!",
          text = "Previous filter isn't selected...",
          type = "error"
        )
      }else {
        counter$n <- counter$n + 1
        prevcount$n <- counter$n - 1
      }
    } else {
      counter$n <- counter$n + 1
      prevcount$n <- counter$n - 1
    }
    
  })
  
  observeEvent(input$rm_filter_btn, {
    if (counter$n > 0) {
      MPS_RV$val = MPS_RV$val[! names(MPS_RV$val) %in% paste0("FilterUiElement_", counter$n)]
      MPS_RV$choice = MPS_RV$choice[! names(MPS_RV$choice) %in% paste0("col_",counter$n)] 
      
      counter$n <- counter$n - 1 
      prevcount$n <- counter$n + 1
    }
  })
  
  output$counter <- renderPrint(print(counter$n))
  
  textboxes <- reactive({
    
    n <- counter$n
    if (n > 0) {
      if(prevcount$n > 0){
        val = list()
        choice = c()
        
        if(prevcount$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcount$n
          isInc <- TRUE
        }
        
        # Memory of previous choices
        for(i in 1:lesscnt){
          MPS_RV$choice[paste0("col_",i)] = isolate(input[[paste0("col_",i)]])
          MPS_RV$val[[paste0("FilterUiElement_", i)]] = isolate(input[[paste0("FilterUiElement_",i)]])
          
          if(!is.null(isolate(input[[paste0("FilterUiElementRGB_",i)]]))){
            MPS_RV$RGB = isolate(input[[paste0("FilterUiElementRGB_",i)]])
          }
        }
        
        if(isInc){
          MPS_RV$val[[paste0("FilterUiElement_", n)]] <- ""
        }
        
        choices <- c('-', colnames(PixelSetExploRV$TAB))
        
        
        lapply(seq_len(n), function(i) {
          
          fluidRow(
            div(class = "dynamicSI",
                column(4, selectInput(paste0("col_", i),  label = NULL, width = "100%", 
                                      choices = choices, selected = MPS_RV$choice[paste0("col_",i)] ))),
            column(8 ,uiOutput(outputId = paste0("FilterUi_", i)))
          )
        })
        
      }else{
        choices <- c('-', colnames(PixelSetExploRV$TAB))
        
        if(n == 1){
          lapply(seq_len(n), function(i) {
            fluidRow(
              div(class = "dynamicSI",column(4,selectInput(paste0("col_", i), label = NULL, width = "100%", choices = choices))),
              column(8 ,uiOutput(outputId = paste0("FilterUi_", i)))
            )
          })
        }
      }
    }
  })
  
  observe({
    input$lastSelect
    isolate({
      if (!is.null(input$last_SI)) {
        id = unlist(strsplit({input$last_SI}, "_"))[2]
        inter = PixelSetExploRV$TAB[PixelSetExploRV$SEARCH ,eval(parse(text = paste0("input$col_",id)))]
        inter = inter[!is.na(inter)]
        inter = inter[inter!=""]
        inter = inter[!is.null(inter)]
        
        if(!is.na(as.numeric(inter[1]))){
          output[[paste0("FilterUi_", id)]] <- renderUI({
            
            if(!is.null(MPS_RV$val) && paste0("FilterUiElement_", id) %in% names(MPS_RV$val) && length(MPS_RV$val[[paste0("FilterUiElement_", id)]]) == 2){
              div(class= "sliderStyle", sliderInput(paste0("FilterUiElement_", id), NULL, width = '100%',
                                                    min = min(inter,na.rm = T), max = max(inter,na.rm = T),
                                                    value = MPS_RV$val[[paste0("FilterUiElement_", id)]]),
                  
                  radioGroupButtons(inputId = paste0("FilterUiElementRGB_", id), 
                                    label = "Part studied", choices = c("Gray", 
                                                                        "Red"), 
                                    selected = MPS_RV$RGB, checkIcon = list(yes = icon("check")))
              )
            } else {
              div(class= "sliderStyle", sliderInput(paste0("FilterUiElement_", id), NULL, width = '100%',
                                                    min = min(inter,na.rm = T), max = max(inter,na.rm = T),
                                                    value = c(min(inter,na.rm = T), max(inter,na.rm = T))),
                  radioGroupButtons(inputId = paste0("FilterUiElementRGB_", id), 
                                    label = "Part studied", choices = c("Gray", 
                                                                        "Red"), 
                                    status = 'RGB_slider',
                                    selected = "Gray", checkIcon = list(yes = icon("check")))
              )
            }
          })
        } else {
          output[[paste0("FilterUi_", id)]] <- renderUI({
            if(!is.null(MPS_RV$val) && paste0("FilterUiElement_", id) %in% names(MPS_RV$val)){
              textInput(inputId = paste0("FilterUiElement_", id),label = NULL,value = MPS_RV$val[[paste0("FilterUiElement_", id)]],
                        width = "100%", placeholder = "a regex expression")
            } else {
              textInput(inputId = paste0("FilterUiElement_", id),label = NULL, width = "100%", placeholder = "a regex expression")
            }
            
          })
          
        }
        
      }
    })
  })
  
  output$textbox_ui <- renderUI({ 
    textboxes() 
  })
  
  
  observeEvent(input$filter_btn, {
    if(counter$n > 0){
      
      check = T
      for (i in 1:counter$n){
        if(eval(parse(text = paste0("input$col_",i))) == "-"){
          check = F
          break()
        }
      }
      
      if(check){
        filterlist = list()
        for (i in 1:counter$n){
          if(length(eval(parse(text = paste0("input$FilterUiElement_",i)))) == 2){
            min = min(eval(parse(text = paste0("input$FilterUiElement_",i))))
            max = max(eval(parse(text = paste0("input$FilterUiElement_",i))))
            data = as.numeric(as.character(PixelSetExploRV$TAB[PixelSetExploRV$SEARCH ,eval(parse(text = paste0("input$col_",i)))]))
            
            if(eval(parse(text = paste0("input$FilterUiElementRGB_",i))) == "Gray"){
              pos = which(data <= min | data >= max )
            } else {
              pos = which(data >= min & data <= max )
            }
            
            filterlist = c(filterlist,
                           list(PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,][pos,1]))
          } else{
            filterlist = c(filterlist,
                           list(PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,][grep(eval(parse(text = paste0("input$FilterUiElement_",i))),PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,eval(parse(text = paste0("input$col_",i)))] , ignore.case = T, value = F),1]))
          }
          
        } 
        
        if(input$joinType == "Full"){
          filterlist = paste(unique(unlist(filterlist)), collapse = " ; ")
        } else if(input$joinType == "Inner"){
          filterlist = paste(Reduce(intersect, filterlist), collapse = " ; ")
        }
        
        updateTextAreaInput(session,inputId = "MPS_searchGenelist", value = filterlist)
        PixelSetExploRV$SEARCH = which(PixelSetExploRV$TAB[, "Feature name"] %in% unlist(strsplit(gsub(" ","", filterlist), ";")))
        
        if( length(PixelSetExploRV$SEARCH) == 0){
          PixelSetExploRV$SEARCH = 1:nrow(PixelSetExploRV$TAB)
          sendSweetAlert(
            session = session,
            title = "Oops!",
            text = "None of the genes were found in the Pixel table.",
            type = "error"
          )
        }
      } else {
        sendSweetAlert(
          session = session,
          title = "Oops!",
          text = "A filters isn't selected...",
          type = "error"
        )
      }
    } else {
      sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "No filters selected...",
        type = "warning"
      )
    }
  }
  )
  
  observeEvent(input$filter_clear_btn, {
    updateTextAreaInput(session,inputId = "MPS_searchGenelist", value = "")
    PixelSetExploRV$SEARCH = 1:nrow(PixelSetExploRV$TAB)
    counter$n = 0
    prevcount$n = 0
    
    for(n in names(MPS_RV$val)){
      MPS_RV$val[[n]] <- NULL
    }
    MPS_RV$choice = NULL
    
  })
  
  #-----------------------------------------------------------------------------
  # MultiPixelSets : Download table
  #-----------------------------------------------------------------------------
  
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
  
  output$PSExploContent <- renderDT(PIXELSETLIST_RV$infoWithTags[PIXELSETLIST_RV$Selected,][input$PIXELSETLIST_tab_rows_selected,],
                                    selection = 'single', options = list(
                                      scrollX = TRUE,searchHighlight = TRUE
                                    ))
  
  observeEvent(input$PSExploContent_rows_selected,{
    SEARCH_RV$PIXELSET = PIXELSETLIST_RV$infoWithTags[PIXELSETLIST_RV$Selected,][input$PIXELSETLIST_tab_rows_selected,][input$PSExploContent_rows_selected,1]
    proxy = dataTableProxy('PSExploContent')
    proxy %>% selectRows(NULL)
  })
  
  output$PSExploTab <- renderDT(PixelSetExploRV$TAB[PixelSetExploRV$SEARCH,],
                                selection = 'none',
                                editable = F, filter = 'top',
                                options = list(
                                  scrollX = TRUE,searchHighlight = TRUE,
                                  search = list(regex = TRUE, caseInsensitive = TRUE)
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
    SEARCH_RV$PIXELSET = CF$PIXELSET_quantitative[input$CF_PixelSET_rows_selected,"Pixelset ID"]
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
                          select PS.id as",'"',"ID",'"', ",PS.Name as ",'"',"Name",'"', ",PS.Description as ",'"',"Description",'"', ", PS.pixelSet_file,  PS.pixelSet_file as ",'"',"Filename",'"',", species.name as ",'"',"Species",'"',", OUT.name as ",'"',"Omics Unit Type",'"',", OmicsArea.name as ",'"',"Omics Area",'"',", pixeler.user_name as ",'"',"User name",'"',", analysis.description as ",'"',"Analysis",'"',", experiment.description as ",'"',"Experiment",'"',", PS.id_submission as ",'"',"Submission",'"',"
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
    PIXELSET_RV$info[1,"Submission"] = paste0("<a href='Submissions/",PIXELSET_RV$info[1,"Submission"],".zip' target='_blank'>",PIXELSET_RV$info[1,"Submission"],"</a>")
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
  
  output$PixelSet_explo_Pixel <- renderDT( {
    PIXELSET_RV$Pixel[PIXELSET_RV$SEARCH,]
  }, 
  server = FALSE,
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
      if(!is.na(as.numeric(PIXELSET_RV$Pixel[input$PixelSet_explo_Pixel_rows_all,"Value"][1])) ){
        Hist <-gvisHistogram(data.frame(Value = as.numeric(PIXELSET_RV$Pixel[input$PixelSet_explo_Pixel_rows_all,"Value"])), options=list(
          colors="['#ff0000']",
          legend="{ position: 'none'}",
          title="Values",
          width='100%', height=360),
          "PixelSetHistoValue")
        Hist
      } else {
        NULL
      }
      
    } else{
      NULL
    }
    
    
  })
  
  output$PixelSetHistoQS <- renderGvis({
    
    if(is.null(PIXELSET_RV$Pixel)){
      NULL
    } else if(!is.null(PIXELSET_RV$Pixel) & nrow(PIXELSET_RV$Pixel) != 0 ){
      if(sum(is.na(PIXELSET_RV$Pixel[input$PixelSet_explo_Pixel_rows_all,"Quality score"]) ) != length(PIXELSET_RV$Pixel[input$PixelSet_explo_Pixel_rows_all,"Quality score"])){
        gvisHistogram(data.frame(QS = PIXELSET_RV$Pixel[input$PixelSet_explo_Pixel_rows_all,"Quality score"]), 
                      options=list(
                        colors="['#3366ff']",
                        legend="{ position: 'none'}",
                        title="Quality scores",
                        width='100%', height=360),
                      "PixelSetHistoQS")
      } else {
        NULL 
      }
      
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
  
  pg <- dbDriver("PostgreSQL")
  con <- dbConnect(pg, user="docker", password="docker",
                   host=ipDB, port=5432)
  on.exit(dbDisconnect(con))
  
  TAG$ALLAnalysis = dbGetQuery(con,paste0("select tag.name, tag.description, count(*)
                                        from tag, Tag_Analysis
                                        where tag.id = tag_Analysis.id_tag
                                        group by tag.id
                                        ORDER by count(*) DESC;"))
  
  TAG$ALLExperiment = dbGetQuery(con,paste0("select tag.name, tag.description, count(*)
                                        from tag,  Tag_Experiment
                                        where tag.id = Tag_Experiment.id_tag
                                        group by tag.id
                                        ORDER by count(*) DESC;"))
  dbDisconnect(con)
  
  observeEvent(TAG$ALLExperiment,{
    if(!is.null(TAG$ALLExperiment) && nrow(TAG$ALLExperiment) != 0 && 
       !is.null(TAG$ALLAnalysis) && nrow(TAG$ALLAnalysis) != 0){
      TAG$ALL = merge(TAG$ALLAnalysis, TAG$ALLExperiment  ,by = "name", all = T)
      TAG$ALL = cbind(TAG$ALL[, 1:2],apply(TAG$ALL[,c(3,5)], 1, sum, na.rm = T))
      colnames(TAG$ALL) = c("Name", "Description", "Count")
      TAG$ALL = TAG$ALL[order(TAG$ALL[,"Count"], decreasing = T),]
      
      TAG$BAR = merge(TAG$ALLAnalysis, TAG$ALLExperiment  ,by = "name", all = T)
      TAG$BAR = cbind(TAG$BAR[, c(1,3,5)],apply(TAG$BAR[,c(3,5)], 1, sum, na.rm = T))
      colnames(TAG$BAR) = c("Name", "Analysis", "Experiment", "Sum")
      
      if(nrow(TAG$BAR) > 10){
        nbr = 10
      } else {
        nbr = nrow(TAG$BAR)
      }
      
      TAG$BAR = TAG$BAR[order(TAG$BAR[,"Sum"], decreasing = T)[1:nbr], ]
      TAG$BAR[is.na(TAG$BAR)] <- 0
    }
  })
  
  output$tagName<- renderUI({
    if(!is.null(TAG$NAME) && length(TAG$NAME) != 0){
      div(h2("Tags -", tags$span(class="TagName",TAG$NAME)))
    } else {
      h2("Tags")
    }
    
  })
  
  observeEvent(TAG$NAME,{
    updateTabItems (session, "tabs", selected = "Tags")
    shinyjs::runjs("window.scrollTo(0, 0)")
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    TAG$DESCRIPTION = dbGetQuery(con,paste0("select name, description
                                            from tag
                                            where name = '",TAG$NAME,"';"))
    if(TAG$DESCRIPTION[1,2] == ""){
      TAG$DESCRIPTION = "No description saved"
    } else {
      TAG$DESCRIPTION = TAG$DESCRIPTION[1,2]
    }
    
    TAG$ALLAnalysis = dbGetQuery(con,paste0("select tag.name, tag.description, count(*)
                                        from tag, Tag_Analysis
                                        where tag.id = tag_Analysis.id_tag
                                        group by tag.id
                                        ORDER by count(*) DESC;"))
    
    TAG$ALLExperiment = dbGetQuery(con,paste0("select tag.name, tag.description, count(*)
                                        from tag,  Tag_Experiment
                                        where tag.id = Tag_Experiment.id_tag
                                        group by tag.id
                                        ORDER by count(*) DESC;"))
    
    
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
                                  options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  output$Tag_experiment <- renderDT(TAG$PIXEL_SET_EXP, 
                                    selection = 'single', 
                                    editable = F,
                                    options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  output$Tag_All <- renderDT(TAG$ALL,selection = 'single',
                             editable = F,rownames= FALSE, 
                             options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  observeEvent(input$Tag_All_rows_selected,{
    
    TAG$NAME = TAG$ALL[input$Tag_All_rows_selected,1]
    
    proxy = dataTableProxy('Tag_All')
    proxy %>% selectRows(NULL)
  })
  
  
  
  output$TagBar <- renderGvis({
    if(!is.null(TAG$BAR) && nrow(TAG$BAR) != 0 ){
      gvisColumnChart(TAG$BAR,
                      options=list(title="Best tags", height = 300))
    } else {
      NULL
    }
  })
  
  output$TagDecription <- renderText({
    TAG$DESCRIPTION
  })
  
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
  # SUBMISSION FOLDER
  #=============================================================================
  
  SubFolder = reactiveValues()
  
  pg <- dbDriver("PostgreSQL")
  con <- dbConnect(pg, user="docker", password="docker",
                   host=ipDB, port=5432)
  on.exit(dbDisconnect(con))
  
  SubFolder$Tab = dbGetQuery(con,"select DISTINCT submission.id, analysis.description, experiment.description, pixeler.user_name 
                                  from submission, pixelset, experiment, analysis_experiment, analysis, pixeler 
                             where pixelset.id_submission = submission.id 
                             and submission.pixeler_user_id = pixeler.id
                             and pixelset.id_analysis = analysis.id
                             and analysis_experiment.id_analysis = analysis.id
                             and analysis_experiment.id_experiment = experiment.id ;")
  dbDisconnect(con)
  
  observeEvent(SubFolder$Tab, {
    if(ncol(SubFolder$Tab) == 4){
      SubFolder$Tab = cbind(SubFolder$Tab, paste0("<a href='Submissions/",SubFolder$Tab[,1],".zip' target='_blank'>Download</a>"))
      colnames( SubFolder$Tab) = c("ID","Analysis description","Experiment analysis", "Pixeler", "File")
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      SubFolder$TabModif =  dbGetQuery(con,"select DISTINCT submission.id, analysis.description, 
                                            experiment.description,submission.status, strain.name, omicsunittype.name, omicsarea.name
                                            from submission, pixelset, experiment, analysis_experiment, analysis, strain, pixel, omicsunittype, omicsArea
                                            where pixelset.id_submission = submission.id 
                                             and pixelset.id_analysis = analysis.id
                                             and analysis_experiment.id_analysis = analysis.id
                                             and analysis_experiment.id_experiment = experiment.id
                                       and experiment.strainId = strain.id
                                       and pixel.pixelSet_id = pixelset.id
                                       and omicsunittype.id = pixel.OmicsUnitType_id
                                       and experiment.omicsAreaid = omicsarea.id;")
      
      colnames(SubFolder$TabModif) = c("ID", "Analysis description", "Experiment description", "Validated?", "Strain", "OmicsUnitType", "OmicsArea")
      
      dbDisconnect(con)
    }
  })
  
  output$submissionFolderTab <- renderDT(SubFolder$Tab, 
                                         selection = 'single', 
                                         editable = F,escape = 3,
                                         options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  SubFolder$TabID = NULL
  
  observeEvent(input$submissionFolderTab_rows_selected, {
    if(!is.null(input$submissionFolderTab_rows_selected)){
      
      if(!is.null(SubFolder$TabID)){
        for(i in SubFolder$TabID){
          removeTab("tab_sub_PS", i)
        }
        SubFolder$TabID = NULL
      }
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      SubFolder$subID = SubFolder$Tab[input$submissionFolderTab_rows_selected,1]
      
      
      SubFolder$infoG = dbGetQuery(con,paste0("SELECT a.description, E.description, PS.id, PS.name, PS.description, strain.name, species.name
                                              FROM pixelset PS , submission, analysis A, experiment E, analysis_experiment AE, strain, species
                                              WHERE submission.id = '", SubFolder$Tab[input$submissionFolderTab_rows_selected,1] ,"'
                                              AND PS.id_submission = submission.id
                                              AND PS.id_analysis = A.id
                                              AND A.id = AE.id_analysis
                                              AND AE.id_experiment = E.id
                                              AND E.strainId = strain.id
                                              AND strain.species_id = species.id
                                              ORDER BY PS.name;")) 
      
      SubFolder$infoAnalysis = SubFolder$infoG[1,1]
      SubFolder$infoExperiment = SubFolder$infoG[1,2]
      
      SubFolder$infoStrain = SubFolder$infoG[1,6]
      SubFolder$infoSpecies = SubFolder$infoG[1,7]
      
      SubFolder$TagA = dbGetQuery(con,paste0("select tag.name 
      from pixelset,  tag_Analysis, tag 
      where pixelSet.id_submission = '", SubFolder$Tab[input$submissionFolderTab_rows_selected,1] ,"'
      and pixelset.id_analysis = tag_analysis.id_analysis
      and tag.id = tag_analysis.id_tag; "))
      
      SubFolder$TagE = dbGetQuery(con,paste0("select tag.name 
      from pixelset,  analysis_experiment, Tag_Experiment, tag
      where pixelSet.id_submission = '", SubFolder$Tab[input$submissionFolderTab_rows_selected,1] ,"'
      and pixelset.id_analysis = analysis_experiment.id_analysis
      and analysis_experiment.id_experiment = Tag_Experiment.id_experiment
      and tag.id = Tag_Experiment.id_tag;"))
      
      if(nrow(SubFolder$TagA) != 0){
        SubFolder$TagA = paste(SubFolder$TagA[,1], collapse = " | ")
      } else {
        SubFolder$TagA = ""
      }
      
      if(nrow(SubFolder$TagE) != 0){
        SubFolder$TagE = paste(SubFolder$TagE[,1], collapse = " | ")
      } else {
        SubFolder$TagE = ""
      }
      
      if(nrow(SubFolder$infoG) != 0){
        for(i in 1:nrow(SubFolder$infoG)){
          
          result = paste("<h4>",SubFolder$infoG[i,3],"</h4>",
                         "<p><b>Name</b>",SubFolder$infoG[i,4],"</p>",
                         "<p><b>Description</b>",SubFolder$infoG[i,5],"</p>")
          
          if(i == 1){
            appendTab("tab_sub_PS", tabPanel(paste0("PS",i), HTML(result)),select = T)
          } else {
            appendTab("tab_sub_PS", tabPanel(paste0("PS",i), HTML(result)),select = F)
          }
          
          SubFolder$TabID = c(SubFolder$TabID, paste0("PS",i))
        }
      }
      
      SubFolder$infoPixel = dbGetQuery(con,paste0("select count(*)
                            from pixel, pixelSet 
                            where pixelset.id_submission ='", SubFolder$subID ,"'
                            and pixel.pixelSet_id = pixelset.id;"))[1,1]
      
    }
  })
  sel <- reactive({!is.null(input$submissionFolderTab_rows_selected)}) 
  
  output$submissionFolderInfo <- renderUI(
    div(
      h3(class = "h3-style","Supplementary information"),
      p(class="info", "Click on a line to have more information about submission."),
      h2(SubFolder$subID),
      p(tags$span(class="bold","Description analysis")," :",SubFolder$infoAnalysis),
      p(tags$span(class="bold","Description experiment")," :",SubFolder$infoExperiment),
      p(tags$span(class="bold","Strain")," :",SubFolder$infoStrain),
      p(tags$span(class="bold","Species")," :",SubFolder$infoSpecies),
      p(tags$span(class="bold","Pixel number")," :",SubFolder$infoPixel),
      p(tags$span(class="bold","Tag Analysis")," :", SubFolder$TagA ),
      p(tags$span(class="bold","Tag Experiment")," : ", SubFolder$TagE)
    )
  )
  
  #=============================================================================
  # END SUBMISSION FOLDER
  #=============================================================================
  
  output$DatasourceTab <- renderDT({
    colnames(datasourceRV$tab) = c("Datasource name", "Datasource description", "Published?", "Datasource URL","Pixelset ID",
                                   "Pixelset name", "Pixelset description", "Omics area")
    
    datasourceRV$tab[,"Datasource name"] = as.factor(datasourceRV$tab[,"Datasource name"])
    datasourceRV$tab[,"Published?"] = as.factor(datasourceRV$tab[,"Published?"])
    datasourceRV$tab[,"Omics area"] = as.factor(datasourceRV$tab[,"Omics area"])
    datasourceRV$tab
  }, 
  selection = 'single', 
  editable = F,  server = TRUE,
  filter = 'top',
  options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  observeEvent(input$DatasourceTab_rows_selected,{
    SEARCH_RV$PIXELSET = datasourceRV$tab[input$DatasourceTab_rows_selected,"Pixelset ID"]
    proxy = dataTableProxy('DatasourceTab')
    proxy %>% selectRows(NULL)
  })
  
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
  datasourceRV$tab = dbGetQuery(con, "SELECT DataSource.name, DataSource.description, DataSource.published, DataSource.url,
                                      pixelset.id, pixelset.name, PixelSet.description, omicsarea.name
                                FROM Datasource, experiment, analysis_experiment,pixelset, OmicsArea
                                WHERE experiment.omicsareaid = OmicsArea.id 
                                AND experiment.DataSourceId = DataSource.id
                                AND analysis_experiment.id_experiment = experiment.id
                                AND analysis_experiment.id_analysis = pixelset.id_analysis
                                ;")
  
  AddRV$OmicsArea = dbGetQuery(con,"SELECT * from OmicsArea ORDER BY path;")
  AddRV$Species = dbGetQuery(con,"SELECT * from species;")
  AddRV$Strain = dbGetQuery(con,"SELECT * from strain;")
  AddRV$StrainSpecies = dbGetQuery(con,"select strain.name as Strain, species.name as Species from strain, species where species.id = strain.species_id;")
  
  dbDisconnect(con)
  
  #-----------------------------------------------------------------------------
  # Add OUT
  #-----------------------------------------------------------------------------
  
  output$DT_AddOUT <- renderDT(AddRV$OUT, selection = 'none', 
                               editable = TRUE,
                               options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  
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
          on.exit(dbDisconnect(con))
          dbGetQuery(con, REQUEST)
          dbDisconnect(con)
          
          MAJ$value = MAJ$value + 1
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
    on.exit(dbDisconnect(con))
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
      on.exit(dbDisconnect(con))
      AddRV$OUT = dbGetQuery(con, REQUEST)
      dbDisconnect(con)
      updateTextInput(session, "Name_OUT", value = "")
      updateTextInput(session, "Description_OUT", value = "")
    }
  })
  
  #-----------------------------------------------------------------------------
  # Remove OUT
  #-----------------------------------------------------------------------------
  
  observeEvent(input$Delete_OUT_btn,{
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    Used = dbGetQuery(con,paste0("SELECT distinct pixelSet_id from Pixel , OmicsUnitType 
                                 WHERE pixel.OmicsUnitType_id = OmicsUnitType.id 
                                 AND OmicsUnitType.name ='",input$Delete_OUT_SI,"';"))
    dbDisconnect(con)
    
    if(nrow(Used) != 0){
      sendSweetAlert(
        session = session,
        title = "Omics unit type used",
        text = paste("This Omics unit type is used (", paste(Used[,1], collapse = ", "),")"),
        type = "error"
      )
    }else {
      confirmSweetAlert(
        session = session,
        inputId = "confirm_delete_OUT",
        type = "warning",
        title = "Want to confirm ?",
        text = paste("Delete Omics unit type  :",input$Delete_OUT_SI , "?" ),
        danger_mode = TRUE
      )
    }
  }
  )
  
  observeEvent(input$confirm_delete_OUT, {
    if (isTRUE(input$confirm_delete_OUT)) {
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      dbGetQuery(con,paste0("delete from OmicsUnitType where OmicsUnitType.name ='",input$Delete_OUT_SI,"';"))
      
      REQUEST = "SELECT * FROM OmicsUnitType;"
      AddRV$OUT = dbGetQuery(con, REQUEST)
      updateSelectInput(session, 'Delete_OUT_SI', choices = AddRV$OUT[,'name'])
      dbDisconnect(con)
    } else {
      sendSweetAlert(
        session = session,
        title = "Cancellation...",
        text = "Deletion cancelled !",
        type = "warning"
      )
    }
  })
  
  output$Delete_OUT = renderUI({
    selectInput('Delete_OUT_SI', NULL, AddRV$OUT[,'name'])
  })
  
  #-----------------------------------------------------------------------------
  # Add Datasource
  #-----------------------------------------------------------------------------
  
  output$DT_AddDataSource <- renderDT(AddRV$DataSource, selection = 'none', 
                                      editable = TRUE,
                                      options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  
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
          on.exit(dbDisconnect(con))
          dbGetQuery(con, REQUEST)
          dbDisconnect(con)
          
          MAJ$value = MAJ$value + 1
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
    on.exit(dbDisconnect(con))
    
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
      on.exit(dbDisconnect(con))
      AddRV$DataSource = dbGetQuery(con, REQUEST)
      datasourceRV$tab = dbGetQuery(con, "SELECT DataSource.name, DataSource.description, DataSource.published, DataSource.url,
                                      pixelset.id, pixelset.name, PixelSet.description, omicsarea.name
                                    FROM Datasource, experiment, analysis_experiment,pixelset, OmicsArea
                                    WHERE experiment.omicsareaid = OmicsArea.id 
                                    AND experiment.DataSourceId = DataSource.id
                                    AND analysis_experiment.id_experiment = experiment.id
                                    AND analysis_experiment.id_analysis = pixelset.id_analysis
                                    ;")
      
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
    on.exit(dbDisconnect(con))
    
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
      on.exit(dbDisconnect(con))
      
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
    on.exit(dbDisconnect(con))
    
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
    
    updateSelectInput(session, 'Delete_branch_OmicsArea_SI', choices = AddRV$OmicsArea[! AddRV$OmicsArea[,'name'] %in% c("Annotation", "GO Terms", "Sequence", "Area"),'name'])
    updateSelectInput(session, 'Modify_OmicsArea_name_SI', choices = AddRV$OmicsArea[,'name'])
    updateSelectInput(session, 'Modify_OmicsArea_path_SI', choices = choices)
    updateSelectInput(session, 'Add_OmicsArea_path_SI', choices = choices)
    textInput('Modify_OmicsArea_description_TI', NULL, value = AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Modify_OmicsArea_name_SI),'description'] )
    dbDisconnect(con)
  })
  
  observeEvent(input$Delete_branch_OmicsArea_btn,{
    
    
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    Used = dbGetQuery(con,paste0("select experiment.id from experiment, OmicsArea where experiment.omicsareaid = omicsarea.id and omicsarea.name ='",input$Delete_branch_OmicsArea_SI,"';"))
    dbDisconnect(con)
    
    if(nrow(Used) != 0){
      sendSweetAlert(
        session = session,
        title = "OmicArea used",
        text = paste("This OmicsArea is used (", paste(Used[,1], collapse = ","),")"),
        type = "error"
      )
    }else {
      confirmSweetAlert(
        session = session,
        inputId = "confirm_delete_OA",
        type = "warning",
        title = "Want to confirm ?",
        text = paste("Delete branch :",AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Delete_branch_OmicsArea_SI),'path'] , "?" ),
        danger_mode = TRUE
      )
    }
  }
  )
  
  observeEvent(input$confirm_delete_OA, {
    if (isTRUE(input$confirm_delete_OA)) {
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
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
      
      updateSelectInput(session, 'Delete_branch_OmicsArea_SI', choices = AddRV$OmicsArea[! AddRV$OmicsArea[,'name'] %in% c("Annotation", "GO Terms", "Sequence", "Area"),'name'])
      updateSelectInput(session, 'Modify_OmicsArea_name_SI', choices = AddRV$OmicsArea[,'name'])
      updateSelectInput(session, 'Modify_OmicsArea_path_SI', choices = choices)
      updateSelectInput(session, 'Add_OmicsArea_path_SI', choices = choices)
      textInput('Modify_OmicsArea_description_TI', NULL, value = AddRV$OmicsArea[which(AddRV$OmicsArea[,'name'] == input$Modify_OmicsArea_name_SI),'description'] )
      dbDisconnect(con)
    } else {
      sendSweetAlert(
        session = session,
        title = "Cancellation...",
        text = "Deletion cancelled !",
        type = "warning"
      )
    }
  })
  
  
  output$Delete_branch_OmicsArea = renderUI({
    selectInput('Delete_branch_OmicsArea_SI', NULL, AddRV$OmicsArea[! AddRV$OmicsArea[,'name'] %in% c("Annotation", "GO Terms", "Sequence", "Area"),'name'])
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
                                   options = list(scrollX = TRUE, searchHighlight = TRUE))
  
  
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
          on.exit(dbDisconnect(con))
          dbGetQuery(con, REQUEST)
          AddRV$Species = dbGetQuery(con, "SELECT * FROM species;")
          AddRV$StrainSpecies = dbGetQuery(con,"select strain.name as Strain, species.name as Species from strain, species where species.id = strain.species_id;")
          dbDisconnect(con)
          
          MAJ$value = MAJ$value + 1
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
    on.exit(dbDisconnect(con))
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
      on.exit(dbDisconnect(con))
      AddRV$Species = dbGetQuery(con, REQUEST)
      dbDisconnect(con)
      
      updateTextInput(session, "Name_Species", value = "")
      updateTextInput(session, "Description_Species", value = "")
      updateTextInput(session, "URL_Species", value = "")
      updateSelectInput(session, "Species_Strain_SI", choices = AddRV$Species[,'name'])
    }
  })
  
  
  #-----------------------------------------------------------------------------
  # Remove species
  #-----------------------------------------------------------------------------
  
  observeEvent(input$Delete_species_btn,{
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    Used = dbGetQuery(con,paste0("SELECT distinct strain.name from strain, species
                                 WHERE strain.species_id = species.id
                                 AND species.name ='",input$Delete_species_SI,"';"))
    dbDisconnect(con)
    
    if(nrow(Used) != 0){
      sendSweetAlert(
        session = session,
        title = "Species used",
        text = paste("This species is used for this/these strain(s) :", paste(Used[,1], collapse = ", ")),
        type = "error"
      )
    }else {
      confirmSweetAlert(
        session = session,
        inputId = "confirm_delete_species",
        type = "warning",
        title = "Want to confirm ?",
        text = paste("Delete species  :",input$Delete_species_SI , "?" ),
        danger_mode = TRUE
      )
    }
  }
  )
  
  observeEvent(input$confirm_delete_species, {
    if (isTRUE(input$confirm_delete_species)) {
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      dbGetQuery(con,paste0("delete from species where species.name ='",input$Delete_species_SI,"';"))
      
      REQUEST = "SELECT * FROM species;"
      AddRV$Species = dbGetQuery(con, REQUEST)
      updateSelectInput(session, 'Delete_species_SI', choices = AddRV$Species[,'name'])
      updateSelectInput(session, "Species_Strain_SI", choices = AddRV$Species[,'name'])
      dbDisconnect(con)
    } else {
      sendSweetAlert(
        session = session,
        title = "Cancellation...",
        text = "Deletion cancelled !",
        type = "warning"
      )
    }
  })
  
  output$Delete_species = renderUI({
    selectInput('Delete_species_SI', NULL, AddRV$Species[,'name'])
  })
  
  #-----------------------------------------------------------------------------
  # Add Strain
  #-----------------------------------------------------------------------------
  
  output$DT_AddStrain <- renderDT(AddRV$Strain, selection = 'none',
                                  editable = TRUE,
                                  options = list(scrollX = TRUE, searchHighlight = TRUE))
  
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
          on.exit(dbDisconnect(con))
          dbGetQuery(con, REQUEST)
          on.exit(dbDisconnect(con))
          AddRV$Strain = dbGetQuery(con, "SELECT * FROM strain;")
          AddRV$StrainSpecies = dbGetQuery(con,"select strain.name as Strain, species.name as Species from strain, species where species.id = strain.species_id;")
          dbDisconnect(con)
          
          MAJ$value = MAJ$value + 1
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
    on.exit(dbDisconnect(con))
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
      on.exit(dbDisconnect(con))
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
  
  #-----------------------------------------------------------------------------
  # Remove strain
  #-----------------------------------------------------------------------------
  
  observeEvent(input$Delete_strain_btn,{
    pg <- dbDriver("PostgreSQL")
    con <- dbConnect(pg, user="docker", password="docker",
                     host=ipDB, port=5432)
    on.exit(dbDisconnect(con))
    
    Used = dbGetQuery(con,paste0("SELECT distinct pixelset.id from strain, experiment, Analysis_Experiment, pixelset
                                 WHERE strain.id = experiment.strainId
                                 AND experiment.id = Analysis_Experiment.id_experiment
                                 AND Analysis_Experiment.id_analysis = pixelset.id_analysis
                                 AND strain.name ='",input$Delete_strain_SI,"';"))
    dbDisconnect(con)
    
    if(nrow(Used) != 0){
      sendSweetAlert(
        session = session,
        title = "Strain  used",
        text = paste("This Strain is used for this/these Pixelset(s) :", paste(Used[,1], collapse = ", ")),
        type = "error"
      )
    }else {
      confirmSweetAlert(
        session = session,
        inputId = "confirm_delete_strain",
        type = "warning",
        title = "Want to confirm ?",
        text = paste("Delete strain  :",input$Delete_strain_SI , "?" ),
        danger_mode = TRUE
      )
    }
  }
  )
  
  observeEvent(input$confirm_delete_strain, {
    if (isTRUE(input$confirm_delete_strain)) {
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      dbGetQuery(con,paste0("delete from strain where strain.name ='",input$Delete_strain_SI,"';"))
      
      REQUEST = "SELECT * FROM strain;"
      AddRV$Strain = dbGetQuery(con, REQUEST)
      AddRV$StrainSpecies = dbGetQuery(con,"select strain.name as Strain, species.name as Species from strain, species where species.id = strain.species_id;")
      updateSelectInput(session, 'Delete_strain_SI', choices = AddRV$Strain[,'name'])
      dbDisconnect(con)
    } else {
      sendSweetAlert(
        session = session,
        title = "Cancellation...",
        text = "Deletion cancelled !",
        type = "warning"
      )
    }
  })
  
  output$Delete_strain = renderUI({
    if(!is.null(AddRV$Strain) && nrow(AddRV$Strain) != 0){
      selectInput('Delete_strain_SI', NULL, AddRV$Strain[,'name'])
    }
  })
  
  
  
  
  #=============================================================================
  # Submission
  #=============================================================================
  
  submissionRV = reactiveValues()
  submissionRV$Read = F
  
  observe({
    if(is.null(input$fileCF)){
      disable("ImportCF")
    } else{
      enable("ImportCF")
    }
  })
  
  observeEvent(input$zip,{
    if(is.null(input$zip)){
      disable("extractZip")
    } else{
      enable("extractZip")
    }
  })
  
  observeEvent(input$extractZip,{
    file$extractError = NULL
    file$extractMD5 = NULL
    
    if(dir.exists(paste0("www/Submissions/tmp_",isolate(input$USER)))){
      unlink(paste0("www/Submissions/tmp_",isolate(input$USER)), recursive = T, force = T)
    }
    
    if(nrow(AddRV$DataSource) == 0 ){
      sendSweetAlert(
        session = session,
        title = "Oops !!",
        text = "No data source are saved.  Please go here : Manage annotation > Data source ",
        type = "error"
      )
    } else if (nrow(AddRV$Strain) == 0){
      sendSweetAlert(
        session = session,
        title = "Oops !!",
        text = "No strains are saved.  Please go here : Manage annotation > Species & strains ",
        type = "error"
      )
      
    } else {
      # Create tmp folder
      file$address = paste0("www/Submissions/tmp_",isolate(input$USER))
      dir.create(file$address)
      setwd(file$address)
      
      # Copy zip folder in tmp and unzip
      file.copy(input$zip$datapath,"tmp.zip", overwrite = T )
      unzip("tmp.zip")
      file$addressFolder = paste0("www/Submissions/tmp_",isolate(input$USER), "/",list.dirs("./", recursive = F, full.names = F)[1] )
      setwd(list.dirs("./", recursive = F, full.names = F)[1])
      
      # Read most recent metafile
      metaDate = max(unlist(lapply(list.files("./", pattern = "meta_*"), function(x){regmatches(x,regexec("meta_(.*?).txt",x))[[1]][2]})))
      metadata = read.table(paste0("meta_",metaDate,".txt"), sep = "\t", header = F, stringsAsFactors = F)
      
      # information controls 
      
      if(! metadata[3,2] %in% AddRV$OmicsArea[,'name']){
        file$extractError = c(file$extractError, paste("<b>Omics area</b> :",metadata[3,2] ))
      }
      
      if(! metadata[4,2] %in% AddRV$OUT[,2]){
        file$extractError = c(file$extractError, paste("<b>Omics unit type</b> :",metadata[4,2] ))
      }
      
      if(! metadata[5,2] %in% AddRV$DataSource[,'name']){
        file$extractError = c(file$extractError, paste("<b>Data source</b> :",metadata[5,2] ))
      }
      
      if(! metadata[6,2] %in% AddRV$StrainSpecies[,"species"]){
        file$extractError = c(file$extractError, paste("<b>Species</b> :",metadata[6,2] ))
      }
      
      if(! metadata[7,2] %in% AddRV$StrainSpecies[,"strain"]){
        file$extractError = c(file$extractError, paste("<b>Strain</b> :",metadata[7,2] ))
      }
      
      if(!is.null(file$extractError)){
        sendSweetAlert(
          session = session,
          title = "Missing information!",
          text =HTML("<p>Some information is not known in the database. You will find the list below:</p>",
                     paste("<p>",file$extractError,"</p>", collapse = "<br/>"),
                     "<p>No information was imported into the database. Once the information has been saved, you can import this zip again. </p>"),
          type = "error", html = TRUE
        )
        
        setwd("../../../..")
        reset('zip')
        disable(id='extractZip')
        unlink(file$address, recursive = T, force = T)
        file$notebook_name = NULL
        file$SD_name = NULL
        file$address = NULL
        file$SD_address = NULL
        file$notebook_address = NULL
        
      } else {
        
        updateTextAreaInput(session = session, inputId = "submission_Exp_description", value = metadata[1,2])
        updateSelectInput(session = session, inputId = "submission_Exp_completionDate", selected = metadata[2,2])
        updateSelectInput(session = session, inputId = "Submission_Exp_omicsArea_SI", selected = metadata[3,2])
        updateSelectInput(session = session, inputId = "submission_pixelSet_OUT", selected = AddRV$OUT[which(AddRV$OUT[,2] == metadata[4,2]),1]  )
        updateSelectInput(session = session, inputId = "Submission_Exp_dataSource_SI", selected = metadata[5,2])
        updateSelectInput(session = session, inputId = "Submission_Exp_Species_SI", selected = metadata[6,2])
        updateSelectInput(session = session, inputId = "Submission_Exp_Strain_SI", selected = metadata[7,2])
        updateTextAreaInput(session = session, inputId = "submission_Analysis_description", value = metadata[8,2])
        updateSelectInput(session = session, inputId = "submission_Analysis_completionDate", selected = metadata[9,2])
        
        nbrPS = as.numeric(regmatches(metadata[nrow(metadata),1],regexec("PixelSet(.*?)_md5",metadata[nrow(metadata),1]))[[1]][2])
        updateSelectInput(session = session, inputId = "submission_pixelSet_nbr", 
                          selected = nbrPS)
        
        shinyjs::hide(id = "submission_Analysis_notebook")
        shinyjs::hide(id = "submission_Analysis_secondary_data")
        shinyjs::show(id = "submission_Analysis_notebook_name")
        shinyjs::show(id = "submission_Analysis_secondary_data_name")
        
        
        file$notebook_name = metadata[10,2]
        file$notebook_address = paste0(file$addressFolder, "/", metadata[10,2])
        
        file$SD_name = metadata[12,2]
        file$SD_address = paste0(file$addressFolder , "/", metadata[12,2])
        
        # Check md5
        if(metadata[10,2] != ""){
          if(md5sum(metadata[10,2]) != metadata[11,2]){
            file$extractMD5 = c(file$extractMD5,"<b>Notebook</b>"  )
          }
        }
        
        if(metadata[12,2] != ""){
          if(md5sum(metadata[12,2]) != metadata[13,2]){
            file$extractMD5 = c(file$extractMD5,"<b>Secondary data file</b>"  )
          }
        }
        
        # Multitable preparation
        
        if(!is.null(submissionRV$nbrPixelSet)){
          for(i in 1: submissionRV$nbrPixelSet){
            removeTab("tab_PixelSets", paste("PixelSet",i))
          }
        }
        
        submissionRV$nbrPixelSet = nbrPS
        
        for(i in 1:nbrPS){
          if(i == 1){
            appendTab("tab_PixelSets", tabPanel(paste("PixelSet",i), 
                                                h4("Name") ,
                                                textInput(paste0('submission_pixelSet_name_',i), NULL, value = metadata[(14 + 4*(i-1)),2]),
                                                h4("Description"),
                                                textAreaInput(paste0('submission_pixelSet_description_',i), NULL, resize = "vertical", value = metadata[(15 + 4*(i-1)),2]),
                                                h4("File"),
                                                disabled( textInput(paste0("submission_pixelSet_file",i), NULL, value = metadata[(16 + 4*(i-1)),2]))
                                                
            ),select = T)
            
          }else{
            appendTab("tab_PixelSets",  tabPanel(paste("PixelSet",i), 
                                                 h4("Name") ,
                                                 textInput(paste0('submission_pixelSet_name_',i), NULL, metadata[(14 + 4*(i-1)),2]),
                                                 h4("Description"),
                                                 textAreaInput(paste0('submission_pixelSet_description_',i), NULL, resize = "vertical", value = metadata[(15 + 4*(i-1)),2]),
                                                 h4("File"),
                                                 disabled(textInput(paste0("submission_pixelSet_file",i), NULL , value = metadata[(16 + 4*(i-1)),2]))
                                                 
            ),select = F)
            
          }
          
          if(md5sum(metadata[(16 + 4*(i-1)),2]) != metadata[(17 + 4*(i-1)),2]){
            file$extractMD5 = c(file$extractMD5,paste0("<b>Pixelset",i,"</b>")  )
          }
          
        }
        
        if(!is.null(file$extractMD5)){
          sendSweetAlert(
            session = session,
            title = "Missing information!",
            text =HTML("<p>The files were corrupted by the copy:</p>",
                       paste("<p>",file$extractMD5,"</p>", collapse = "<br/>"),
                       "<p>If you have made any changes, this message is not blocking. Otherwise, we advise you not to 
                       import this data into Pixel2 because it is different from the expected data. </p>"),
            type = "warning", html = TRUE
          )
        } else {
          sendSweetAlert(
            session = session,
            title = "md5 checked",
            text =HTML("The files were not corrupted by the copy"),
            type = "success", html = TRUE
          )
        }
        
        file$extractMD5 = NULL
        
        file$extract = T
        setwd("../../../..")
      }
    }
    
    
    
    
    
    
  })
  
  output$submission_Analysis_notebook_name = renderText(
    file$notebook_name
  )
  
  output$submission_Analysis_secondary_data_name = renderText(
    file$SD_name
  )
  
  
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
  
  pg <- dbDriver("PostgreSQL")
  con <- dbConnect(pg, user="docker", password="docker",
                   host=ipDB, port=5432)
  on.exit(dbDisconnect(con))
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
    on.exit(dbDisconnect(con))
    
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
    on.exit(dbDisconnect(con))
    
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
    file$extract = F
    reset('submission_Analysis_notebook')
    reset('submission_Analysis_secondary_data')
    updateTextAreaInput(session, "submission_Exp_description", value = "")
    updateTextAreaInput(session, "submission_Analysis_description", value = "")
    updateCheckboxGroupInput(session,"Submission_Exp_tags_CBG", selected = character(0))
    updateCheckboxGroupInput(session,"Submission_Analysis_tags_CBG", selected = character(0))
    updateSelectInput(session, "submission_pixelSet_nbr", selected = 2)
    updateSelectInput(session, "submission_pixelSet_nbr", selected = 1)
    submissionRV$Read = F
    
    shinyjs::show(id = "submission_Analysis_notebook")
    shinyjs::show(id = "submission_Analysis_secondary_data")
    shinyjs::hide(id = "submission_Analysis_notebook_name")
    shinyjs::hide(id = "submission_Analysis_secondary_data_name")
    
    reset('zip')
    disable(id='extractZip')
    unlink(file$address, recursive = T, force = T)
    file$notebook_name = NULL
    file$SD_name = NULL
    file$address = NULL
    file$SD_address = NULL
    file$notebook_address = NULL
    
    shinyjs::runjs("window.scrollTo(0, 0)")
  })
  
  observeEvent(input$Submission,{
    
    filename = NULL 
    for(i in 1:input$submission_pixelSet_nbr){
      if(!is.null(file$extract) && file$extract == T){
        filename = c(filename, eval(parse(text = paste0("input$submission_pixelSet_file",i))))
      } else {
        filename = c(filename, eval(parse(text = paste0("input$submission_pixelSet_file",i,"$datapath"))))
      }
    }
    
    if(nrow(AddRV$DataSource) == 0 ){
      sendSweetAlert(
        session = session,
        title = "Oops !!",
        text = "No data source are saved.  Please go here : Manage annotation > Data source ",
        type = "error"
      )
    } else if (nrow(AddRV$Strain) == 0){
      sendSweetAlert(
        session = session,
        title = "Oops !!",
        text = "No strains are saved.  Please go here : Manage annotation > Species & strains ",
        type = "error"
      )
      
    } else if (is.null(filename) || length(which(filename == "")) != 0  || length(filename) != input$submission_pixelSet_nbr) {
      sendSweetAlert(
        session = session,
        title = "Oops !!",
        text = "All files are not selected to import. Please check your files.",
        type = "error"
      )
    } else {
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
      #---------------------------------------------------------------------------
      # Check
      #---------------------------------------------------------------------------
      
      CF_Temp = dbGetQuery(con,paste0("SELECT feature_name from chromosomalfeature;"))
      
      if(nrow(CF_Temp) != 0){
        warning_sub = NULL
        duplicated_sub = NULL
        allCF = 0
        refusedCF = 0
        for( i in 1:input$submission_pixelSet_nbr){
          if(!is.null(file$extract) && file$extract == T){
            inter_warning <- read.csv2(paste0(file$addressFolder,"/",eval(parse(text = paste0("input$submission_pixelSet_file",i)))),
                                       header = as.logical(input$header_PS),
                                       sep = input$sep_PS,
                                       quote = input$quote_PS
            )
          } else {
            inter_warning <- read.csv2(eval(parse(text = paste0("input$submission_pixelSet_file",i,"$datapath"))),
                                       header = as.logical(input$header_PS),
                                       sep = input$sep_PS,
                                       quote = input$quote_PS
            )
          }
          
          pos = !(inter_warning[,1] %in% CF_Temp[,1])
          refused = inter_warning[pos,1]
          
          allCF = allCF + nrow(inter_warning)
          refusedCF = refusedCF +length(refused)
          
          warning_sub= c(warning_sub, paste0("<b>PixelSet ", i, "</b> <br/>", paste(refused, collapse = "\t")))
          duplicated_sub = c(duplicated_sub,paste0("<b>PixelSet ", i, "</b> <br/>", paste( unique(inter_warning[duplicated(inter_warning[,1]), 1]), collapse = "\t")) )
          
        }
        
        if(allCF == refusedCF){
          sendSweetAlert(
            session = session,
            title = "Oops !!",
            text = "No chromosomal features have been found for your genes. Possible reasons: (1) The chromosomal features are not in the database or (2) the data was not correctly read (The wrong column separator was used?)",
            type = "error"
          )
        }else {
          confirmSweetAlert(
            session = session,
            inputId = "confirm_submission_warning",
            type = "warning",
            title = "Want to confirm ?",
            text = HTML("<h3>Refused </h3><hr><p><i>NOTE : If you confirm, the pixels associated with the genes below will not be imported into the database</i></p><p><b>Percent </b>:",round(refusedCF * 100/allCF) ,"%</p>",
                        paste("<br/><p>",warning_sub,"</p>", collapse = "<br/>"), 
                        "<h3>Duplicated</h3><hr><p><i>NOTE : If you confirm, the duplicated pixels will be saved in the database. As a result, there will be redundancy when creating multipixelsets with these pixelsets.</i></p>",paste("<br/><p>",duplicated_sub,"</p>", collapse = "<br/>") ),
            danger_mode = TRUE,  html = TRUE
          )
        }
        
      } else {
        sendSweetAlert(
          session = session,
          title = "Oops !!",
          text = "There are no chromosomal features in the database",
          type = "error"
        )
      }
      
      dbDisconnect(con)
    }
  })
  
  
  observeEvent(input$confirm_submission_warning, {
    
    if (isTRUE(input$confirm_submission_warning)) {
      
      # Create a submission forlder
      time = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      id_Submission = paste0("Submission_",time)
      dir.create(paste0("www/Submissions/", id_Submission))
      
      #-------------------------------------------------------------------------
      # EXPERIMENT
      #-------------------------------------------------------------------------
      submissionRV$META = NULL
      # Add experiment in META
      submissionRV$META = rbind(submissionRV$META,
                                c("Experiment_description", input$submission_Exp_description),
                                c("Experiment_completionDate", input$submission_Exp_completionDate),
                                c("Experiment_omicsArea", input$Submission_Exp_omicsArea_SI),
                                c("Experiment_omicsUnitType", AddRV$OUT[which(AddRV$OUT[,1] == input$submission_pixelSet_OUT),2]),
                                
                                c("Experiment_dataSource", input$Submission_Exp_dataSource_SI),
                                c("Experiment_species", input$Submission_Exp_Species_SI),
                                c("Experiment_strain", input$Submission_Exp_Strain_SI)
      )
      
      
      
      pg <- dbDriver("PostgreSQL")
      con <- dbConnect(pg, user="docker", password="docker",
                       host=ipDB, port=5432)
      on.exit(dbDisconnect(con))
      
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
      
      #-------------------------------------------------------------------------
      # ANALYSIS
      #-------------------------------------------------------------------------
      
      # Add analysis in META
      
      if(is.null(input$submission_Analysis_notebook$name) && is.null(file$notebook_name)){
        file$notebook_name = ""
      } else if ( !is.null(input$submission_Analysis_notebook$name)) {
        file$notebook_name = input$submission_Analysis_notebook$name
      }
      
      if (is.null(input$submission_Analysis_secondary_data$name) && is.null(file$SD_name)){
        file$SD_name = ""
      } else if (!is.null(input$submission_Analysis_secondary_data$name)) {
        file$SD_name = input$submission_Analysis_secondary_data$name
      }
      
      
      # Add analysis
      id_analysis = paste0("Analysis_",time)
      adresse_analysis = paste0("Submissions/",id_Submission,"/")
      
      if(file$notebook_name == ""){
        adresse_analysis_notebook = ""
      } else {
        if(!is.null(input$submission_Analysis_secondary_data$name)){
          adresse_analysis_notebook = paste0(adresse_analysis,input$submission_Analysis_notebook$name)
          file.copy(input$submission_Analysis_notebook$datapath, paste0("www/", adresse_analysis_notebook))
        } else {
          adresse_analysis_notebook = paste0(adresse_analysis,file$notebook_name)
          file.copy(file$notebook_address,paste0("www/",adresse_analysis_notebook) )
        }
      }
      
      if(file$SD_name == ""){
        adresse_analysis_SD = ""
      } else {
        if(!is.null(input$submission_Analysis_secondary_data$name)){
          adresse_analysis_SD = paste0(adresse_analysis,input$submission_Analysis_secondary_data$name)
          file.copy(input$submission_Analysis_secondary_data$datapath, paste0("www/",adresse_analysis_SD))
        } else {
          adresse_analysis_SD = paste0(adresse_analysis,file$SD_name)
          file.copy(file$SD_address,  paste0("www/",adresse_analysis_SD) )
        }
        
      }
      
      
      md5_notebook = md5sum(paste0("www/",adresse_analysis_notebook))
      md5_notebook[is.na(md5_notebook)] = ""
      
      md5_SD = md5sum(paste0("www/",adresse_analysis_SD))
      md5_SD[is.na(md5_SD)] = ""
      
      submissionRV$META = rbind(submissionRV$META,
                                c("Analysis_description",input$submission_Analysis_description),
                                c("Analysis_completionDate",input$submission_Exp_completionDate),
                                c("Analysis_notebookFile",file$notebook_name),
                                c("md5_notebook", md5_notebook),
                                c("Analysis_secondaryDataFile",file$SD_name),
                                c("md5_secondaryDataFile", md5_SD)
                                
      )
      
      
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
      
      #-------------------------------------------------------------------------
      # SUBMISSION
      #-------------------------------------------------------------------------
      
      pixeler_user_id = dbGetQuery(con, paste0("SELECT id from pixeler where user_name='",isolate(input$USER),"';"))[1,1]
      REQUEST_Submission = paste0("insert into Submission (id, submission_date, status, pixeler_user_id) values('",id_Submission,"', to_date('",Sys.Date(),"', 'YYYY-MM-DD'), FALSE, '",pixeler_user_id,"');")
      dbGetQuery(con, REQUEST_Submission)
      
      #-------------------------------------------------------------------------
      # PIXELSET
      #-------------------------------------------------------------------------
      
      withProgress(message = 'PixelSet imported', value = 0, {
        m = as.numeric(input$submission_pixelSet_nbr)
        for( i in 1:input$submission_pixelSet_nbr){
          incProgress(1/m, detail = paste0("Imported :", floor(i/m*100),"%"))
          
          id_PixelSets = paste0("PixelSet_",time,"_",i)
          adresse_PixelSet = paste0("Submissions/",id_Submission,"/")
          
          if(!is.null(file$extract) && file$extract == T){
            adresse_analysis_file = paste0(adresse_PixelSet,eval(parse(text = paste0("input$submission_pixelSet_file",i))))
            file.copy(paste0(file$addressFolder,"/", eval(parse(text = paste0("input$submission_pixelSet_file",i)))), paste0("www/", adresse_analysis_file)) 
          } else {
            adresse_analysis_file = paste0(adresse_PixelSet,eval(parse(text = paste0("input$submission_pixelSet_file",i,"$name"))))
            file.copy(eval(parse(text = paste0("input$submission_pixelSet_file",i,"$datapath"))), paste0("www/", adresse_analysis_file)) 
          }
          
          
          REQUEST_PixelSet = paste0("insert into PixelSet (id, name, pixelSet_file, description, id_analysis, id_submission) values('",id_PixelSets,"', '",eval(parse(text = paste0("input$submission_pixelSet_name_",i))),"','",adresse_analysis_file,"','",eval(parse(text = paste0("input$submission_pixelSet_description_",i))),"','",id_analysis,"','",id_Submission,"');")
          dbGetQuery(con, REQUEST_PixelSet)
          
          if(!is.null(file$extract) && file$extract == T){
            inter <- read.csv2(paste0(file$addressFolder,"/",eval(parse(text = paste0("input$submission_pixelSet_file",i)))),
                               header = as.logical(input$header_PS),
                               sep = input$sep_PS,
                               quote = input$quote_PS
            )
            submissionRV$META = rbind(submissionRV$META,
                                      c(paste0("PixelSet",i,"_name"),eval(parse(text = paste0("input$submission_pixelSet_name_",i)))),
                                      c(paste0("PixelSet",i,"_description"),eval(parse(text = paste0("input$submission_pixelSet_description_",i)))),
                                      c(paste0("PixelSet",i,"_file"),eval(parse(text = paste0("input$submission_pixelSet_file",i)))),
                                      c(paste0("PixelSet",i,"_md5"),md5sum(paste0(paste0("www/", adresse_analysis_file))))
                                      
            )
          } else {
            inter <- read.csv2(eval(parse(text = paste0("input$submission_pixelSet_file",i,"$datapath"))),
                               header = as.logical(input$header_PS),
                               sep = input$sep_PS,
                               quote = input$quote_PS
            ) 
            
            submissionRV$META = rbind(submissionRV$META,
                                      c(paste0("PixelSet",i,"_name"),eval(parse(text = paste0("input$submission_pixelSet_name_",i)))),
                                      c(paste0("PixelSet",i,"_description"),eval(parse(text = paste0("input$submission_pixelSet_description_",i)))),
                                      c(paste0("PixelSet",i,"_file"),eval(parse(text = paste0("input$submission_pixelSet_file",i,"$name")))),
                                      c(paste0("PixelSet",i,"_md5"),md5sum(paste0(paste0("www/", adresse_analysis_file))))
                                      
            )
          }
          
          
          
          #---------------------------------------------------------------------
          # PIXEL
          #---------------------------------------------------------------------
          
          withProgress(message = 'Pixel', value = 0, {
            n = nrow(inter)
            for(j in 1:nrow(inter)){
              incProgress(1/n, detail = paste0("Imported :", floor(j/n*100),"%")) 
              REQUEST_Pixel = paste0("insert into Pixel (value, quality_score, pixelSet_id, cf_feature_name, OmicsUnitType_id) values('",gsub("\'","\'\'",inter[j,2]),"',", inter[j,3],",'",id_PixelSets,"','",inter[j,1],"','",input$submission_pixelSet_OUT,"');")
              dbGetQuery(con, REQUEST_Pixel)
            }
          })
        }
      })
      
      write.table(submissionRV$META, paste0("www/Submissions/", id_Submission,"/meta_",time,".txt") ,
                  sep = "\t", row.names = F, col.names= F,quote = F)
      
      setwd("www/Submissions/")
      zip(id_Submission, id_Submission)
      setwd("../..")
      
      SubFolder$Tab = dbGetQuery(con,"select DISTINCT submission.id, analysis.description, experiment.description, pixeler.user_name 
                                  from submission, pixelset, experiment, analysis_experiment, analysis, pixeler 
                                 where pixelset.id_submission = submission.id 
                                 and submission.pixeler_user_id = pixeler.id
                                 and pixelset.id_analysis = analysis.id
                                 and analysis_experiment.id_analysis = analysis.id
                                 and analysis_experiment.id_experiment = experiment.id ;")
      
      submissionRV$ZIP = paste0("www/Submissions/", id_Submission,".zip")
      
      #-------------------------------------------------------------------------
      # UPDATE 
      #-------------------------------------------------------------------------
      MAJ$value = MAJ$value + 1
      
      #-------------------------------------------------------------------------
      # CLEAR AFTER SUBMISSION
      #-------------------------------------------------------------------------
      
      file$extract = F
      reset('submission_Analysis_notebook')
      reset('submission_Analysis_secondary_data')
      updateTextAreaInput(session, "submission_Exp_description", value = "")
      updateTextAreaInput(session, "submission_Analysis_description", value = "")
      updateCheckboxGroupInput(session,"Submission_Exp_tags_CBG", selected = character(0))
      updateCheckboxGroupInput(session,"Submission_Analysis_tags_CBG", selected = character(0))
      updateSelectInput(session, "submission_pixelSet_nbr", selected = 2)
      updateSelectInput(session, "submission_pixelSet_nbr", selected = 1)
      submissionRV$Read = F
      
      
      shinyjs::show(id = "submission_Analysis_notebook")
      shinyjs::show(id = "submission_Analysis_secondary_data")
      shinyjs::hide(id = "submission_Analysis_notebook_name")
      shinyjs::hide(id = "submission_Analysis_secondary_data_name")
      
      if(!is.null(file$address) && dir.exists(file$address)){
        unlink( file$address, recursive = T, force = T)
      }
      
      reset('zip')
      disable(id='extractZip')
      file$notebook_name = NULL
      file$SD_name = NULL
      file$address = NULL
      file$SD_address = NULL
      file$notebook_address = NULL
      
      #-------------------------------------------------------------------------
      # MESSAGE
      #-------------------------------------------------------------------------
      
      updateTabItems (session, "tabs", selected = "Dashboard")
      sendSweetAlert(
        session = session,
        title = "Successfully imported!",
        text = div("The pixels have been successfully imported. Click on the button to download the zip folder of submission ",br(),br(),
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
    
    if(!is.null(file$extract) && file$extract == F){
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
                                              # actionButton(inputId = paste0('submissionPixelSetNameAuto_Sequence_',i), "Sequence", class="autoname"), 
                                              # actionButton(inputId = paste0('submissionPixelSetNameAuto_GO_',i), "GO terms", class="autoname"), 
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
                                               # actionButton(inputId = paste0('submissionPixelSetNameAuto_Sequence_',i), "Sequence", class="autoname"), 
                                               # actionButton(inputId = paste0('submissionPixelSetNameAuto_GO_',i), "GO terms", class="autoname"), 
                                               h4("Description"),
                                               textAreaInput(paste0('submission_pixelSet_description_',i), NULL, resize = "vertical"),
                                               fileInput(paste0("submission_pixelSet_file",i),label = NULL,
                                                         buttonLabel = "Browse...",
                                                         placeholder = "No file selected")
          ),select = F)
          
        }
        
      }
    }
    
    
  })
  
  observeEvent({input$last_btn},{
    
    id = unlist(strsplit({input$last_btn}, "_"))
    
    if(id[2] == "Sequence"){
      updateTextInput(session,inputId = paste0("submission_pixelSet_name_", id[3]), value = "Sequence" )
    } else if (id[2] == "GO"){
      updateTextInput(session,inputId = paste0("submission_pixelSet_name_", id[3]),value =  "GO terms" )
    } 
  }
  )
  
  
  
  output$submission_pixelSet_OUT_UI <- renderUI({
    choices = AddRV$OUT[,1]
    names(choices) = AddRV$OUT[,2]
    selectInput("submission_pixelSet_OUT",label = NULL,
                choices)
    
  })
  
  observeEvent(input$submission_pixelSet_file1,{
    
    if (file$extract){
      submissionRV$Read = T
      submissionRV$PS1 = paste0(file$addressFolder,"/", input$submission_pixelSet_file1)
    } else if (!is.null(input$submission_pixelSet_file1) && input$submission_pixelSet_file1$datapath != ""){
      submissionRV$Read = T
      submissionRV$PS1 = input$submission_pixelSet_file1$datapath
    } else {
      submissionRV$Read = F
    }
  })
  
  output$contents_PS <-  renderDataTable({
    
    req(input$submission_pixelSet_file1)
    
    if(submissionRV$Read){
      df <- read.csv2(submissionRV$PS1,
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

