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

ui <- dashboardPage( skin= "red",
  
  dashboardHeader(title = "Pixel"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Submissions", tabName = "Submissions", icon = icon("plus-circle")),
      menuItem("Pixel sets", tabName = "Pixel_sets", icon = icon("search")), 
      menuItem("Explorer", tabName = "Explorer", icon = icon("signal"))
    )
    
  ),
  
  dashboardBody(
    useShinyjs(), useShinyalert(),
    # options(shiny.fullstacktrace = TRUE),
    # extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); }"),
    
    #---------------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------------
    
    # Add nav icon
    tags$head(tags$link(href = "Images/pixel_icon.png",
                        rel ="icon", type="image/png")),
    
    # Add css style
    tags$head(HTML('<link rel="stylesheet" type="text/css"
                               href="style.css" />')),
    
    # Add JS script
    # tags$head(tags$script(src="script.js")),
    
    #---------------------------------------------------------------------------
    # Body
    #---------------------------------------------------------------------------
    
    tabItems(
      #.........................................................................
      # Dashboard
      #.........................................................................
      tabItem(tabName = "dashboard"
              
      ),
      
      #.........................................................................
      # Submissions
      #.........................................................................
      tabItem(tabName = "Submissions"
              
      ),
      
      #.........................................................................
      # Pixel set 
      #.........................................................................
      tabItem(tabName = "Pixel_sets"
              
      ),
      
      #.........................................................................
      # Explorer
      #.........................................................................
      
      tabItem(tabName = "Explorer"
              
              
      )
    )
    
  )
)

################################################################################
# SERVER
################################################################################

server <- function(input, output, session) {

}

################################################################################
# APP
################################################################################

shinyApp(ui, server)

