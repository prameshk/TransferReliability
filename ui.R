##Copy 12/21/2016 2:07 PM
# This script is for direction # This is updated on 08/13/2016

#You need to load the shiny library to run the app
library(shiny)
library(leaflet)

# this is for converting data frame into data table, idk but it works
require(data.table)
transferData <- read.csv("testData.csv")

#here you write your User Interface for your app. Note that some of the fields are decided by uioutput
ui <- fluidPage(
  # Setup the page title
  pageWithSidebar(
    #Name of the header panel
    headerPanel('Public Transit Transfer Reliability Analysis Tool', windowTitle = "Transfer Reliability"),
    # These options (dropdown menus) will come in the sidebar.
    sidebarPanel(
      selectInput('date', 'Available dates', choices= c("2017-02-06")), 
      radioButtons('ptime','Select time', c('All day', 'AM peak', 'AM non-peak', 'PM peak', 'PM non-peak')),
      radioButtons('method','Select analysis method', c('System-level', 'Route-level')),
      
      conditionalPanel(
        condition = "input.method == 'System-level'",
        radioButtons('routes','Select type', c('All routes', 'Two routes only')), 
        conditionalPanel(
          condition = "input.routes == 'Two routes only'",
          selectInput('routeFrom', 'Select route from', choices= unique(transferData$routeFrom), selected = 19),
          uiOutput("fromDir"),
          selectInput('routeTo', 'Select route to', choices= unique(transferData$route_short_id), selected = 5),
          uiOutput("toDir"),
          uiOutput("hmap")
        )
        
        
        
        ),
      conditionalPanel(
        condition = "input.method == 'Route-level'",
        selectInput('route', 'Select route #', choices= unique(transferData$route_short_id)),
        uiOutput("dir"), 
        uiOutput("heats")
        )
      
      
      
      #selectInput('stopFrom', 'Select the Transfer Stop From', choices= unique(transferData$stopFromName)),
      #selectInput('route', 'Select the Transfer Stop To', choices= unique(transferData$stop_name)),
      # This function is reactive based on selection of database or route choice by the user.
      
      ),
    mainPanel(

          splitLayout(
            #This is important for plotting frame
            plotOutput('piePlot'),
            plotOutput('distPlot1'),
            plotOutput('distPlot2')
          ),
          
          
          splitLayout(
            #This is important for plotting frame
            plotOutput('MAP1'),
            plotOutput('MAP2')
          ),
          
         
          tableOutput("table")

        
    
      
      
      
      
      
    )
  )
)
