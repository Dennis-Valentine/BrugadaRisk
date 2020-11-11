#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Brugada Syndrome Risk Stratification"),
  mainPanel(
      # Start of variable 1: Probable arrhythmia related syncope
      fluidRow(
        column(width = 3, 
               h6("Probable arrhythmia related syncope")),
        column(width = 3, actionButton(inputId = "PARS_yes", 
                                       label = "Yes (+12)", 
                                       class="btn btn-success",
        )),
        column(width = 3, actionButton(inputId = "PARS_no", 
                                       label = "No (+0)"))),
        
        #Start of sum of variables 
        fluidRow(
          column(width = 9, textOutput(outputId = "sum"))
        )
      ) # End of mainPanel
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #react_pars <- reactiveValues(PARS = 0, ST1_BS_ECG = 0, ER = 0, T1BP = 0)
  
  #react_pars <- NULL
  #print(react_pars)
  react_pars <- reactiveValues()
  
  observeEvent(input$PARS_yes, {react_pars$PARS <- 12 })
  observeEvent(input$PARS_no, {react_pars$PARS <- 0 })
  
  output$sum <- renderText(
    
    expr = { 
      
      #print(isolate(react_pars$PARS))
      print( react_pars[['PARS']])  
      }
  ) # end of renderText
}

# Run the application 
shinyApp(ui = ui, server = server)
