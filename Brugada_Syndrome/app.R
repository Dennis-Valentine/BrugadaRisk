
# Background --------------------------------------------------------------

# This is the shiny app for the Brugada Syndrome Risk Stratification. 


# Housekeeping ------------------------------------------------------------

library(shiny)
library(shinythemes)
library(shinyalert)


# Global Scope ------------------------------------------------------------

# Size of the button
width <- "100px" 
# Button style 
style <- "color: #fff; background-color: #4EAA7F; border-color: #284E42" 



# Define UI for the calculator
ui <- fluidPage(
    
    theme = shinythemes::shinytheme("flatly"),
    
    shinyalert::useShinyalert(),

    # Application title
    titlePanel("Brugada Syndrome Risk Stratification"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("Background"),
            p("Risk stratification for sudden cardiac death (SCD) in Brugada syndrome (BrS) is a significant challenge."), 
            h5("Purpose"),
            p("To evaluate the importance of clinical and ECG factors in the likelihood of developing significant ventricular arrhythmias (VAs)/SCD in BrS patients."),
            h5("Methods"),
p("VA occurrence during follow-up were assessed and the role of 16 proposed clinical or ECG risk markers evaluated in a multicenter international study of BrS patients and no history of cardiac arrest. Markers with predictive power were identified and incorporated into a risk score model."),
h5("Results")

#Across 15 international centers, 1084 patients were included. During a follow-up of 5.3 years (IQR 2.7–9.0 years)- 110 patients had VA occurrence (10.1%) with an annual event rate of 1.7% (95% CI 1.4–2.0). Of the 16 proposed risk factors, diagnosis by family screening of sudden cardiac death (HR 4.65; p<0.001), probable arrhythmia related syncope (HR 3.88, p<0.001), type 1 spontaneous ECG (HR 3.56; p<0.001), Early Repolarisation (HR 3.15; p<0.001) and type 1 Brugada pattern in peripheral leads (HR 2.42; p<0.001) were associated with a higher VA occurrence risk during follow-up. These 5 variables were incorporated into a risk score model whereby each variable was allocated a point score based on the variable's predictive strength. The total points obtained from the model for a patient could then be translated into the predicted VA occurrence risk during follow-up (Figure 1). The model showed a sensitivity of 63.5% (95% CI 50.0–76.9) and specificity of 84.2% (95% CI 81.1–87.1) in predicting VA occurrence at 5-years follow-up. The model showed a greater discriminative power compared to an existing model (AUC 0.83 vs. 0.71; p<0.001)."),
            
            

        ),

        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot"),
           
           # Start of variable 1: Probable arrhythmia related syncope
           fluidRow(
               column(width = 3, 
                      h6("Probable arrhythmia related syncope")),
               column(width = 3, actionButton(inputId = "PARS_yes", 
                                              label = "Yes (+12)", 
                                              icon = icon(" "),
                                              width = width,
                                              style = style
                                              #class="btn btn-success",
                                              )),
               column(width = 3, actionButton(inputId = "PARS_no", 
                                              label = "No (+0)",
                                              width = width,
                                              style = style
                                              ))  
           ), # End of variabel/fluidRow
           
           # Start of variable 2: Spontaneous Type 1 Brugada ECG pattern
           fluidRow(
               column(width = 3, 
                      h6("Spontaneous Type 1 Brugada ECG pattern")),
               column(width = 3, actionButton(inputId = "ST1_BS_ECG_yes", 
                                   label = "Yes (+14)",
                                   width = width,
                                   style = style
                                   #class="btn btn-success"
                                   )),
               column(width = 3, actionButton(inputId = "ST1_BS_ECG_no", 
                                              label = "No (+0)",
                                              width = width,
                                              style = style))  
             ), # End of variabel/fluidRow
           
           # Start of variable 3: Early repolarization in peripheral leads
           fluidRow(
               column(width = 3, 
                      h6("Early repolarization in peripheral leads")),
               column(width = 3, actionButton(inputId = "ER_yes", 
                                              label = "Yes (+9)", 
                                              width = width,
                                              style = style
                                              #class="btn btn-success"
                                              )),
               column(width = 3, actionButton(inputId = "ER_no", 
                                              label = "No (+0)",
                                              width = width,
                                              style = style))  
           ), # End of variabel/fluidRow
           
           # Start of variable 3: Type 1 Brugada pattern in peripheral leads
           fluidRow(
               column(width = 3, 
                      h6("Type 1 Brugada pattern in peripheral leads")),
               column(width = 3, actionButton(inputId = "T1BP_yes", 
                                              label = "Yes (+9)", 
                                              width = width,
                                              style = style
                                              #class="btn btn-success"
                                              )),
               column(width = 3, actionButton(inputId = "T1BP_no", 
                                              label = "No (+0)",
                                              width = width,
                                              style = style))  
           ), # End of variabel/fluidRow
           
           #Start of sum of variables 
           fluidRow(
               column(width = 9,
                      #plotOutput(outputId = "plot"),
                      textOutput(outputId = "sum")
                      )
               )
           ) # End of 
    ) # End of mainPanel
    )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    shinyalert(
        title = "Hello",
        text = "Brugada Syndrome Risk Stratification",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
    )
    
    react_pars <- reactiveValues()
    
    inuse <- function(x){
        updateActionButton(session = session, inputId = x, icon = icon("check")) 
                           }
    
    unuse <- function(x){
        updateActionButton(session = session, inputId = x, icon = icon("times"))   
    }
    
    observeEvent(input$PARS_yes, {react_pars$PARS <- 12; 
    inuse("PARS_yes"); unuse("PARS_no") })
    observeEvent(input$PARS_no, {react_pars$PARS <- 0;
    inuse("PARS_no"); unuse("PARS_yes") })
    
    observeEvent(input$ST1_BS_ECG_yes, {react_pars$ST1 <- 14;
    inuse("ST1_BS_ECG_yes"); unuse("ST1_BS_ECG_no") })
    observeEvent(input$ST1_BS_ECG_no, {react_pars$ST1 <- 0;
    inuse("ST1_BS_ECG_no"); unuse("ST1_BS_ECG_yes")})
    
    observeEvent(input$ER_yes, {react_pars$ER <- 9;
    inuse("ER_yes"); unuse("ER_no")})
    observeEvent(input$ER_no, {react_pars$ER <- 0;
    inuse("ER_no"); unuse("ER_yes")})
    
    observeEvent(input$T1BP_yes, {react_pars$T1BP <- 9;
    inuse("T1BP_yes"); unuse("T1BP_no")})
    observeEvent(input$T1BP_no, {react_pars$T1BP <- 0;
    inuse("T1BP_no"); unuse("T1BP_yes")})
    
    output$sum <- renderText(
        
        expr = { sum(react_pars[['PARS']],
                     react_pars[['ST1']], 
                     react_pars[['ER']],
                     react_pars[['T1BP']]) }
        ) # end of renderText
}

# Run the application 
shinyApp(ui = ui, server = server)
