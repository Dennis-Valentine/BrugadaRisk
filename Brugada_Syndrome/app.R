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

# global scope
results <- c(PARS = 0, ST1_BS_ECG = 0, ER = 0, T1BP = 0)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("flatly"),

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
                                              class="btn btn-success",
                                              )),
               column(width = 3, actionButton(inputId = "PARS_no", 
                                              label = "No (+0)"))  
           ), # End of variabel/fluidRow
           
           # Start of variable 2: Spontaneous Type 1 Brugada ECG pattern
           fluidRow(
               column(width = 3, 
                      h6("Spontaneous Type 1 Brugada ECG pattern")),
               column(width = 3, actionButton(inputId = "ST1_BS_ECG_yes", 
                                   label = "Yes (+14)", 
                                   class="btn btn-success")),
               column(width = 3, actionButton(inputId = "ST1_BS_ECG_no", 
                                              label = "No (+0)"))  
             ), # End of variabel/fluidRow
           
           # Start of variable 3: Early repolarization in peripheral leads
           fluidRow(
               column(width = 3, 
                      h6("Early repolarization in peripheral leads")),
               column(width = 3, actionButton(inputId = "ER_yes", 
                                              label = "Yes (+9)", 
                                              class="btn btn-success")),
               column(width = 3, actionButton(inputId = "ER_yes", 
                                              label = "No (+0)"))  
           ), # End of variabel/fluidRow
           
           # Start of variable 3: Type 1 Brugada pattern in peripheral leads
           fluidRow(
               column(width = 3, 
                      h6("Type 1 Brugada pattern in peripheral leads")),
               column(width = 3, actionButton(inputId = "T1BP_yes", 
                                              label = "Yes (+9)", 
                                              class="btn btn-success")),
               column(width = 3, actionButton(inputId = "T1BP_no", 
                                              label = "No (+0)"))  
           ), # End of variabel/fluidRow
           
           #Start of sum of variables 
           fluidRow(
               column(width = 9,
                      textOutput(outputId = "sum")
                      )
               )
           ) # End of 
    ) # End of mainPanel
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # values <- reactiveValues(PARS = 0, ST1_BS_ECG = 0, ER = 0, T1BP = 0)
    # 
    # observeEvent(eventExpr = input$PARS_yes, once = TRUE,
    #              handlerExpr = {values$PARS <- reactiveValues(PARS = 12)} )
    
    # observeEvent(eventExpr = input$PARS_yes, 
    #              handlerExpr = {values <- reactiveValues(PARS_yes = 12)} )
    
    # values <- reactiveValues()
    # values$PARS_yes <- 12
    # values$PARS_no <- 0


    
    output$sum <- renderText(
        
        expr = {
            
            results <- c(PARS = 0, ST1_BS_ECG = 0, ER = 0, T1BP = 0)
            if(input$PARS_yes >= 1){
                results[1] <- 12    
            } 
            
            if(input$ST1_BS_ECG_yes >= 1){
                results[2] <- 14
            }
            
            if(input$ER_yes >= 1){
                results[3] <- 9
            }
            
            if(input$T1BP_yes >= 1){
                results[4] <- 9
            }
            
            sum(results)
            
            #sum( (input$PARS_yes * 12 ), (input$ST1_BS_ECG_yes * 14))
            
            }
        ) # end of renderText
}

# Run the application 
shinyApp(ui = ui, server = server)
