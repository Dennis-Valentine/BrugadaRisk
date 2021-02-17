
# Background --------------------------------------------------------------

# This is the shiny app for the Brugada Syndrome Risk Stratification. 


# Housekeeping ------------------------------------------------------------

library(shiny)
library(shinythemes)
library(shinyalert)
library(ggplot2)


# Global Scope ------------------------------------------------------------

# Size of the button
width <- "100px" 
# Button style 
style <- "color: #fff; background-color: #4EAA7F; border-color: #284E42" 

# User risk variable
user_risk <- 0

# Data used to generate the line graph
# RF_tab <- read.csv(file = "rf_perm_table.csv")
# RF_tab$AVE <- as.numeric(gsub(pattern = "%", replacement = "", x = RF_tab$AVE))


# TODO: the buttons should be  Radio Group Buttons (http://shinyapps.dreamrs.fr/shinyWidgets/) 

# Define UI for the calculator
ui <- fluidPage(
    
    theme = shinythemes::shinytheme("flatly"),
    
    shinyalert::useShinyalert(),

    # Application title
    titlePanel("Brugada Syndrome Risk Stratification"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("BRUGADA-RISK estimates the risk of ventricular arrhythmias (VA) or sudden cardiac death (SCD) at 5 years in patients with Brugada syndrome."),
            p("BRUGADA-RISK has a sensitivity of 71.2% and a specificity of 80.2% for the prediction of VA/SCD at 5 years."),
            p("Risk for an individual patient can be calculated from the following equation:"), 
            code(em("Probable arrhythmia related syncope +\n
                 Spontaneous Type 1 Brugada ECG pattern + \n
                 Early repolarization in peripheral leads +
                 Type 1 Brugada pattern in peripheral leads")), 
            
            br(),
            br(),
            
            p("Reference: S Honarbakhsh, R Providencia, J Garcia-Hernandez, et al … , PD Lambiase. A Primary Prevention Clinical Risk Score Model for Patients With Brugada Syndrome (BRUGADA-RISK). JACC Clin Electrophysiol 2020 Oct 28;[EPub Ahead of Print]"),
            downloadLink(outputId = "BR_paper", label = "Download"),
            #a("Click here to get the PDF", href="www/BR_paper.pdf")
            #a("paper", href='www/BR_paper.pdf')
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
                                              label = "Yes (+12)", 
                                              width = width,
                                              style = style
                                              #class="btn btn-success"
                                              )),
               column(width = 3, actionButton(inputId = "ER_no", 
                                              label = "No (+0)",
                                              width = width,
                                              style = style))  
           ), # End of variabel/fluidRow
           
           # Start of variable 4: Type 1 Brugada pattern in peripheral leads
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
               column(width = 6,
                      plotOutput(outputId = "plot_circle"),
                      ),
               column(width = 6, 
                      plotOutput(outputId = "plot_linegraph"))
               )
           ) # End of 
    ) # End of mainPanel
    )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    shinyalert(
        title = "Hello",
        text = "Brugada Syndrome Risk Stratification. Predict ventricular arrhythmias / SCD at 5 years",
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
    
    # Download PDF
    output$BR_paper <- downloadHandler(
        filename = "BR_paper.pdf",
        content = function(file) {
            file.copy("www/BR_paper.pdf", file)}
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
    
    observeEvent(input$ER_yes, {react_pars$ER <- 12;
    inuse("ER_yes"); unuse("ER_no")})
    observeEvent(input$ER_no, {react_pars$ER <- 0;
    inuse("ER_no"); unuse("ER_yes")})
    
    observeEvent(input$T1BP_yes, {react_pars$T1BP <- 9;
    inuse("T1BP_yes"); unuse("T1BP_no")})
    observeEvent(input$T1BP_no, {react_pars$T1BP <- 0;
    inuse("T1BP_no"); unuse("T1BP_yes")})
    
    output$sum <- renderText(
        
        expr = { user_risk <- sum(react_pars[['PARS']],
                     react_pars[['ST1']], 
                     react_pars[['ER']],
                     react_pars[['T1BP']]) }
        ) # end of renderText
    
    output$plot_circle <- renderPlot(expr = {
        
        user_risk <- sum(react_pars[['PARS']],
                         react_pars[['ST1']], 
                         react_pars[['ER']],
                         react_pars[['T1BP']])
        
        data <- data.frame(
            category = c("user_risk", "max_risk"),
            count = c(user_risk, 47))
        
        # Compute percentages
        data$fraction = data$count / 47
        
        # Compute the cumulative percentages (top of each rectangle)
        data$ymax = data$fraction
        
        # Compute the bottom of each rectangle
        data$ymin = c(0, head(data$ymax, n=-1))
        
        # Make the plot
        base_colour <- "#DBEAE0"
        #fill_col <- "#C69A60"
        fill_col_range <- seq(from = 1, to = 5, by = 1)
        names(fill_col_range) <- c("#4C8C4C", "#82C57B", "#CBCC85", "#DD9854", "#B22B3B")
        fill_col_index <- round(quantile(fill_col_range, user_risk/47))
        fill_col <- fill_col_range[fill_col_index]
        
        # Making the label that fits inside the circle 
        risk_at_5 <- "tmp"
        # risk_at_5 <- subset(RF_tab, SC == user_risk)$AVE
        # risk_at_5 <- paste0(unique(risk_at_5), "%")

        label_user_score <- c(paste("User score:", user_risk))
        label_predicted_event <- c(paste("Predicted event over \n 5 years:", risk_at_5))
        label_display <- paste(label_user_score, "\n", "\n", label_predicted_event)

        ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
            geom_rect() +
            coord_polar(theta = "y") + # Try to remove that to understand how the chart is built initially
            annotate("text", label = label_display, size = 7, x = 0, y = 0) +
            scale_fill_manual(values = c(base_colour, names(fill_col))) +
            theme_void() +
            theme(legend.position = "none")
        }
    )
    
    
    output$plot_linegraph <- renderPlot(expr = {
        
        # user_risk <- sum(react_pars[['PARS']],
        #                  react_pars[['ST1']], 
        #                  react_pars[['ER']],
        #                  react_pars[['T1BP']])
        # 
        # label_point_df <- subset(RF_tab, SC == user_risk)
        # 
        # ggplot(data = RF_tab, aes(x = SC, y = AVE)) +
        #     geom_point(colour = ifelse(RF_tab$SC == user_risk, "red", "grey50")) +
        #     geom_line() +
        #     ggrepel::geom_text_repel(data = label_point_df[1,], inherit.aes = FALSE,
        #                              direction = "y",
        #                              min.segment.length = 0, seed = 45, box.padding = 1,
        #                              aes(x = SC, y = AVE, 
        #                                  label = paste("User Score:", user_risk, "\n", "Estimated Risk:",
        #                                                label_point_df[1,"AVE"], "%"))) +
        #     xlab(label = "Total Risk Points") +
        #     ylab(label = "Five Year Predicted Risk of VA/SCD") + 
        #     ggtitle(label = "Risk Score Calculation and Predicted Event Rates Over 5 Years") +
        #     scale_x_continuous(breaks = seq(from = 0, to = 47, by = 5)) +
        #     scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(0, 100)) +
        #     theme_minimal()
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
