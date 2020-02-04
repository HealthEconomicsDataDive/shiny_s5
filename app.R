#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(shinyjs)
library(MCMCpack)
library(ggplot2)
library(DT)
library(shinycssloaders)

# loading screen   
appCSS <- "
                  #loading-content {
                    position: absolute;
                    background: #000000;
                    padding-top: 250px;
                    opacity: 0.9;
                    z-index: 100;
                    font-size:30px;
                    left: 100px;
                    right: 0;
                    top: 0;
                    height: 100%;
                    text-align: center;
                    color: cyan;
                  }
                  "

# install.packages("shinysky")

source("./support_functions.R")
source("./Markov_PSA.R")

pic <- png::readPNG( "./www/5S_markov.PNG")

# Define UI for application that draws a histogram
ui <- navbarPage(
  
  title = 'R-Shiny Markov Model Example',
  
  # TAB 1 - BACKGROUND
  tabPanel('Background',
           
           # picture showing model structure
           span(HTML('<img src="5S_markov.PNG" style="height:1000px; margin-top:-0.5em;">'))
           ), # close tab
           
           # insert text to be added to the background tab.       
           #"Background Information"),
  
  # TAB 2 - INPUTS
  tabPanel('Inputs', 
           fluidRow(
             column(6,fluidRow(h4("Model Structure"),align="center",
                               
                               br(),
                               
                               HTML("Use the sliders to choose the structural inputs"),
                               
                               br(),
                               
                               sliderInput(inputId = "PSA_len_slider","How many PSA?",min = 1,max = 2000,value = 100),
                               
                               sliderInput(inputId = "start_age_input","Start age",min = 0,max=90,value = 16),
                               
                               sliderInput(inputId = "cycle_len_input","Horizon",min = 1,max=86,value = 86),
                               
                               sliderInput(inputId = "dr_qalys","Discount Rate QALYs",min = 0,max=0.1,value = 0.035),
                               
                               sliderInput(inputId = "dr_costs","Discount Rate Costs",min = 0,max=0.1,value = 0.035)
                               
                               
                               )),
             
             column(6,fluidRow(h4("Probabilities"),align="center"
                               
                               )) 
             ), # end row
           
           br(),br(),br(),
           
           fluidRow(column(6,fluidRow(h4("Costs"),align="center",
                                      
                                      br(),
                                      
                                      HTML("Enter the costs in the boxes below"),
                                      
                                      br(),
                                     
                                      numericInput(inputId = "iv_day_costs_input","IV day costs",min = 0,max=NA,value = 360.1),
                                      
                                      numericInput(inputId = "int_costs_yearly_input","Annual intervention costs",min = 0,max=NA,value = 5500)
                                      
                                      )),
                    column(6,fluidRow(h4("Utilities"),align="center"
                                      
                                      ))
                    ) # end row
       
           
           
           ),
  
  # TAB 3 - Deterministic Results
  
  tabPanel('Deterministic Results',
           #actionButton("action_GO","Run Deterministic Model",align="center"),
           
           fluidRow(column(6, align="center",
                           h4("Markov Trace Baseline")#, # Markov Trace
                           ),#plotOutput("CEAC_plot")),
                    column(6,align="center",
                           h4("Markov Trace Intervention")#, # Markov Trace
                           )#plotOutput("CEAC_plot"))
                    
                    )# end row
           
           ),
  
  # TAB 3 - PSA Results
  tabPanel('PSA Results',
           
           useShinyjs(),
           
           inlineCSS(appCSS),
           
           # Loading message
           div(
             id = "loading-content",
             HTML("Loading...<br>"),
             HTML("Just one moment please.")
           ),
           
           tags$head(tags$style(HTML('
                                     /* logo */
                                     .skin-blue .main-header .logo {
                                     background-color: #333333;
                                     }
                                     
                                     /* navbar (rest of the header) */
                                     .skin-blue .main-header .navbar {
                                     background-color: #333333;
                                     }
                                     
                                     /* main sidebar */
                                     .skin-blue .main-sidebar {
                                     background-color: #333333;
                                     color:black;
                                     }
                                     
                                     /* body */
                                     .content-wrapper, .right-side {
                                     background-color: white;
                                     }
                                     
                                     '))),
           
           actionButton("action_GO","Run PSA",align="center"),
           
           fluidRow(column(2),
                    column(8,align="center",
                           h4("Results Table: Central Estimates"),
                           tableOutput("result_table")),
                    column(2)),#actionButton("trump_GO","Trump_me"),
           # HTML('<img src="trump_up.png" style="height:30px; margin-top:-0.5em;">'))),
           
           br(),
           
           fluidRow(column(2),
                    column(8,align="center", 
                           h4("Cost-effectiveness plane"),   # cost effeciveness plane from Markov_PSA
                           plotOutput("CE_plane")),
                    column(2)),
           
           br(),
           
           fluidRow(column(2),
                    column(8,align="center",
                           h4("Cost-effectiveness acceptability curve"), # CEAC from Markov_PSA
                           plotOutput("CEAC_plot")),
                    column(2)), # end row
           
           
           #        textOutput("show_runtime"),
           
          # br(),
           
          #         h4(textOutput("show_runtime")),
           
           
           useShinyjs()
           ), # end tab
  
  # TAB 4 - FURTHER INFORMATION
  
  tabPanel('Further Information',  # who we are
           
           HTML("This RShiny was created by <strong>Robert Smith & Paul Schneider</strong> to demonstrate the feasibility of using R-Shiny in Health Technology Assessment."),
           
           br(),br(),br(),
           
           HTML("The underlying model is based on a paper by Paul Tappenden, Susannah Sadler & Martin Wildman Published in PharmacoEcononomics in 2017 entitled:"),
           
           br(),
           
           HTML("An Early Health Economic Analysis of the Potential Cost Effectiveness of an Adherence Intervention to Improve Outcomes for Patients with Cystic Fibrosis"),
           helpText(   a("https://doi.org/10.1007/s40273-017-0500-x",     href="https://doi.org/10.1007/s40273-017-0500-x")),
           
           br(),br(),
           
           HTML("It was originally created using Excel, but recoded in R by Paul Schneider in 2019, we have now created this app as a user 'Front End'"),
           
           br(),br(),
             
           HTML("Both Robert and Paul and part of the  Wellcome Trust Doctoral Training Centre in Public Health Economics and Decision Science at the University of Sheffield"),
           
           br(),br(),br(),  
           
           HTML("For more information contact:<br><br><strong>Robert Smith</strong><br>University of Sheffield<br>rasmith3@sheffield.ac.uk<br>https://github.com/RobertASmith"),
           
           br(),br(),br(),br(),
             
           span(HTML('<img src="logo_small2.PNG" style="height:300px; margin-top:-0.5em;">'))  # dark peak logo
           
           
           
           ) # end tab
) # close UI
    
    
## ------------------ SERVER ---------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    
        observeEvent(input$action_GO, ignoreNULL=F, {
            
            hide("app-content")
            show(id = "loading-content", anim = TRUE, animType = "fade",time = 1)    
            
            
            # run model with updated input
            res = markov_s5_wrapper(set_PSA = input$PSA_len_slider,
                                    set_start_age = input$start_age_input,
                                    set_cycle_length = input$cycle_len_input,
                                    set_int_costs_yearly = input$int_costs_yearly_input,
                                    set_DR_COSTS = input$dr_costs, #0.035,
                                    set_DR_QALY = input$dr_qalys, # 0.035,
                                    set_iv_day_costs = input$iv_day_costs_input
                                        
                                        )
            
            output$result_table <- renderTable({
              data.frame(
                Option = c("Adherance Intervention","Current Care"),
                QALYs = c(res$res_table[2],res$res_table[1]),
                Costs = c(res$res_table[4],res$res_table[5]),
                Inc.QALYs = c(res$res_table[3],NA),
                Inc.Costs = c(res$res_table[6],NA),
                ICER = c(res$res_table[6]/res$res_table[3],NA)
              )
            })
            
            
            output$CE_plane <- renderPlot({
                res$ce_plane
            })
            
            output$CEAC_plot <- renderPlot({
                res$ceac_plot
            })
            
            output$show_runtime <- renderText({
                # res$run_time
                paste("PSA runtime with",input$PSA_len_slider,"iterations:",res$runtime)
            })
            
            hide(id = "loading-content", anim = TRUE, animType = "fade",time = 1)    
            show("app-content")
            
            
        })
        
        
    }
    




## ------------------    APP LAUNCH -----------------
    # Run the application 
    shinyApp(ui = ui, server = server)
