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
library(kableExtra)

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
           
           h3("An Early Health Economic Analysis of the Potential Cost Effectiveness of an Adherence Intervention to Improve Outcomes for Patients with Cystic Fibrosis"),
           
           br(),br(),br(),
           
           HTML("This is an example R-Shiny model built around a markov model originally created by Paul Tappenden, Susannah Sadler & Martin Wildman published <br> in PharmacoEcononomics in 2017."),
           
           helpText(a("Tappenden et al. (2017)",     href="https://doi.org/10.1007/s40273-017-0500-x")),
           
           br(),
           
           HTML("It was originally created using Excel, but recoded in R by Paul Schneider in 2019, we have now created this app as a Shiny 'Front End'"),
           
           br(),br(),
           
           HTML("In the model individuals can be in one of five health states, three of which relate to lung function, one post transplant and dead"),
           
           br(),br(),
           
           span(HTML('<img src="Diagram.PNG" style="height:300px; margin-top:-0.5em;">')),  # model structure
           
           br(),br(),
           
           HTML("As the user, you can change the model parameters in 'Inputs', and run the model in 'PSA Results'")
           
           

  ), # close tab
           

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
             
             column(6,fluidRow(h4("Probabilities"),align="center",
                               
                               sliderInput(inputId = "transplant_rate_mean","Set Transplant Rate Mean",min = 0,max = 0.05,value = 0.003)
                               
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
  
  #tabPanel('Deterministic Results',
  #         #actionButton("action_GO","Run Deterministic Model",align="center"),
  #         
  #         fluidRow(column(6, align="center",
  #                         h4("Markov Trace Baseline")#, # Markov Trace
  #                         ),#plotOutput("CEAC_plot")),
  #                  column(6,align="center",
  #                         h4("Markov Trace Intervention")#, # Markov Trace
  #                         )#plotOutput("CEAC_plot"))
  #                  
  #                  )# end row
  #         
  #         ), # end tab
  
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
           
           fluidRow(column(2),
                    column(4, align='center', 
                           actionButton("action_GO","Run PSA",align="center")),
                    column(4, align='center',
                           textOutput("show_runtime")
                           )),
                    
           br(),
                    
           
           fluidRow(column(2),
                    column(8, align="center",
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
           
           
          #fluidRow(column(2),
          #         column(8,align="center",
          #                textOutput("show_runtime")),
          #         column(2)),
                                    
           
          # br(),
           
          #         h4(textOutput("show_runtime")),
           
           
           useShinyjs()
           ), # end tab
  
  # TAB 4 - FURTHER INFORMATION
  
  tabPanel('Further Information',  # who we are
           
           HTML("This RShiny was created by <strong>Robert Smith & Paul Schneider</strong> to demonstrate the feasibility of using R-Shiny in Health Technology Assessment, and as a teaching tool."),
           
           br(),br(),br(), 
           
           HTML("Both Robert and Paul and part of the Wellcome Trust Doctoral Training Centre in Public Health Economics and Decision Science at the University of Sheffield, they have experience in working with stakeholders to improve the transparency of economic models "),
           
           br(),br(),br(),  
           
           HTML("For more information contact:<br><br><strong>Robert Smith</strong><br>University of Sheffield<br>rasmith3@sheffield.ac.uk<br>https://github.com/RobertASmith<br><br>or <br><br><strong>Paul Schneider</strong><br>University of Sheffield<br>p.schneider@sheffield.ac.uk<br>https:bitowaqr.github.io<br>"),
           
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
            
            psa_number <- input$PSA_len_slider
            
            # run model with updated input
            res = markov_s5_wrapper(
                                    set_PSA = input$PSA_len_slider,
                                    set_start_age = input$start_age_input,
                                    set_cycle_length = input$cycle_len_input,
                                    set_int_costs_yearly = input$int_costs_yearly_input,
                                    set_DR_COSTS = input$dr_costs, #0.035,
                                    set_DR_QALY = input$dr_qalys, # 0.035,
                                    set_iv_day_costs = input$iv_day_costs_input,
                                    set_transplant_rate_mean = input$transplant_rate_mean
                                    )
            
            output$result_table <- renderTable({
              
              icer <- res$res_table[6] / res$res_table[3]
              
              if(icer < 0 & res$res_table[6] > 0 ){
                icer <- "Dominated"
              }
              
              if(icer < 0 & res$res_table[3] > 0 ){
                icer <- "Dominating"
              }
              
                         data.frame(Option = c("Adherance Intervention","Current Care"),
                                    QALYs = c(prettyNum(round(res$res_table[2],2), big.mark=","),prettyNum(round(res$res_table[1],2), big.mark=",")),
                                    Costs = c(prettyNum(round(res$res_table[4],0), big.mark=","),prettyNum(round(res$res_table[5],0), big.mark=",")),
                                    Inc.QALYs = c(prettyNum(round(res$res_table[3],2), big.mark=","),NA),
                                    Inc.Costs = c(prettyNum(round(res$res_table[6],0), big.mark=","),NA),
                                    ICER = c(prettyNum(round(icer,0), big.mark=","),NA)) # close data frame
                          
              }) # close render table
            
            
            output$CE_plane <- renderPlot({
                res$ce_plane
            })
            
            output$CEAC_plot <- renderPlot({
                res$ceac_plot
            })
            
            output$show_runtime <- renderText({
                runtime    <- res$runtime
                
                paste("PSA runtime with",psa_number,"iterations:",runtime)
            })
            
            hide(id = "loading-content", anim = TRUE, animType = "fade",time = 1)    
            
            show("app-content")
            
            
        }) # close observe event
        
        
    } # close server
    




## ------------------    APP LAUNCH -----------------
    # Run the application 
    shinyApp(ui = ui, server = server)
