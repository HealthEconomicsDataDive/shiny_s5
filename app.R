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


# Define UI for application that draws a histogram
ui <- shinyUI({dashboardPage(
    
    
    
    dashboardHeader(),

    
## --------------- SIDE BAR --------------
    dashboardSidebar(
        # logo
        span(HTML('<img src="logo_small.png" style="height:200px; margin-top:-0.5em; margin-left:2em">')),
        
        sliderInput(inputId = "PSA_len_slider","How many PSA?",1,2000,value = 100),
        
        numericInput(inputId = "start_age_input","Start age",min = 0,max=90,value = 16),
        
        numericInput(inputId = "cycle_len_input","Horizon",min = 1,max=86,value = 86),
        
        numericInput(inputId = "iv_day_costs_input","IC day costs",min = 0,max=NA,value = 360.1),
        
        numericInput(inputId = "int_costs_yearly_input","Annual intervention costs",min = 0,max=NA,value = 5500),
        
        actionButton("action_GO","Run Model")
    ),

## --------------- BODY --------------
    dashboardBody(
        
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
                 column(6,h4("Results Table: Central Estimates"),
                        tableOutput("result_table")),
                 column(2)),#actionButton("trump_GO","Trump_me"),
                       # HTML('<img src="trump_up.png" style="height:30px; margin-top:-0.5em;">'))),
        
        br(),
        
        fluidRow(column(6, 
                        h4("Cost-effectiveness plane"),
                        plotOutput("CE_plane")),
                 column(6,
                        h4("Cost-effectiveness acceptability curve"),
                        plotOutput("CEAC_plot"))
                 ),
        
        br(),
        

#        textOutput("show_runtime"),

        
        
        br(),
        
#        h4(textOutput("show_runtime")),

        
        useShinyjs()
        ),

title = "Sample Shiny",
skin = "blue"
)} )

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
                                    set_DR_COSTS = 0.035,set_DR_QALY = 0.035,
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
