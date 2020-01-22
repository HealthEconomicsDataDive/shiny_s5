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
# install.packages("shinysky")

source("./support_functions.R")
source("./Markov_PSA.R")


# Define UI for application that draws a histogram
ui <- shinyUI({dashboardPage(
    
    dashboardHeader(),
    
## --------------- SIDE BAR --------------
    dashboardSidebar(
        sliderInput(inputId = "PSA_len_slider","How many PSA?",1,2000,value = 100),
        
        numericInput(inputId = "start_age_input","Start age",min = 0,max=90,value = 16),
        
        numericInput(inputId = "cycle_len_input","Horizon",min = 1,max=86,value = 86),
        
        numericInput(inputId = "iv_day_costs_input","IC day costs",min = 0,max=NA,value = 360.1),
        
        numericInput(inputId = "int_costs_yearly_input","Annual intervention costs",min = 0,max=NA,value = 5500),
        
        actionButton("action_GO","Run Model")
    ),

## --------------- BODY --------------
    dashboardBody(
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
        fluidRow(column(6, 
                        h4("Cost-effectiveness plance"),
                        plotOutput("CE_plane")),
                 column(6,
                        h4("Cost-effectiveness acceptibility curve"),
                        plotOutput("CEAC_plot"))
                 ),
        
        br(),
        
        
        
        br(),
        
        textOutput("show_runtime"),
        
        useShinyjs()
        ),

title = "Sample Shiny",
skin = "blue"
)} )

## ------------------ SERVER ---------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
    
        observeEvent(input$action_GO, ignoreNULL=F, {
            
            
            
            
            # run model with updated input
            res = markov_s5_wrapper(set_PSA = input$PSA_len_slider,
                                    set_start_age = input$start_age_input,
                                    set_cycle_length = input$cycle_len_input,
                                    set_int_costs_yearly = input$int_costs_yearly_input,
                                    set_DR_COSTS = 0.035,set_DR_QALY = 0.035,
                                    set_iv_day_costs = input$iv_day_costs_input
                                        
                                        )
            
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
            
            
            
            
        })
        
        
    }
    




## ------------------    APP LAUNCH -----------------
    # Run the application 
    shinyApp(ui = ui, server = server)
