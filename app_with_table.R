rm(list=ls())

# ShinyApp
library(shiny)
library(DT)

server <- function(input, output) {

# display 10 rows initially
output$ex1 <- DT::renderDataTable(
  DT::datatable(iris, options = list(pageLength = 25))
)

#==== 
# 1. USER INPUTS
#====

previous <- reactive({iris})


MyChanges <- reactive({
  if(is.null(input$hotable1)){return(previous())}
  else if(!identical(previous(),input$hotable1)){
    # hot.to.df function will convert your updated table into the dataframe
    df_temp = as.data.frame(shinysky::hot.to.df(input$hotable1))
    df_temp = cbind(df_temp,iris)
    names(df_temp) = names(iris)
    df_temp
  }
})


output$hotable1 <- shinysky::renderHotable({
  MyChanges()[,1:4]
}, readOnly = F)


#====
# 2. OUTPUTS
#===

# -1 means no pagination; the 2nd element contains menu labels
output$ex2 <- DT::renderDataTable(
  DT::datatable(
    iris, options = list(
      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
      pageLength = 15
    )
  )
)

# you can also use paging = FALSE to disable pagination
output$ex3 <- DT::renderDataTable(
  DT::datatable(iris, options = list(paging = FALSE))
)

output$iris1 = renderText({
  # input from country pickselector
  paste("ICER = ",MyChanges()[1,1])
})
# output$iris1 <- paste(iris[1,1])
} # close server



ui <- navbarPage(
  title = 'Shiny HTA Example',
  
  # TAB 1 - BACKGROUND
  tabPanel('Background',    

  # insert text to be added to the background tab.       
           
           "Background Information"),
  # TAB 2 - INPUTS
  tabPanel('Inputs', 
           
  # hottable of input parameters.
           shinysky::hotable("hotable1")),
  
  # TAB 3 - OUTPUTS
  tabPanel('Outputs',
           
  # output tables/ results.
           textOutput(outputId = "iris1")),
  
  # TAB 4 - FURTHER INFORMATION
  
  tabPanel('Further Information', "Created by RS, PS ... on 22nd January 2020 as part of the Belfast R Workshop")
) # close UI


# -------- RUN APP ---------- 
# Run the application 
shinyApp(ui = ui, server = server)
