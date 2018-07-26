library(shiny)

shinyUI(fluidPage( 
    helpText(" "),
    tags$head(tags$style(HTML("
                              .selectize-input, .selectize-dropdown {
                              font-size: 90%;
                              }
                              "))),
    fluidRow(        
        column(3,
               
               selectInput("country", label="Country", "United States"), 
               
               sliderInput("dates", label="Years",
                           min = 1960, max = 2017, 
                           value = c(1975, 2017), sep = "")
        ),
   
        column(9,
               plotOutput("plot",
                          click = "plot_click")
        )
    ),
    
    # fluidRow(
    #     verbatimTextOutput("click_info")
    # ),
    
    fluidRow(
        DT::dataTableOutput("table")
    )
))
