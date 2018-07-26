library(shiny)

shinyUI(fluidPage( 
    helpText(" "),

    fluidRow(        
        column(3,
               
               uiOutput("countryControl"),
               
               uiOutput("yearControl"),
               
               helpText("Click on any point in a series to display only that series in the table below."),
               
               helpText("Please note that the table's source links may download a spreadsheet or PDF file.")
        ),
   
        column(9,
               plotOutput("plot",
                          click = "plot_click")
        )
    ),
    
    fluidRow(
        DT::dataTableOutput("table")
    )
))
