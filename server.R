library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(readr)
library(dplyr)

# We'll use a subset of the mtcars data set, with fewer columns
# so that it prints nicely
swiid_source <- read_csv("swiid_source.csv", col_types = "ciddcclcccc") %>% 
    select(-monetary)

cc <- swiid_source %>% 
    select(country) %>% 
    distinct()

shinyServer(function(input, output, session) {
    
    observe({
        updateSelectInput(session, "country", choices = cc$country, selected = "United States")
    })
    
    plotInput <- reactive({
        # Get data for selected countries and series
        s1 <- swiid_source %>% 
            filter(country == input$country1 & 
                       year >= input$dates[1] &
                       year <= input$dates[2])
        
        output$plot1 <- renderPlot({
            ggplot(s1, aes(year, gini)) +
                geom_point() + 
                geom_line() + 
                theme(legend.position="none")
        })
        
        output$table <- DT::datatable(s1, options = list(orderClasses = TRUE))
        
        output$click_info <- renderPrint({
            # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
            # were a base graphics plot, we'd need those.
            nearPoints(mtcars2, input$plot1_click, addDist = TRUE)
        })
        
    })
    
})