library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(readr)
library(dplyr)

swiid_source <- read_csv("swiid_source.csv", col_types = "ciddcclcccc") %>% 
    select(-monetary) %>% 
    mutate(gini = gini*100)

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
            filter(country == input$country & 
                       year >= input$dates[1] &
                       year <= input$dates[2])
        
        p <- ggplot(s1, aes(year, gini, color = series)) +
            geom_point() + 
            geom_line() + 
            theme(legend.position="none")
    })
    
    output$plot <- renderPlot({
        print(plotInput())
    })
    
    output$table <- DT::renderDataTable(
        DT::datatable({
            # nearPoints(swiid_source, input$plot_click, xvar = "year", yvar = "gini") %>% 
            swiid_source %>% 
                filter(country == input$country & 
                           year >= input$dates[1] &
                           year <= input$dates[2]) %>%
                transmute(Year = year,
                          Gini = round(gini, 1),
                          `Welfare Definition` = case_when(
                              welfare_def == "disp" ~ "Disposable Income",
                              welfare_def == "gross" ~ "Gross Income",
                              welfare_def == "market" ~ "Market Income",
                              welfare_def == "con" ~ "Consumption"),
                          `Equivalence Scale` = case_when(
                              equiv_scale == "ae" ~ "Adult Equivalent",
                              equiv_scale == "hh" ~ "Household",
                              equiv_scale == "pc" ~ "Per Capita",
                              equiv_scale == "sqrt" ~ "Square Root",
                              equiv_scale == "oecdm" ~ "OECD Modified"),
                          Series = series,
                          Source = source1,
                          Link = paste0("<a href='", link, "' target='_blank'>", link, "</a>"))},
            rownames = FALSE,
            escape = -7,
            class = "stripe",
            options = list(paging = FALSE,
                           searching = FALSE
                           )
        )
    )
    # 
    # output$click_info <- renderPrint({
    #     # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    #     # were a base graphics plot, we'd need those.
    #     nearPoints(mtcars2, input$plot1_click, addDist = TRUE)
    # })
    
})
