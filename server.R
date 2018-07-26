library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(readr)
library(dplyr)


select_series <- function (df, coordinfo, xvar = "year", yvar = "gini", threshold = 50) {
    if (is.null(coordinfo)) {
        return(df)
    }
    x <- df[[xvar]]
    y <- df[[yvar]]
    coordPx <- scaleCoords(coordinfo$x, coordinfo$y, coordinfo)
    dataPx <- scaleCoords(x, y, coordinfo)
    dists <- sqrt((dataPx$x - coordPx$x)^2 + (dataPx$y - coordPx$y)^2)
    keep_rows <- (dists <= threshold)
    keep_idx <- which(keep_rows)
    dists <- dists[keep_idx]
    keep_idx <- keep_idx[order(dists)][1]
    keep_series <- df[keep_idx, ] %>% 
        pull(series) %>% 
        unique()

    df1 <- df %>% 
        dplyr::filter(series == keep_series)
    return(df1)
}

scaleCoords <- shiny:::scaleCoords

swiid_source <- read_csv("swiid_source.csv", col_types = "ciddcclcccc") %>% 
    select(-monetary) %>% 
    mutate(gini = gini*100)

shinyServer(function(input, output, session) {
    
    output$countryControl <- renderUI({
        countries <- swiid_source %>% 
            select(country) %>% 
            distinct()
        selectInput(inputId = "country", 
                    label = "Country", 
                    selected = "United States",
                    choices = countries)
    })
    
    output$yearControl <- renderUI({
        max_year <- swiid_source %>% 
            pull(year) %>% 
            max()
        sliderInput(inputId = "dates", 
                    label = "Years",
                    min = 1960,
                    max = max(max_year, round(max_year, -1)), 
                    value = c(1975, max_year),
                    step = 1,
                    sep = "")
    })
    
    swiid_data <- reactive({
        swiid_source %>%
            filter(country == input$country &
                       year >= input$dates[1] &
                       year <= input$dates[2])
    })

    output$plot <- renderPlot({
        ggplot(swiid_data(),
               aes(year, gini, color = series)) +
            geom_point() + 
            geom_line() + 
            xlab("Year") +
            ylab("Reported Gini Index") +
            expand_limits(x = c(input$dates[1], input$dates[2])) +
            theme(legend.position="none")
    })
    
    output$table <- DT::renderDataTable(
        DT::datatable({
            swiid_data() %>%
                select_series(input$plot_click, xvar = "year", yvar = "gini") %>% 
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
                          Source = paste0("<a href='", link, "' target='_blank'>", source1, "</a>"),
                          Page = page)},
            rownames = FALSE,
            escape = -6,
            class = "stripe",
            options = list(paging = FALSE,
                           searching = FALSE)
        )
    )
    
})
