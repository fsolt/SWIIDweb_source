library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(readr)
library(dplyr)

nearPoints1 <- function (df, coordinfo, xvar = NULL, yvar = NULL, panelvar1 = NULL, 
                       panelvar2 = NULL, threshold = 5, maxpoints = NULL, addDist = FALSE, 
                       allRows = FALSE) 
{
    if (is.null(coordinfo)) {
        if (addDist) 
            df$dist_ <- NA_real_
        if (allRows) 
            df$selected_ <- FALSE
        else df <- df[0, , drop = FALSE]
        return(df)
    }
    if (is.null(coordinfo$x)) {
        stop("nearPoints requires a click/hover/double-click object with x and y values.")
    }
    xvar <- xvar
    yvar <- yvar
    # panelvar1 <- panelvar1 shiny:::%OR% coordinfo$mapping$panelvar1
    # panelvar2 <- panelvar2 %OR% coordinfo$mapping$panelvar2
    if (is.null(xvar)) 
        stop("nearPoints: not able to automatically infer `xvar` from coordinfo")
    if (is.null(yvar)) 
        stop("nearPoints: not able to automatically infer `yvar` from coordinfo")
    x <- as.numeric(df[[xvar]])
    y <- as.numeric(df[[yvar]])
    coordPx <- scaleCoords1(coordinfo$x, coordinfo$y, coordinfo)
    dataPx <- scaleCoords1(x, y, coordinfo)
    dists <- sqrt((dataPx$x - coordPx$x)^2 + (dataPx$y - coordPx$y)^2)
    if (addDist) 
        df$dist_ <- dists
    keep_rows <- (dists <= threshold)
    if (!is.null(panelvar1)) 
        keep_rows <- keep_rows & panelMatch(coordinfo$panelvar1, 
                                            df[[panelvar1]])
    if (!is.null(panelvar2)) 
        keep_rows <- keep_rows & panelMatch(coordinfo$panelvar2, 
                                            df[[panelvar2]])
    keep_idx <- which(keep_rows)
    dists <- dists[keep_idx]
    keep_idx <- keep_idx[order(dists)]
    if (!is.null(maxpoints) && length(keep_idx) > maxpoints) {
        keep_idx <- keep_idx[seq_len(maxpoints)]
    }
    if (allRows) {
        df$selected_ <- FALSE
        df$selected_[keep_idx] <- TRUE
    }
    else {
        df <- df[keep_idx, , drop = FALSE]
    }
    df
}

scaleCoords1 <- function (x, y, scaleinfo) {
    if (is.null(scaleinfo)) 
        return(NULL)
    domain <- scaleinfo$domain
    range <- scaleinfo$range
    print(c("range", range))
    log <- scaleinfo$log
    list(x = scale1D(x, domain$left, domain$right, range$left, 
                     range$right, log$x), y = scale1D(y, domain$bottom, domain$top, 
                                                      range$bottom, range$top, log$y))
}

select_series <- function (df, coordinfo, xvar = "year", yvar = "gini", threshold = 5) {
    if (is.null(coordinfo)) {
        return(df)
    }
    if (is.null(coordinfo$x)) {
        stop("select_series requires a click/hover/double-click object with x and y values.")
    }
    x <- df[[xvar]]
    y <- df[[yvar]]
    print(c("coordinfo", coordinfo))
    coordPx <- scaleCoords(coordinfo$x, coordinfo$y, coordinfo)
    dataPx <- scaleCoords(x, y, coordinfo)
    dists <- sqrt((dataPx$x - coordPx$x)^2 + (dataPx$y - coordPx$y)^2)
    keep_rows <- (dists <= threshold)
    keep_idx <- which(keep_rows)
    dists <- dists[keep_idx]
    keep_idx <- keep_idx[order(dists)]
    keep_series <- df[keep_idx, ] %>% 
        pull(series) %>% 
        unique()

    df1 <- df %>% 
        dplyr::filter(series == keep_series)
    return(df1)
}

scaleCoords <- function (x, y, scaleinfo) {
    if (is.null(scaleinfo)) 
        return(NULL)
    domain <- scaleinfo$domain
    #print(c("domain", domain))
    range <- scaleinfo$range
    #print(c("range", range))
    sc <- list(x = scale1D(x, domain$left, domain$right, range$left, range$right),
         y = scale1D(y, domain$bottom, domain$top, range$bottom, range$top))
    #print(c("sc", sc))
    return(sc)
}

scale1D <- function (val, domainMin, domainMax, rangeMin, rangeMax, clip = TRUE) {
    factor <- (rangeMax - rangeMin)/(domainMax - domainMin)
    val <- val - domainMin
    newval <- (val * factor) + rangeMin
    if (clip) {
        maxval <- max(rangeMax, rangeMin)
        minval <- min(rangeMax, rangeMin)
        newval[newval > maxval] <- maxval
        newval[newval < minval] <- minval
    }
    newval
}

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
            swiid_source %>% 
                filter(country == input$country & 
                           year >= input$dates[1] &
                           year <= input$dates[2]) %>%
                #select_series(input$plot_click, xvar = "year", yvar = "gini") %>% 
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
                          Source = paste0("<a href='", link, "' target='_blank'>", source1, "</a>"))},
            rownames = FALSE,
            escape = -6,
            class = "stripe",
            options = list(paging = FALSE,
                           searching = FALSE
                           )
        )
    )
# 
#     output$click_info <- renderPrint({
#         nearPoints1(swiid_source %>% 
#                        filter(country == input$country & 
#                                   year >= input$dates[1] &
#                                   year <= input$dates[2]),
#                               input$plot_click,
#                    xvar = "year",
#                    yvar = "gini")
#     })
    
})
