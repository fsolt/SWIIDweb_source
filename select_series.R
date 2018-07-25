select_series <- function (df, coordinfo, xvar = "year", yvar = "gini", 
                           threshold = 5, maxpoints = NULL) {
    if (is.null(coordinfo)) {
        df <- df[0, , drop = FALSE]
        return(df)
    }
    if (is.null(coordinfo$x)) {
        stop("select_series requires a click/hover/double-click object with x and y values.")
    }
    coordPx <- scaleCoords(coordinfo$x, coordinfo$y, coordinfo)
    dataPx <- scaleCoords(x, y, coordinfo)
    dists <- sqrt((dataPx$x - coordPx$x)^2 + (dataPx$y - coordPx$y)^2)
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
    else {
        df <- df[keep_idx, , drop = FALSE]
    }
    df
}

scaleCoords <- function (x, y, scaleinfo) {
    if (is.null(scaleinfo)) 
        return(NULL)
    domain <- scaleinfo$domain
    range <- scaleinfo$range
    log <- scaleinfo$log
    list(x = scale1D(x, domain$left, domain$right, range$left, 
                     range$right, log$x), y = scale1D(y, domain$bottom, domain$top, 
                                                      range$bottom, range$top, log$y))
}