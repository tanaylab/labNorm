clean_downloaded_data <- function() {
    if (!is.null(getOption("labNorm.dir")) && dir.exists(getOption("labNorm.dir"))) {
        unlink(file.path(getOption("labNorm.dir"), "Clalit"), recursive = TRUE)
        unlink(file.path(getOption("labNorm.dir"), "UKBB"), recursive = TRUE)
        options(labNorm.dir = NULL)
        pkgenv$asked_to_download <- FALSE
        pkgenv$alerted_about_download <- FALSE
        pkgenv[["Clalit"]] <- NULL
        pkgenv[["UKBB"]] <- NULL
    }
}
