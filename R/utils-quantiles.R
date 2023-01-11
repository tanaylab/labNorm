load_quantiles <- function(fn, reference, alert = TRUE) {
    pkgenv[[reference]] <- readRDS(fn)
    if (alert) {
        cli::cli_alert("Loading quantiles from {.file {fn}}.")
    }
    return(pkgenv[[reference]])
}

has_reference <- function(reference) {
    if (reference == "Clalit-demo") {
        return(TRUE)
    }
    if (is.null(pkgenv[[reference]])) {
        if (is.null(options("labNorm.dir"))) {
            return(FALSE)
        }
        if (!file.exists(file.path(options("labNorm.dir"), paste0(reference, ".rds")))) {
            return(FALSE)
        }
        load_quantiles(file.path(options("labNorm.dir"), paste0(reference, ".rds")), reference, alert = FALSE)
        return(TRUE)
    } else {
        return(TRUE)
    }
}

get_norm_func <- function(lab, age, sex, reference) {
    if (!has_reference(reference)) {
        cli::cli_abort("The {.field reference} {.val {reference}} is not available. Please download it using {.code ln_download_data()}.")
    }
    if (reference == "Clalit-demo") {
        return(LAB_QUANTILES[[lab]][[paste0(age, ".", sex)]])
    }
    if (reference == "Clalit") {
        return(pkgenv$Clalit[[lab]][[paste0(age, ".", sex)]])
    }
    if (reference == "UKBB") {
        age <- as.character(cut(age, seq(35, 80, 5), right = FALSE))
        if (is.na(age)) {
            return(NA)
        }
        return(pkgenv$UKBB[[lab]][[paste0(age, ".", sex)]])
    }
}
