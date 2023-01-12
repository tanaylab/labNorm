init_reference <- function(reference) {
    pkgenv[[reference]] <- list()
}

load_quantiles <- function(reference, lab, alert = FALSE) {
    if (!has_reference(reference)) {
        cli::cli_abort("The data for {.code {reference}} has not been downloaded. Please run {.code ln_download_data()} to download the data.")
    }

    if (is.null(pkgenv[[reference]])) {
        init_reference(reference)
    }

    if (!is.null(pkgenv[[reference]][[lab]])) {
        return(pkgenv[[reference]][[lab]])
    }

    fn <- LAB_TO_FILENAME[lab]
    if (is.na(fn)) {
        cli::cli_abort("Invalid lab name {.val lab}.")
    }

    if (is.null(getOption("labNorm.dir"))) {
        cli::cli_abort("The {.field labNorm.dir} option is not set. Please run {.code ln_download_data()} to download the data.")
    }

    full_fn <- file.path(getOption("labNorm.dir"), file.path(reference, glue("{fn}.rds")))
    if (!file.exists(full_fn)) {
        cli::cli_abort("File {.file {full_fn}} does not exist. Please run {.code ln_download_data()} to download the data.")
    }

    pkgenv[[reference]][[lab]] <- readRDS(full_fn)

    if (alert) {
        cli::cli_alert("Loading quantiles from {.file {fn}}.")
    }
    return(pkgenv[[reference]][[lab]])
}

has_reference <- function(reference) {
    if (reference == "Clalit-demo") {
        return(TRUE)
    }
    if (is.null(pkgenv[[reference]])) {
        if (is.null(options("labNorm.dir"))) {
            return(FALSE)
        }
        if (!dir.exists(file.path(options("labNorm.dir"), "reference"))) {
            return(FALSE)
        } else {
            return(TRUE)
        }
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
        quantiles <- load_quantiles(reference, lab)
        return(quantiles[[paste0(age, ".", sex)]])
    }
    if (reference == "UKBB") {
        quantiles <- load_quantiles(reference, lab)
        age <- as.character(cut(age, seq(35, 80, 5), right = FALSE))
        if (is.na(age)) {
            return(NA)
        }
        return(quantiles[[paste0(age, ".", sex)]])
    }
}
