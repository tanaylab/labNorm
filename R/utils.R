the <- new.env(parent = emptyenv())
the$asked_to_download <- FALSE
the$alerted_about_download <- FALSE
the$yesno2 <- yesno::yesno2 # used inside an environment so that it can be mocked

load_quantiles <- function(fn, alert = TRUE) {
    the$quantiles <- readRDS(fn)
    if (alert) {
        cli::cli_alert("Loading quantiles from {.file {fn}}.")
    }
    return(the$quantiles)
}

get_quantiles <- function() {
    if (is.null(getOption("labNorm.use_low_res"))) {
        options(labNorm.use_low_res = FALSE)
    }

    if (!is.null(getOption("labNorm.use_low_res")) && getOption("labNorm.use_low_res")) {
        return(LAB_QUANTILES)
    }

    quantiles <- NULL
    if (!is.null(the$quantiles)) {
        quantiles <- the$quantiles
    } else {
        fn <- "high_res_labs.rds"
        if (!is.null(getOption("labNorm.dir")) && file.exists(file.path(getOption("labNorm.dir"), fn))) {
            quantiles <- load_quantiles(file.path(getOption("labNorm.dir"), fn))
        } else if (file.exists(file.path(rappdirs::user_data_dir("Labnorm"), fn))) {
            quantiles <- load_quantiles(file.path(rappdirs::user_data_dir("Labnorm"), fn))
        } else if (file.exists(fn)) {
            quantiles <- load_quantiles(fn)
        } else {
            if (interactive() && !the$asked_to_download && !getOption("labNorm.use_low_res")) {
                the$asked_to_download <- TRUE
                # ask the user if they want to download the quantiles
                if (the$yesno2("The high resolution reference distributions are not available. Would you like to download them now?\n(this question will only be asked once per session)")) {
                    ln_download_data()
                    quantiles <- load_quantiles(file.path(getOption("labNorm.dir"), fn), alert = FALSE)
                }
            }
        }
    }

    if (is.null(quantiles)) {
        if (!the$alerted_about_download) {
            cli::cli_alert("Using default quantiles. For higher resolution quantiles, run {.code ln_download_data()}. This message will only be shown once per session.")
            the$alerted_about_download <- TRUE
        }
        quantiles <- LAB_QUANTILES
    }

    return(quantiles)
}

validate_lab <- function(lab) {
    if (!(lab %in% LAB_INFO$short_name)) {
        cli::cli_abort("Lab {.field {lab}} is not available. Available labs can be found in {.code LAB_INFO$short_name}. Examples are {.field Hemoglobin} and {.field Creatinine}.", call = parent.frame(1))
    }
}

validate_units <- function(units, lab) {
    lab_info <- get_lab_info(lab)

    if (!all(units %in% lab_info$units[[1]])) {
        bad_values <- unique(units[!(units %in% lab_info$units[[1]])])
        cli::cli_abort("Invalid units: {.val {bad_values}} for lab {.field {lab}}. Available units are: {.val {lab_info$units[[1]]}}.", call = parent.frame(1))
    }
}

validate_age_and_sex <- function(age, sex) {
    if (any(age < 20 | age > 99)) {
        cli::cli_abort("Invalid age. Age must be between 20 and 99.", call = parent.frame(1))
    }

    if (!all(sex %in% c("male", "female"))) {
        bad_values <- unique(sex[sex %in% c("male", "female")])
        cli::cli_abort("Invalid sex values: {.val {bad_values}}. Sex can only be {.val male} or {.val female}", call = parent.frame(1))
    }

    if (length(age) != length(sex)) {
        cli::cli_abort("The length of {.field age} must be the same as the length of {.field sex}.", call = parent.frame(1))
    }
}

get_lab_info <- function(lab) {
    validate_lab(lab)
    LAB_INFO[LAB_INFO$short_name == lab, ]
}

#' Check if data was downloaded
#'
#' @return True if the data was downloaded, false otherwise.
#' @rdname ln_download_data
ln_is_high_res <- function() {
    quantiles <- get_quantiles()
    if (identical(quantiles, LAB_QUANTILES)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#' Get available units for a lab
#'
#' @param lab the lab name. See \code{LAB_INFO$short_name} for a list of available labs.
#'
#' @return a vector of available units for the lab
#'
#' @examples
#' ln_lab_units("Hemoglobin")
#'
#' @export
ln_lab_units <- function(lab) {
    validate_lab(lab)
    get_lab_info(lab)$units[[1]]
}
