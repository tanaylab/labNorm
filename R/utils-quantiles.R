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

#' Compute the lab value for a given quantile
#'
#' @description This function computes the lab value for given quantiles in the default units for the lab. Default units can be found using the function \code{ln_lab_default_units}. In cases where the original data had only a single value, NA is returned.
#'
#' @param quantiles a vector of quantiles (in the range 0-1) to compute the lab value for.
#' @param lab The lab name.
#'
#' @return a data frame which contains the values for each combination of quantile, age and sex.
#' The data frame has the the following columns:
#'
#' \itemize{
#' \item{age: }{age in years}
#' \item{sex: }{"male" or "female"}
#' \item{quantile: }{he quantile}
#' \item{value: }{the lab value}
#' \item{unit: }{the lab unit}
#' \item{lab: }{the lab name}
#' }
#'
#'
#'
#' @examples
#' ln_quantile_value(c(0, 0.03, 0.5, 0.97, 1), 50, "male", "WBC")
#'
#' ln_quantile_value(
#'     c(0, 0.03, 0.1, 0.4, 0.5, 0.6, 0.9, 0.97, 1),
#'     c(50, 60),
#'     c("male", "female"),
#'     "Glucose"
#' )
#'
#' @inheritParams ln_normalize
#' @export
ln_quantile_value <- function(quantiles, age, sex, lab) {
    validate_lab(lab)

    validate_quantiles(quantiles)

    params <- expand.grid(age = age, sex = sex)

    validate_age_and_sex(params$age, params$sex)

    all_quantiles <- get_quantiles()

    res <- params %>%
        purrr::pmap_dfr(function(...) {
            .x <- tibble(...)
            func <- all_quantiles[[lab]][[paste0(.x$age, ".", .x$sex)]]
            func_env <- environment(func)
            all_vals <- func_env$x
            all_quants <- func_env$y
            if (is.null(all_vals) || is.null(all_quants)) {
                values <- rep(NA, length(quantiles))
            } else {
                func_quant_to_val <- approxfun(x = all_quants, y = all_vals, rule = 2)
                values <- func_quant_to_val(quantiles)
            }

            data.frame(age = .x$age, sex = .x$sex, quantile = quantiles, value = values, unit = ln_lab_default_units(lab), lab = lab)
        })

    return(res)
}
