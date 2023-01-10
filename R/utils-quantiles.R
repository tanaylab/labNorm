load_quantiles <- function(fn, alert = TRUE) {
    pkgenv$quantiles <- readRDS(fn)
    if (alert) {
        cli::cli_alert("Loading quantiles from {.file {fn}}.")
    }
    return(pkgenv$quantiles)
}

get_quantiles <- function() {
    if (is.null(getOption("labNorm.use_low_res"))) {
        options(labNorm.use_low_res = FALSE)
    }

    if (!is.null(getOption("labNorm.use_low_res")) && getOption("labNorm.use_low_res")) {
        return(LAB_QUANTILES)
    }

    quantiles <- NULL
    if (!is.null(pkgenv$quantiles)) {
        quantiles <- pkgenv$quantiles
    } else {
        fn <- "high_res_labs.rds"
        if (!is.null(getOption("labNorm.dir")) && file.exists(file.path(getOption("labNorm.dir"), fn))) {
            quantiles <- load_quantiles(file.path(getOption("labNorm.dir"), fn))
        } else if (file.exists(file.path(rappdirs::user_data_dir("Labnorm"), fn))) {
            quantiles <- load_quantiles(file.path(rappdirs::user_data_dir("Labnorm"), fn))
        } else if (file.exists(fn)) {
            quantiles <- load_quantiles(fn)
        } else {
            if (interactive() && !pkgenv$asked_to_download && !getOption("labNorm.use_low_res")) {
                pkgenv$asked_to_download <- TRUE
                # ask the user if they want to download the quantiles
                if (pkgenv$yesno2("The high resolution reference distributions are not available. Would you like to download them now?\n(this question will only be asked once per session)")) {
                    ln_download_data()
                    quantiles <- load_quantiles(file.path(getOption("labNorm.dir"), fn), alert = FALSE)
                }
            }
        }
    }

    if (is.null(quantiles)) {
        if (!pkgenv$alerted_about_download) {
            cli::cli_alert("Using default quantiles. For higher resolution quantiles, run {.code ln_download_data()}. This message will only be shown once per session.")
            pkgenv$alerted_about_download <- TRUE
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
#' @description This function computes the lab value for given quantiles in the default units for the lab. Default units can be found using the function \code{ln_lab_default_units}. In case where no quantile is available for a given lab, age, and sex the function returns \code{NA}. \cr
#' Note that the values very high or low quantiles (e.g. >0.95,<0.05 on the low resolution version, >0.99,<0.01 on the high resolution version) are not reliable as they can represent technical outliers of the data.
#'
#'
#' @param quantiles a vector of quantiles (in the range 0-1) to compute the lab value for.
#' @param lab The lab name.
#' @param allow_edge_quantiles If \code{TRUE} (default) then the function will return the value for the edge quantiles (>0.05 or <0.95 for the low-res version, <0.01 or >0.99 for the high-res version) even though they are not reliable. If \code{FALSE} then the function will return \code{NA} for those quantiles.
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
#' ln_quantile_value(c(0.05, 0.5, 0.95), 50, "male", "WBC")
#'
#' ln_quantile_value(
#'     c(0, 0.05, 0.1, 0.4, 0.5, 0.6, 0.9, 1),
#'     c(50, 60),
#'     c("male", "female"),
#'     "Glucose"
#' )
#'
#' @inheritParams ln_normalize
#' @export
ln_quantile_value <- function(quantiles, age, sex, lab, allow_edge_quantiles = FALSE) {
    validate_lab(lab)

    validate_quantiles(quantiles)

    params <- expand.grid(age = age, sex = sex)

    validate_age_and_sex(params$age, params$sex)

    all_quantiles <- get_quantiles()

    if (ln_is_high_res()) {
        qmin <- 0.01
        qmax <- 0.99
    } else {
        qmin <- 0.05
        qmax <- 0.95
    }

    res <- params %>%
        purrr::pmap_dfr(function(...) {
            .x <- tibble(...)
            func <- all_quantiles[[lab]][[paste0(.x$age, ".", .x$sex)]]
            if (is.function(func)) {
                func_env <- environment(func)
                all_vals <- func_env$x
                all_quants <- func_env$y
                func_quant_to_val <- approxfun(x = all_quants, y = all_vals, rule = 2)
                values <- func_quant_to_val(quantiles)
            } else {
                values <- rep(NA, length(quantiles))
            }

            if (!allow_edge_quantiles) {
                values[quantiles < qmin | quantiles > qmax] <- NA
            }

            data.frame(age = .x$age, sex = .x$sex, quantile = quantiles, value = values, unit = ln_lab_default_units(lab), lab = lab)
        })

    return(res)
}
