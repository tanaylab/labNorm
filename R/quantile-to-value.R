
#' Compute the lab value for a given quantile
#'
#' @description The function \code{ln_quantile_value} calculates lab values at a specified quantile, using the default units for that lab. The function \code{ln_patients_quantile_value} does the same calculation for a specific group of patients. \cr
#' Default units for a lab can be obtained using \code{ln_lab_default_units}. \cr
#' If no quantile data is available for a particular lab, age, and sex, the function returns 'NA'. \cr
#' It should be noted that the values of extreme quantiles (e.g. >0.95 or <0.05 on low resolution, >0.99 or <0.01 on high resolution) may not be reliable, as they may represent outliers in the data. \cr \cr
#'
#' Note that \code{ln_quantile_value} returns values for all combinations of age, sex, and lab, while \code{ln_patients_quantile_value} returns values for a specific set of patients, similar to \code{ln_normalize}.
#'
#'
#'
#' @param quantiles a vector of quantiles (in the range 0-1) to compute the lab value for, or a vector with a quantile for each patient when running \code{ln_patients_quantile_value}.
#' @param age a vector of ages to compute the lab values for or a vector with an age for each patient when running \code{ln_patients_quantile_value}.
#' @param sex the sexes to compute the lab values for, or a vector with a sex for each patient when running \code{ln_patients_quantile_value}. Note that for \code{ln_quantile_value} this parameter can only be either: "male", "female" or c("male", "female")
#' @param lab The lab name.
#' @param allow_edge_quantiles If \code{TRUE} (default) then the function will return the value for the edge quantiles (<0.01 or >0.99) even though they are not reliable. If \code{FALSE} then the function will return \code{NA} for those quantiles. Note that for the "Clalit-demo" reference, the threshold would be <0.05 or >0.95.
#'
#' @return \code{ln_quantile_value} returns a data frame which contains the values for each combination of quantile, age and sex.
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
#' \code{ln_patients_quantile_value} returns a vector of value per patient.
#'
#'
#'
#' @examples
#' \donttest{
#' ln_quantile_value(c(0.05, 0.5, 0.95), 50, "male", "WBC")
#'
#' ln_quantile_value(
#'     c(0, 0.05, 0.1, 0.4, 0.5, 0.6, 0.9, 1),
#'     c(50, 60),
#'     c("male", "female"),
#'     "Glucose"
#' )
#' }
#'
#' # on the demo data
#' \dontshow{
#' ln_quantile_value(c(0.05, 0.5, 0.95), 50, "male", "WBC", reference = "Clalit-demo")
#' }
#'
#' @inheritParams ln_normalize
#' @export
ln_quantile_value <- function(quantiles, age, sex, lab, reference = "Clalit", allow_edge_quantiles = FALSE) {
    validate_lab(lab)

    validate_quantiles(quantiles)
    if (length(unique(quantiles)) != length(quantiles)) {
        cli::cli_alert_warning("The quantiles vector contains duplicate values. Did you mean to use ln_patients_quantile_value?")
    }

    if (length(unique(age)) != length(age)) {
        cli::cli_alert_warning("The age vector contains duplicate values. Did you mean to use ln_patients_quantile_value?")
    }

    if (length(sex) > 2) {
        cli::cli_abort("{.field sex} can be either {.val male}, {.val female} or {.code c('male', 'female')}. If you have multiple patients, please run {.code ln_patients_quantile_value}")
    }

    params <- expand.grid(age = unique(age), sex = unique(sex))

    if (reference %in% c("Clalit", "UKBB")) {
        if (!has_reference(reference)) {
            ln_download_data()
        }
    }

    if (reference == "Clalit-demo") {
        min_q <- 0.05
        max_q <- 0.95
    } else {
        min_q <- 0.01
        max_q <- 0.99
    }

    res <- params %>%
        purrr::pmap_dfr(function(...) {
            .x <- tibble(...)
            values <- ln_patients_quantile_value(quantiles, .x$age, .x$sex, lab, reference = reference, allow_edge_quantiles = allow_edge_quantiles)
            data.frame(age = .x$age, sex = .x$sex, quantile = quantiles, value = values, unit = ln_lab_default_units(lab), lab = lab)
        })

    return(res)
}

#' Compute the lab value for quantiles of patients
#'
#'
#'
#' @examples
#' \donttest{
#' hemoglobin_data$quantile <- ln_normalize(
#'     hemoglobin_data$value,
#'     hemoglobin_data$age,
#'     hemoglobin_data$sex,
#'     "Hemoglobin"
#' )
#'
#' hemoglobin_data$value1 <- ln_patients_quantile_value(
#'     hemoglobin_data$quantile,
#'     hemoglobin_data$age,
#'     hemoglobin_data$sex,
#'     "Hemoglobin"
#' )
#' head(hemoglobin_data)
#' }
#'
#' \dontshow{
#' hemoglobin_data$quantile <- ln_normalize(
#'     hemoglobin_data$value,
#'     hemoglobin_data$age,
#'     hemoglobin_data$sex,
#'     "Hemoglobin",
#'     reference = "Clalit-demo"
#' )
#'
#' hemoglobin_data$value1 <- ln_patients_quantile_value(
#'     hemoglobin_data$quantile,
#'     hemoglobin_data$age,
#'     hemoglobin_data$sex,
#'     "Hemoglobin",
#'     reference = "Clalit-demo"
#' )
#' head(hemoglobin_data)
#' }
#'
#' @rdname ln_quantile_value
#' @export
ln_patients_quantile_value <- function(quantiles, age, sex, lab, reference = "Clalit", allow_edge_quantiles = FALSE) {
    lab_info <- get_lab_info(lab)

    if (length(age) == 1) {
        age <- rep(age, length(quantiles))
    }

    if (length(sex) == 1) {
        sex <- rep(sex, length(quantiles))
    }

    validate_age_and_sex(age, sex, reference)

    if (length(quantiles) != length(age)) {
        cli::cli_abort("The length of {.field quantiles} must be the same as the length of {.field age}. Did you mean to use {.code ln_quantile_value} instead?")
    }

    if (reference == "Clalit-demo") {
        min_q <- 0.05
        max_q <- 0.95
    } else {
        min_q <- 0.01
        max_q <- 0.99
    }

    # extract values
    ages <- unique(age)
    sexes <- unique(sex)
    values <- rep(NA, length(quantiles))

    if (reference %in% c("Clalit", "UKBB")) {
        if (!has_reference(reference)) {
            ln_download_data()
        }
    }

    for (cur_age in ages) {
        for (cur_sex in sexes) {
            if (!age_in_range(cur_age, reference)) {
                values[age == cur_age & sex == cur_sex] <- NA
            } else {
                func <- get_norm_func(lab, cur_age, cur_sex, reference)

                # test if func is a function
                cur_quantiles <- quantiles[age == cur_age & sex == cur_sex]
                if (is.function(func)) {
                    func_env <- environment(func)
                    all_vals <- func_env$x
                    all_quants <- func_env$y
                    func_quant_to_val <- approxfun(x = all_quants, y = all_vals, rule = 2)
                    values[age == cur_age & sex == cur_sex] <- func_quant_to_val(cur_quantiles)
                } else {
                    values[age == cur_age & sex == cur_sex] <- NA
                }

                if (!allow_edge_quantiles) {
                    values[(age == cur_age & sex == cur_sex) & (quantiles < min_q | quantiles > max_q)] <- NA
                }
            }
        }
    }

    return(values)
}
