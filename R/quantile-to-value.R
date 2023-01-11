
#' Compute the lab value for a given quantile
#'
#' @description This function computes the lab value for given quantiles in the default units for the lab. Default units can be found using the function \code{ln_lab_default_units}. In case where no quantile is available for a given lab, age, and sex the function returns \code{NA}. \cr
#' Note that the values of very high or low quantiles (e.g. >0.95,<0.05 on the low resolution version, >0.99,<0.01 on the high resolution version) are not reliable as they can represent technical outliers of the data.
#'
#'
#' @param quantiles a vector of quantiles (in the range 0-1) to compute the lab value for.
#' @param lab The lab name.
#' @param allow_edge_quantiles If \code{TRUE} (default) then the function will return the value for the edge quantiles (<0.01 or >0.99) even though they are not reliable. If \code{FALSE} then the function will return \code{NA} for those quantiles. Note that for the "Clalit-demo" reference, the threshold would be <0.05 or >0.95.
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

    params <- expand.grid(age = age, sex = sex)

    validate_age_and_sex(params$age, params$sex, reference)

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
            if (!age_in_range(.x$age, reference)) {
                values <- rep(NA, length(quantiles))
            } else {
                func <- get_norm_func(lab, .x$age, .x$sex, reference)
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
                    values[quantiles < min_q | quantiles > max_q] <- NA
                }
            }

            data.frame(age = .x$age, sex = .x$sex, quantile = quantiles, value = values, unit = ln_lab_default_units(lab), lab = lab)
        })

    return(res)
}
