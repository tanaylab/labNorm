#' Convert values to the default units for the lab
#'
#' @param values a vector of lab values
#' @param units the units of the lab values. See \code{LAB_DETAILS$units} for a list of available units for each lab. If different values have different units then this should be a vector of the same length as \code{values}.
#' @param lab the lab name. See \code{LAB_DETAILS$short_name} for a list of available labs.
#'
#' @return the values converted to the default units for the lab
#'
#' @examples
#'
#' # emulate a dataset with different units
#'
#' hemoglobin_diff_units <- hemoglobin_data
#'
#' # first 50 values will be in mg/ML
#' hemoglobin_diff_units$value[1:50] <- hemoglobin_diff_units$value[1:50] * 10
#'
#' # last 50 values will be in mmol/L
#' hemoglobin_diff_units$value[51:100] <- hemoglobin_diff_units$value[51:100] / 1.61
#'
#'
#' converted <- ln_convert_units(
#'     hemoglobin_diff_units$value[1:100],
#'     c(rep("mg/mL", 50), rep("mmol/L", 50)),
#'     "Hemoglobin"
#' )
#'
#' head(converted)
#' head(hemoglobin_data$value)
#' @export
ln_convert_units <- function(values, units, lab) {
    lab_info <- get_lab_info(lab)

    validate_units(units, lab)

    if (length(units) == 1) {
        units <- rep(units, length(values))
    }

    if (length(units) != length(values)) {
        cli::cli_abort("The number of {.field units} must be 1 or the same as the number of {.field values}.")
    }

    if (!all(units == lab_info$default_units)) {
        conversion <- UNITS_CONVERSION[[lab]]
        uniq_units <- unique(units)
        uniq_units[uniq_units != lab_info$default_units]
        for (unit in uniq_units) {
            cli::cli_alert_info("Converting {.field {unit}} to {.field {lab_info$default_units}} for lab {.field {lab}}. Using the formula {.code {deparse(conversion[[unit]])[2]}}.")
            values[units == unit] <- conversion[[unit]](values[units == unit])
        }
    }

    return(values)
}
