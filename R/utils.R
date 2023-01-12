validate_lab <- function(lab) {
    if (!(lab %in% LAB_DETAILS$short_name)) {
        cli::cli_abort("Lab {.field {lab}} is not available. Available labs can be found in {.code LAB_DETAILS$short_name}. Examples are {.field Hemoglobin} and {.field Creatinine}.", call = parent.frame(1))
    }
}

validate_units <- function(units, lab) {
    lab_info <- get_lab_info(lab)

    if (!all(units %in% lab_info$units[[1]])) {
        bad_values <- unique(units[!(units %in% lab_info$units[[1]])])
        cli::cli_abort("Invalid units: {.val {bad_values}} for lab {.field {lab}}. Available units are: {.val {lab_info$units[[1]]}}.", call = parent.frame(1))
    }
}

validate_na <- function(age, sex, values) {
    if (any(is.na(age))) {
        cli::cli_abort("Missing values in {.field age}.", call = parent.frame(1))
    }
    if (any(is.na(sex))) {
        cli::cli_abort("Missing values in {.field age}.", call = parent.frame(1))
    }
    if (any(is.na(values))) {
        cli::cli_abort("Missing values in {.field values}.", call = parent.frame(1))
    }
}

validate_age_and_sex <- function(age, sex, reference) {
    if (any(!is.na(age) & age < pkgenv$age_limits[[reference]][1])) {
        cli::cli_warn("Age must be at least {.val {pkgenv$age_limits[[reference]][1]}} for {.field {reference}}.", call = parent.frame(1))
    }
    if (any(!is.na(age) & age > pkgenv$age_limits[[reference]][2])) {
        cli::cli_warn("Age must be at most {.val {pkgenv$age_limits[[reference]][2]}} for {.field {reference}}.", call = parent.frame(1))
    }

    if (!all(sex[!is.na(sex)] %in% c("male", "female"))) {
        bad_values <- unique(sex[sex %in% c("male", "female")])
        cli::cli_abort("Invalid sex values: {.val {bad_values}}. Sex can only be {.val male} or {.val female}", call = parent.frame(1))
    }

    if (length(age) != length(sex)) {
        cli::cli_abort("The length of {.field age} must be the same as the length of {.field sex}.", call = parent.frame(1))
    }
}

age_in_range <- function(age, reference) {
    return(age >= pkgenv$age_limits[[reference]][1] & age <= pkgenv$age_limits[[reference]][2])
}

validate_quantiles <- function(quantiles) {
    if (any(quantiles < 0) || any(quantiles > 1)) {
        cli::cli_abort("All quantiles must be in the range 0-1.", call = parent.frame(1))
    }
}

get_lab_info <- function(lab) {
    validate_lab(lab)
    LAB_DETAILS[LAB_DETAILS$short_name == lab, ]
}

#' Get available units for a lab
#'
#' @param lab the lab name. See \code{LAB_DETAILS$short_name} for a list of available labs.
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

#' Get the default units for a lab
#' @param lab the lab name. See \code{LAB_DETAILS$short_name} for a list of available labs.
#' @return the default units for the lab
#' @examples
#' ln_lab_default_units("Hemoglobin")
#' @export
#' @rdname ln_lab_units
ln_lab_default_units <- function(lab) {
    validate_lab(lab)
    get_lab_info(lab)$default_units
}
