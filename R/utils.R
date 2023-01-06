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
    get_lab_info(lab)$units[[1]]
}
