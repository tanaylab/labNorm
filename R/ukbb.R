
#' Normalize UKBB labs to age and sex
#'
#' @param lab_code UKBB lab code (e.g. 30020 for hemoglobin). You can see the available labs by running \code{ln_ukbb_labs()}.
#'
#' @examples
#' \dontrun{
#' hemoglobin_data$quantile <- ln_normalize_ukbb(
#'     hemoglobin_data$value,
#'     hemoglobin_data$age,
#'     hemoglobin_data$sex,
#'     "30020"
#' )
#' }
#'
#' @export
#' @rdname ln_normalize
ln_normalize_ukbb <- function(values, age, sex, lab_code, reference = "UKBB", na.rm = FALSE) {
    ln_normalize(values = values, age = age, sex = sex, units = ln_ukbb_units(lab_code), lab = ln_ukbb_name(lab_code), reference = reference, na.rm = na.rm)
}

#' Normalize UKBB multiple labs for age and sex
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' multi_labs_df <- bind_rows(
#'     hemoglobin_data %>% mutate(lab_code = "30020"),
#'     creatinine_data %>% mutate(lab_code = "30700")
#' )
#'
#'
#' multi_labs_df$quantile <- ln_normalize_multi_ukbb(multi_labs_df)
#'
#' head(multi_labs_df)
#' }
#'
#' @rdname ln_normalize
#' @export
ln_normalize_multi_ukbb <- function(labs_df, reference = "UKBB", na.rm = FALSE) {
    # validate that labs_df has the right columns
    required_columns <- c("value", "age", "sex", "lab_code")
    purrr::walk(required_columns, function(col) {
        if (!(col %in% colnames(labs_df))) {
            cli::cli_abort("{.field labs_df} must have a column named {.field {col}}")
        }
    })

    codes <- unique(labs_df$lab_code)
    normalized <- rep(NA, nrow(labs_df))
    for (code in codes) {
        lab_df <- labs_df[labs_df$lab_code == code, ]
        normalized[labs_df$lab_code == code] <- ln_normalize_ukbb(
            lab_df$value,
            lab_df$age,
            lab_df$sex,
            code,
            reference = reference,
            na.rm = na.rm
        )
    }

    return(normalized)
}

#' Normalize Clalit multiple labs for age and sex
#'
#' @examples
#' library(dplyr)
#' multi_labs_df <- bind_rows(
#'     hemoglobin_data %>% mutate(lab_code = "lab.103"),
#'     creatinine_data %>% mutate(lab_code = "lab.20300")
#' )
#' \donttest{
#' multi_labs_df$quantile <- ln_normalize_multi_clalit(multi_labs_df)
#' }
#' # on the demo data
#' \dontshow{
#' multi_labs_df$quantile <- ln_normalize_multi_clalit(multi_labs_df, reference = "Clalit-demo")
#' }
#' head(multi_labs_df)
#'
#' @rdname ln_normalize
#' @export
ln_normalize_multi_clalit <- function(labs_df, reference = "Clalit", na.rm = FALSE) {
    # validate that labs_df has the right columns
    required_columns <- c("value", "age", "sex", "lab_code")
    purrr::walk(required_columns, function(col) {
        if (!(col %in% colnames(labs_df))) {
            cli::cli_abort("{.field labs_df} must have a column named {.field {col}}")
        }
    })

    codes <- unique(labs_df$lab_code)
    normalized <- rep(NA, nrow(labs_df))
    for (code in codes) {
        lab_df <- labs_df[labs_df$lab_code == code, ]
        normalized[labs_df$lab_code == code] <- ln_normalize_clalit(
            lab_df$value,
            lab_df$age,
            lab_df$sex,
            code,
            reference = reference,
            na.rm = na.rm
        )
    }

    return(normalized)
}

ln_ukbb_units <- function(lab_code) {
    LAB_DETAILS %>%
        filter(ukbb_code == lab_code) %>%
        pull(ukbb_units)
}

ln_ukbb_name <- function(lab_code) {
    LAB_DETAILS %>%
        filter(ukbb_code == lab_code) %>%
        pull(short_name)
}

#' Get available UKBB labs
#'
#' @return A data frame with the available UKBB labs.
#'
#' @examples
#' ln_ukbb_labs()
#'
#' @export
ln_ukbb_labs <- function() {
    LAB_DETAILS %>%
        select(short_name, long_name, ukbb_code, ukbb_units) %>%
        filter(!is.na(ukbb_code))
}

#' Normalize Clalit labs to age and sex
#'
#' @param lab_code Clalit lab code (e.g. "lab.103" for hemoglobin). You can see the available labs by running \code{ln_ukbb_labs()}.
#'
#' @examples
#' \dontrun{
#' hemoglobin_data$quantile <- ln_normalize_clalit(
#'     hemoglobin_data$value,
#'     hemoglobin_data$age,
#'     hemoglobin_data$sex,
#'     "lab.103"
#' )
#' }
#'
#' @export
#' @rdname ln_normalize
ln_normalize_clalit <- function(values, age, sex, lab_code, reference = "Clalit", na.rm = FALSE) {
    # deafult units are based on clalit units
    ln_normalize(values = values, age = age, sex = sex, lab = ln_clalit_name(lab_code), reference = reference, na.rm = na.rm)
}

ln_clalit_name <- function(lab_code) {
    LAB_DETAILS %>%
        filter(clalit_code == lab_code) %>%
        pull(short_name)
}
