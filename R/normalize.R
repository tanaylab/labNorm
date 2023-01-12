#' Normalize lab values to age and sex
#'
#' @description Normalize standard laboratory measurements (e.g. hemoglobin, cholesterol levels) according to age and sex, based on the algorithms described in "Personalized lab test models to quantify disease potentials in healthy individuals" \doi{10.1038/s41591-021-01468-6}. \cr \cr
#' The "Clalit" reference distributions are based on 2.1B lab measurements taken from 2.8M individuals between 2002-2019, filtered to exclude severe chronic diseases and medication effects. The resulting normalized value is a quantile between 0 and 1, representing the value's position in the reference distribution. \cr \cr
#' The "UKBB" reference distributions are based on the UK-Biobank, a large-scale population-based cohort study of 500K individuals, which underwent the same filtering process as the "Clalit" reference distributions.
#' \cr \cr
#' The list of supported labs can be found below or by running \code{LAB_DETAILS$short_name}.
#'
#' @section reference distribution:
#' It is highly recommended to use \code{ln_download_data} to download the "Clalit" and "UKBB" reference distributions. If you choose not to download the data, the package will use the demo reference distributions included in the package ("Clalit-demo"), which doesn't include all the labs, and has a resolution of 20 quantile bins and therefore may have an error of up to 5 percentiles (0.05), particularly at the edges of the distribution. \cr
#'
#'
#' @section labs:
#' The following labs are supported in the "Clalit" reference (some labs are missing from the UKBB reference): \cr
#'
#'   * WBC
#'   * RBC
#'   * Hemoglobin
#'   * Hematocrit
#'   * Platelets
#'   * MCV
#'   * MCH
#'   * MCHC
#'   * RDW
#'   * MPV
#'   * Large unstained cells, Abs
#'   * Albumin
#'   * Total Cholesterol
#'   * Triglycerides
#'   * BMI
#'   * Iron
#'   * Transferrin
#'   * Ferritin
#'   * PDW
#'   * MPXI
#'   * Total Globulin
#'   * PCT
#'   * HDW
#'   * Fibrinogen
#'   * CH
#'   * Chloride
#'   * Large unstained cells, %
#'   * Macrocytic
#'   * Microcytic
#'   * Hyperchromic
#'   * Hypochromic
#'   * Lymphocytes, Abs
#'   * Lymphocytes, %
#'   * Neutrophils, Abs
#'   * Neutrophils, %
#'   * Monocytes, Abs
#'   * Monocytes, %
#'   * Eosinophils, Abs
#'   * Eosinophils, %
#'   * Basophils, Abs
#'   * Basophils, %
#'   * Microcytic:Hypochromic
#'   * Glucose
#'   * Urea
#'   * Creatinine
#'   * Uric Acid
#'   * Calcium
#'   * Phosphorus
#'   * Total Protein
#'   * HDL Cholesterol
#'   * LDL Cholesterol
#'   * Alk. Phosphatase
#'   * AST
#'   * ALT
#'   * GGT
#'   * LDH
#'   * CPK
#'   * Total Bilirubin
#'   * Direct Bilirubin
#'   * Hemoglobin A1c
#'   * Sodium
#'   * Potassium
#'   * Vitamin D (25-OH)
#'   * Microalbumin:Creatinine
#'   * Urine Creatinine
#'   * Urine Microalbumin
#'   * Non-HDL
#'   * TSH
#'   * T3, Free
#'   * T4, Free
#'   * Blood Pressure, Systolic
#'   * Blood Pressure, Diastolic
#'   * Urine Specific Gravity
#'   * Urine pH
#'   * PT, INR
#'   * PT, sec
#'   * PT, %
#'   * Vitamin B12
#'   * PSA
#'   * ESR
#'   * aPTT, sec
#'   * CRP
#'   * Amylase
#'   * Folic Acid
#'   * Total:HDL
#'   * Hematocrit:Hemoglobin
#'   * Magnesium
#'   * aPTT, ratio
#'   * Indirect Bilirubin
#'   * RDW-SD
#'   * RDW-CV
#'   * LH
#'   * Estradiol
#'
#'
#' @param values a vector of lab values
#' @param age a vector of ages between 20-89 for "Clalit" reference and 35-80 for "UKBB". Can be a single value if all values are the same age.
#' @param sex a vector of either "male" or "female". Can be a single value if all values are the same sex.
#' @param lab the lab name. See \code{LAB_DETAILS$short_name} for a list of available labs.
#' @param units the units of the lab values. See \code{ln_lab_units(lab)} for a list of available units for each lab. If \code{NULL} then the default units (\code{ln_lab_default_units(lab)}) for the lab will be used. If different values have different units then this should be a vector of the same length as \code{values}.
#' @param reference the reference distribution to use. Can be either "Clalit" or "UKBB" or "Clalit-demo". Please download the Clalit and UKBB reference distributions using \code{ln_download_data()}.
#' @param na.rm if \code{TRUE}, then \code{NA} in age, sex or values will be ignored and 'NA' would be returned. Otherwise, an error will be thrown.
#'
#' @return a vector of normalized values. If \code{ln_download_data()} was not run, a lower resolution reference distribution will be used, which can have an error of up to 5 quantiles (0.05). Otherwise, the full reference distribution will be used. You can check if the high resolution data was downloaded using \code{ln_data_downloaded()}. \cr
#' You can force the function to use the lower resolution distribution by setting \code{options(labNorm.use_low_res = TRUE)}. \cr
#' If the quantile information is not available (e.g. "Estradiol" for male patients, various labs which are not available in the UKBB data), then the function will return \code{NA}.
#'
#' @examples
#' \donttest{
#' # Normalize Hemoglobin values to age and sex
#' hemoglobin_data$quantile <- ln_normalize(
#'     hemoglobin_data$value,
#'     hemoglobin_data$age,
#'     hemoglobin_data$sex,
#'     "Hemoglobin"
#' )
#'
#' # plot the quantiles vs values for age 50-60
#' library(ggplot2)
#' library(dplyr)
#' hemoglobin_data %>%
#'     filter(age >= 50 & age <= 60) %>%
#'     ggplot(aes(x = value, y = quantile, color = sex)) +
#'     geom_point() +
#'     theme_classic()
#'
#' # Different units
#' hemoglobin_diff_units <- hemoglobin_data
#' hemoglobin_diff_units$value <- hemoglobin_diff_units$value * 0.1
#' hemoglobin_diff_units$quantile <- ln_normalize(
#'     hemoglobin_data$value,
#'     hemoglobin_data$age,
#'     hemoglobin_data$sex,
#'     "Hemoglobin",
#'     "mg/mL"
#' )
#'
#' # Multiple units
#' creatinine_diff_units <- creatinine_data
#' creatinine_diff_units$value <- c(
#'     creatinine_diff_units$value[1:500] * 0.011312,
#'     creatinine_diff_units$value[501:1000] * 11.312
#' )
#' creatinine_diff_units$quantile <- ln_normalize(
#'     creatinine_diff_units$value,
#'     creatinine_diff_units$age,
#'     creatinine_diff_units$sex,
#'     "Creatinine",
#'     c(rep("umol/L", 500), rep("mmol/L", 500))
#' )
#'
#' # Use UKBB as reference
#' hemoglobin_data_ukbb <- hemoglobin_data %>% filter(age >= 35 & age <= 80)
#' hemoglobin_data_ukbb$quantile_ukbb <- ln_normalize(
#'     hemoglobin_data_ukbb$value,
#'     hemoglobin_data_ukbb$age,
#'     hemoglobin_data_ukbb$sex,
#'     "Hemoglobin",
#'     reference = "UKBB"
#' )
#'
#' # plot UKBB vs Clalit
#' hemoglobin_data_ukbb %>%
#'     filter(age >= 50 & age <= 60) %>%
#'     ggplot(aes(x = quantile, y = quantile_ukbb, color = sex)) +
#'     geom_point() +
#'     geom_abline() +
#'     theme_classic()
#' }
#'
#' # examples on the demo data
#' \dontshow{
#' hemoglobin_data$quantile <- ln_normalize(
#'     hemoglobin_data$value,
#'     hemoglobin_data$age,
#'     hemoglobin_data$sex,
#'     "Hemoglobin",
#'     reference = "Clalit-demo"
#' )
#' }
#'
#' @export
ln_normalize <- function(values, age, sex, lab, units = NULL, reference = "Clalit", na.rm = FALSE) {
    if (na.rm == FALSE) {
        validate_na(age, sex, values)
    }

    # check inputs

    lab_info <- get_lab_info(lab)

    if (length(age) == 1) {
        age <- rep(age, length(values))
    }

    if (length(sex) == 1) {
        sex <- rep(sex, length(values))
    }

    age <- floor(age)

    validate_age_and_sex(age, sex, reference)

    if (length(values) != length(age)) {
        cli::cli_abort("The length of {.field values} must be the same as the length of {.field age}.")
    }

    if (is.null(units)) {
        units <- lab_info$default_units
    }

    validate_units(units, lab)

    # convert units if needed
    values <- ln_convert_units(values, units, lab)

    f_good <- !is.na(values) & !is.na(age) & !is.na(sex) & !is.na(units)
    normalized <- rep(NA, length(values[f_good]))
    good_values <- values[f_good]

    # normalize
    ages <- unique(age[f_good])
    sexes <- unique(sex[f_good])


    if (reference %in% c("Clalit", "UKBB")) {
        if (!has_reference(reference)) {
            ln_download_data()
        }
    }

    for (cur_age in ages) {
        for (cur_sex in sexes) {
            if (!age_in_range(cur_age, reference)) {
                normalized[age[f_good] == cur_age & sex[f_good] == cur_sex] <- NA
            } else {
                func <- get_norm_func(lab, cur_age, cur_sex, reference)

                # test if func is a function
                cur_values <- good_values[age[f_good] == cur_age & sex[f_good] == cur_sex]
                if (is.function(func)) {
                    normalized[age[f_good] == cur_age & sex[f_good] == cur_sex] <- func(cur_values)
                } else {
                    normalized[age[f_good] == cur_age & sex[f_good] == cur_sex] <- NA
                }
            }
        }
    }

    normalized_full <- rep(NA, length(values))
    normalized_full[f_good] <- normalized

    return(normalized_full)
}

#' Normalize multiple labs for age and sex
#'
#' @param labs_df a data frame with the columns "value", "age", "sex", "units", and "lab". The "lab" column should be a vector with the lab name per row. See \code{ln_normalize} for details on the other columns.
#'
#' @examples
#' library(dplyr)
#' multi_labs_df <- bind_rows(
#'     hemoglobin_data %>% mutate(lab = "Hemoglobin"),
#'     creatinine_data %>% mutate(lab = "Creatinine")
#' )
#'
#' \donttest{
#' multi_labs_df$quantile <- ln_normalize_multi(multi_labs_df)
#' }
#'
#' # on the demo data
#' \dontshow{
#' multi_labs_df$quantile <- ln_normalize_multi(multi_labs_df, reference = "Clalit-demo")
#' }
#'
#' head(multi_labs_df)
#'
#' @rdname ln_normalize
#' @export
ln_normalize_multi <- function(labs_df, reference = "Clalit", na.rm = FALSE) {
    # validate that labs_df has the right columns
    required_columns <- c("value", "age", "sex", "lab")
    purrr::walk(required_columns, function(col) {
        if (!(col %in% colnames(labs_df))) {
            cli::cli_abort("{.field labs_df} must have a column named {.field {col}}")
        }
    })

    labs <- unique(labs_df$lab)
    normalized <- rep(NA, nrow(labs_df))
    for (lab in labs) {
        lab_df <- labs_df[labs_df$lab == lab, ]
        normalized[labs_df$lab == lab] <- ln_normalize(
            lab_df$value,
            lab_df$age,
            lab_df$sex,
            lab,
            lab_df$units, # units can be NULL
            reference = reference,
            na.rm = na.rm
        )
    }

    return(normalized)
}
