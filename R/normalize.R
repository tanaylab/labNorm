#' Normalize lab values to age and gender
#'
#' @description Normalize standard laboratory measurements (e.g. hemoglobin, cholesterol levels) according to age and gender, based on the algorithms described in "Personalized lab test models to quantify disease potentials in healthy individuals" <doi:10.1038/s41591-021-01468-6>. \cr
#' The reference distribution used in this function are based on 2.1B lab measurements taken from 2.8M individuals between 2002-2019, filtered to exclude severe chronic diseases and medication effects. The resulting normalized value is a quantile between 0 and 1, representing the value's position in the reference distribution.
#' \cr
#' The list of supported labs can be found below or by running \code{LAB_INFO$short_name}.
#'
#' @section reference distribution:
#' The reference distribution used in the function has a resolution of 20 quantile bins and therefore may have an error of up to 5 quantiles (0.05), particularly at the edges of the distribution. The full reference distributions can be used after downloading the data using the \code{ln_download_data()} function. \cr
#' The function would first look for the downloaded values at \code{getOption("labNorm.dir")}, then at \code{rappdirs::user_data_dir("Labnorm")}, then at the current working directory. If the data is not found, the lower resolution distribution. \cr
#' You can check if the high resolution data was downloaded using \code{ln_is_high_res()}.
#'
#'
#' @section labs:
#' The following labs are supported:
#' \describe{
#'   \item{WBC}{White Blood Cells (WBC)}
#'   \item{RBC}{Red Blood Cells (RBC)}
#'   \item{Hemoglobin}{}
#'   \item{Hematocrit}{}
#'   \item{Platelets}{}
#'   \item{MCV}{Mean Corpuscular Volume (MCV)}
#'   \item{MCH}{Mean Corpuscular Hemoglobin (MCH)}
#'   \item{MCHC}{Mean Corpuscular Hemoglobin Concentration (MCHC)}
#'   \item{RDW}{Red cell Distribution Width (RDW)}
#'   \item{MPV}{Mean Platelet Volume (MPV)}
#'   \item{Albumin}{}
#'   \item{Total Cholesterol}{}
#'   \item{Triglycerides}{}
#'   \item{BMI}{Body Mass Index (BMI)}
#'   \item{Iron}{}
#'   \item{Transferrin}{}
#'   \item{Ferritin}{}
#'   \item{Total Globulin}{}
#'   \item{Fibrinogen}{}
#'   \item{Lymphocytes, Abs}{Lymphocytes, Absolute}
#'   \item{Lymphocytes, %}{}
#'   \item{Neutrophils, Abs}{Neutrophils, Absolute}
#'   \item{Neutrophils, %}{}
#'   \item{Monocytes, Abs}{Monocytes, Absolute}
#'   \item{Monocytes, %}{}
#'   \item{Eosinophils, Abs}{Eosinophils, Absolute}
#'   \item{Eosinophils, %}{}
#'   \item{Basophils, Abs}{Basophils, Absolute}
#'   \item{Basophils, %}{}
#'   \item{Glucose}{Glucose, Fasting}
#'   \item{Urea}{}
#'   \item{Creatinine}{}
#'   \item{Uric Acid}{}
#'   \item{Calcium}{}
#'   \item{Phosphorus}{}
#'   \item{Total Protein}{}
#'   \item{HDL Cholesterol}{}
#'   \item{LDL Cholesterol}{}
#'   \item{Alk. Phosphatase}{Alkaline Phosphatase (AP)}
#'   \item{AST}{Aspartate aminotransferase (AST)}
#'   \item{ALT}{Alanine aminotransferase (ALT)}
#'   \item{GGT}{Gamma-glutamyltransferase (GGT)}
#'   \item{LDH}{Lactate dehydrogenase (LDH)}
#'   \item{CPK}{Creatine Phospho-Kinase (CPK)}
#'   \item{Total Bilirubin}{}
#'   \item{Direct Bilirubin}{}
#'   \item{Hemoglobin A1c}{}
#'   \item{Sodium}{}
#'   \item{Potassium}{}
#'   \item{Vitamin D (25-OH)}{Vitamin D, 25-Hydroxy}
#'   \item{TSH}{Thyroid-Stimulating Hormone (TSH)}
#'   \item{T3, Free}{Triiodothyronine (T3), Free}
#'   \item{T4, Free}{Thyroxine (T4), Free (Direct)}
#'   \item{Blood Pressure, Systolic}{Systolic Blood Pressure}
#'   \item{Blood Pressure, Diastolic}{Diastolic Blood Pressure}
#'   \item{Vitamin B12}{}
#'   \item{PSA}{Prostate Specific Antigen (PSA)}
#'   \item{ESR}{Erythrocyte Sedimentation Rate (ESR)}
#'   \item{CRP}{C-Reactive Protein, Quant (CRP)}
#'   \item{Amylase}{}
#'   \item{Folic Acid}{}
#'   \item{Magnesium}{}
#'   \item{Indirect Bilirubin}{}
#'   \item{LH}{Luteinizing Hormone (LH)}
#'   \item{Estradiol}{Estradiol (E2)}
#' }
#'
#' @param values a vector of lab values
#' @param age a vector of ages between (20-99). Can be a single value if all values are the same age.
#' @param sex a vector of either "male" or "female". Can be a single value if all values are the same sex.
#' @param lab the lab name. See \code{LAB_INFO$short_name} for a list of available labs.
#' @param units the units of the lab values. See \code{ln_lab_units(lab)} for a list of available units for each lab. If \code{NULL} then the default units (\code{LAB_INFO$default_units}) for the lab will be used. If different values have different units then this should be a vector of the same length as \code{values}.
#'
#' @return a vector of normalized values. If \code{ln_download_data()} was not run, a lower resolution reference distribution will be used, which can have an error of up to 5 quantiles (0.05). Otherwise, the full reference distribution will be used. You can check if the high resolution data was downloaded using \code{ln_is_high_res()}.
#'
#' @examples
#'
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
#' @export
ln_normalize <- function(values, age, sex, lab, units = NULL) {
    # check inputs

    lab_info <- get_lab_info(lab)

    if (length(age) == 1) {
        age <- rep(age, length(values))
    }

    if (length(sex) == 1) {
        sex <- rep(sex, length(values))
    }

    validate_age_and_sex(age, sex)

    if (length(values) != length(age)) {
        cli::cli_abort("The length of {.field values} must be the same as the length of {.field age}.")
    }

    if (is.null(units)) {
        units <- LAB_INFO$default_units[LAB_INFO$short_name == lab]
    }

    validate_units(units, lab)

    # convert units if needed
    values <- ln_convert_units(values, units, lab)

    # normalize
    ages <- unique(age)
    sexes <- unique(sex)
    normalized <- rep(NA, length(values))

    quantiles <- get_quantiles()

    for (cur_age in ages) {
        for (cur_sex in sexes) {
            func <- quantiles[[lab]][[paste0(cur_age, ".", cur_sex)]]
            cur_values <- values[age == cur_age & sex == cur_sex]
            normalized[age == cur_age & sex == cur_sex] <- func(cur_values)
        }
    }

    return(normalized)
}

#' Normalize multiple labs for age and gender to age and gender
#'
#' @param df a data frame with the columns "value", "age", "sex", "units", and "lab". The "lab" column should be a vector with the lab name per row. See \code{ln_normalize} for details on the other columns.
#'
#' @examples
#' library(dplyr)
#' multi_labs_df <- bind_rows(
#'     hemoglobin_data %>% mutate(lab = "Hemoglobin"),
#'     creatinine_data %>% mutate(lab = "Creatinine")
#' )
#'
#' multi_labs_df$quantile <- ln_normalize_multi(multi_labs_df)
#'
#' head(multi_labs_df)
#'
#' @rdname ln_normalize
#' @export
ln_normalize_multi <- function(df) {
    # validate that df has the right columns
    required_columns <- c("value", "age", "sex", "lab")
    purrr::walk(required_columns, function(col) {
        if (!(col %in% colnames(df))) {
            cli::cli_abort("{.field df} must have a column named {.field {col}}")
        }
    })

    labs <- unique(df$lab)
    normalized <- rep(NA, nrow(df))
    for (lab in labs) {
        lab_df <- df[df$lab == lab, ]
        normalized[df$lab == lab] <- ln_normalize(
            lab_df$value,
            lab_df$age,
            lab_df$sex,
            lab,
            lab_df$units # units can be NULL
        )
    }

    return(normalized)
}
