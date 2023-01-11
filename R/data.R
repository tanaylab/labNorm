#' Available lab names
#'
#' Names of the labs available in the package.
#'
#' @format ## `LAB_DETAILS`
#' A data frame with 95 rows and 4 columns:
#' \describe{
#'   \item{short_name}{Short lab name}
#'   \item{long_name}{Long lab name}
#'   \item{units}{a list column with all the units available for the lab}
#'   \item{default_units}{the default units for the lab}
#'   \item{low_male,high_male,low_female,high_female}{the reference ranges for the lab, taken from the American Board of Internal Medicine. Can be NA if the lab does not have reference ranges.}
#' }
#' @source  American Board of Internal Medicine. ABIM Laboratory Test Reference Ranges — July 2021. <https://www.abim.org/~/media/ABIM%20Public/Files/pdf/exam/laboratory-reference-ranges.pdf> (2021).
#' @examples
#' head(LAB_DETAILS)
"LAB_DETAILS"

#' Example values of Hemoglobin and Creatinine
#'
#' Example datasets of Hemoglobin and Creatinine values for testing
#'
#' @format ## `hemoglobin_data` `creatinine_data`
#' A data frame with 1000 rows and 3 columns:
#' \describe{
#'   \item{age}{age of the patient}
#'   \item{sex}{sex of the patient}
#'   \item{value}{the lab value for the patient, in the default units for the lab}
#' }
#' @examples
#' head(hemoglobin_data)
#' head(creatinine_data)
"hemoglobin_data"

#' @rdname hemoglobin_data
"creatinine_data"
