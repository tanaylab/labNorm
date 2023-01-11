#' Download high-resolution reference distributions
#'
#' @description The data is downloaded to the directory specified by the \code{dir} parameter. Note
#' that if you specified a directory different from the default, you will need to set \code{options(labNorm.dir = dir)} in order for the package to use the downloaded data in future sessions.
#' \cr
#' Default directories are:
#'
#' * Unix: ~/.local/share/LabNorm
#' * Mac OS X: `~/Library/Application Support/LabNorm`
#' * Win XP (not roaming): `C:\\Documents and Settings\\<username>\\Data\\<AppAuthor>\\LabNorm`
#' * Win XP (roaming): `C:\\Documents and Settings\\<username>\\Local Settings\\Data\\<AppAuthor>\\LabNorm`
#' * Win 7 (not roaming): `C:\\Users\\<username>\\AppData\\Local\\<AppAuthor>\\LabNorm`
#' * Win 7 (roaming): `C:\\Users\\<username>\\AppData\\Roaming\\<AppAuthor>\\LabNorm`
#'
#'
#' @param dir the directory to download the data to. If \code{NULL} and the user approves, the data will be downloaded to the package directory, using \code{rappdirs::user_data_dir("labnorm")}, otherwise - a temporary directory would be used.
#' @param load load the data after downloading it.
#'
#' @return None.
#'
#' @examples
#' \donttest{
#' ln_download_data()
#' }
#'
#' @export
ln_download_data <- function(dir = NULL, load = TRUE) {
    default_dir <- FALSE
    if (is.null(dir)) {
        dir <- rappdirs::user_data_dir("labNorm")
        # ask the user if they want to download to the specified directory
        if (interactive() && pkgenv$yesno2(glue::glue("Would you like to use the default directory {dir}?\n(if you choose 'No', the file will be downloaded to a temporary directory)"))) {
            # create the directory if it doesn't exist
            if (!dir.exists(dir)) {
                dir.create(dir, recursive = TRUE)
            }
            default_dir <- TRUE
        } else {
            dir <- tempdir()
            cli::cli_alert("Downloading to a temporary directory {.file {dir}}.")
            default_dir <- TRUE
        }
    }

    withr::local_options(timeout = 2 * 60 * 60)

    tryCatch(
        {
            download.file(
                "https://labnorm.s3.eu-west-1.amazonaws.com/Clalit.rds",
                file.path(dir, "Clalit.rds")
            )
        },
        error = function(e) {
            cli::cli_abort("There was an error downloading the data. Please check your internet connection and try again.")
        }
    )

    tryCatch(
        {
            download.file(
                "https://labnorm.s3.eu-west-1.amazonaws.com/UKBB.rds",
                file.path(dir, "UKBB.rds")
            )
        },
        error = function(e) {
            cli::cli_abort("There was an error downloading the data. Please check your internet connection and try again.")
        }
    )

    cli::cli_alert_success("Data downloaded succesfully to {.field {dir}}.")

    if (!default_dir) {
        cli::cli_alert_info("The data was downloaded to {.field {dir}}. You will need to set {.code options(labNorm.dir = '{dir}')} in order for the package to use the downloaded data in future sessions.")
    }

    options(labNorm.dir = dir)

    if (load) {
        cli::cli_alert("Loading the data into the environment.")
        pkgenv$Clalit <- readRDS(file.path(dir, "Clalit.rds"))
        pkgenv$UKBB <- readRDS(file.path(dir, "UKBB.rds"))
    }
}

#' Check if data was downloaded
#'
#' @return True if the data was downloaded, false otherwise.
#' @export
#' @examples
#' ln_data_downloaded()
#'
#' @rdname ln_download_data
ln_data_downloaded <- function() {
    if (!has_reference("Clalit") || !has_reference("UKBB")) {
        return(FALSE)
    }
    return(TRUE)
}
