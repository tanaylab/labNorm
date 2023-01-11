setup_test <- function(...) {
    default_dir <- rappdirs::user_data_dir("labNorm")
    dir.create(default_dir, recursive = TRUE, showWarnings = FALSE)
    if (file.exists(file.path(default_dir, "Clalit.rds"))) {
        file.remove(file.path(default_dir, "Clalit.rds"))
    }
    if (file.exists(file.path(default_dir, "UKBB.rds"))) {
        file.remove(file.path(default_dir, "UKBB.rds"))
    }

    withr::defer(pkgenv$yesno2 <- yesno::yesno2)
    pkgenv$yesno2 <- function(prompt) FALSE

    # Test function
    ln_download_data(...)

    return(default_dir)
}

# Test that the function downloads the data file to a temporary directory if the user does not approve or if `dir` is not provided.
test_that("ln_download_data downloads to temp dir if not approved or if dir not provided", {
    skip_on_cran()

    # Set up test
    default_dir <- setup_test()

    # Check that the data file was downloaded to a temporary directory
    expect_true(file.exists(file.path(getOption("labNorm.dir"), "Clalit.rds")))
    expect_true(file.exists(file.path(getOption("labNorm.dir"), "UKBB.rds")))

    # Check that the quantile data was read and stored correctly
    expect_equal(pkgenv$Clalit, readRDS(file.path(getOption("labNorm.dir"), "Clalit.rds")))
    expect_equal(pkgenv$UKBB, readRDS(file.path(getOption("labNorm.dir"), "UKBB.rds")))

    pkgenv$yesno2 <- yesno::yesno2 # Reset the function
})
