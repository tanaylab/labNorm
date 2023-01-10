setup_test <- function(...) {
    default_dir <- rappdirs::user_data_dir("labNorm")
    dir.create(default_dir, recursive = TRUE, showWarnings = FALSE)
    if (file.exists(file.path(default_dir, "high_res_labs.rds"))) {
        file.remove(file.path(default_dir, "high_res_labs.rds"))
    }

    withr::defer(pkgenv$yesno2 <- yesno::yesno2)
    pkgenv$yesno2 <- function(prompt) TRUE

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
    expect_true(file.exists(file.path(getOption("labNorm.dir"), "high_res_labs.rds")))

    # Check that the quantile data was read and stored correctly
    expect_equal(pkgenv$quantiles, readRDS(file.path(getOption("labNorm.dir"), "high_res_labs.rds")))

    pkgenv$yesno2 <- yesno::yesno2 # Reset the function
})

# Test that the function sets the `labNorm.dir` option correctly.
test_that("ln_download_data sets labNorm.dir option correctly", {
    skip_on_cran()

    # Set up test
    default_dir <- setup_test(load = FALSE)

    # Check that the `labNorm.dir` option was set correctly
    expect_equal(getOption("labNorm.dir"), default_dir)

    # Clean up
    unlink(default_dir, recursive = TRUE, force = TRUE)
    options(labNorm.dir = NULL)
})
