test_that("LAB_DETAILS and LAB_QUANTILES match", {
    expect_true(all.equal(LAB_DETAILS$short_name, names(LAB_QUANTILES)))
})

test_that("LAB_QUANTILES are valid", {
    field_names <- expand.grid(sex = c("female", "male"), age = 20:89) %>%
        mutate(s = paste0(age, ".", sex)) %>%
        pull(s)

    purrr::map(LAB_QUANTILES, function(x) {
        expect_true(all(names(x) %in% field_names))
        expect_true(all(field_names %in% names(x)))
        purrr::walk(x, function(y) {
            if (is.function(y)) {
                expect_true(is.function(y))
            } else {
                expect_true(is.na(y))
            }
        })
    })
})

test_that("Clalit data is valid", {
    skip_on_cran()
    if (!ln_data_downloaded()) {
        pkgenv$yesno2 <- function(prompt) FALSE
        withr::defer(pkgenv$yesno2 <- yesno::yesno2)
        ln_download_data()
    }
    field_names <- expand.grid(sex = c("female", "male"), age = 20:89) %>%
        mutate(s = paste0(age, ".", sex)) %>%
        pull(s)

    quantiles <- pkgenv[["Clalit"]]

    purrr::map(quantiles, function(x) {
        expect_true(all(names(x) %in% field_names))
        expect_true(all(field_names %in% names(x)))
        purrr::walk(x, function(y) {
            if (is.function(y)) {
                expect_true(is.function(y))
            } else {
                expect_true(is.na(y))
            }
        })
    })
})

test_that("UKBB data is valid", {
    skip_on_cran()
    if (!ln_data_downloaded()) {
        pkgenv$yesno2 <- function(prompt) FALSE
        withr::defer(pkgenv$yesno2 <- yesno::yesno2)
        ln_download_data()
    }
    field_names <- expand.grid(sex = c("male", "female"), age = levels(cut(35:80, seq(35, 80, 5), right = FALSE))) %>%
        mutate(s = paste0(age, ".", sex)) %>%
        pull(s)

    quantiles <- pkgenv[["UKBB"]]

    purrr::map(quantiles, function(x) {
        expect_true(all(names(x) %in% field_names))
        expect_true(all(field_names %in% names(x)))
        purrr::walk(x, function(y) {
            if (is.function(y)) {
                expect_true(is.function(y))
            } else {
                expect_true(is.na(y))
            }
        })
    })
})

test_that("all default units are in units", {
    purrr::map2(LAB_DETAILS$default_units, LAB_DETAILS$units, function(default_units, units) {
        expect_true(default_units %in% units)
    })
})

test_that("UNITS_CONVERSION and LAB_DETAILS are compatible", {
    purrr::map2(UNITS_CONVERSION, LAB_DETAILS$units, function(conversion, units) {
        expect_true(all(names(conversion) %in% units))
        expect_true(all(units %in% names(conversion)))
    })
})

test_that("UNITS_CONVERSION are valid", {
    purrr::map(UNITS_CONVERSION, function(x) {
        expect_true(all(sapply(x, is.function)))
    })
})

test_that("all labs have quantiles", {
    expect_true(all(names(LAB_QUANTILES) %in% LAB_DETAILS$short_name))
    expect_true(all(LAB_DETAILS$short_name %in% names(LAB_QUANTILES)))
})

test_that("all labs have units conversion", {
    expect_true(all(names(UNITS_CONVERSION) %in% LAB_DETAILS$short_name))
    expect_true(all(LAB_DETAILS$short_name %in% names(UNITS_CONVERSION)))
})
