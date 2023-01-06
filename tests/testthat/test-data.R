options(labNorm.use_low_res = TRUE)

test_that("LAB_INFO and LAB_QUANTILES match", {
    expect_true(all.equal(LAB_INFO$short_name, names(LAB_QUANTILES)))
})

test_that("LAB_QUANTILES are valid", {
    field_names <- expand.grid(sex = c("female", "male"), age = 20:99) %>%
        mutate(s = paste0(age, ".", sex)) %>%
        pull(s)
    purrr::map(LAB_QUANTILES, function(x) {
        expect_true(all(names(x) %in% field_names))
        expect_true(all(field_names %in% names(x)))
        expect_true(all(sapply(x, is.function)))
    })
})

test_that("all default units are in units", {
    purrr::map2(LAB_INFO$default_units, LAB_INFO$units, function(default_units, units) {
        expect_true(default_units %in% units)
    })
})

test_that("UNITS_CONVERSION and LAB_INFO are compatible", {
    purrr::map2(UNITS_CONVERSION, LAB_INFO$units, function(conversion, units) {
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
    expect_true(all(names(LAB_QUANTILES) %in% LAB_INFO$short_name))
    expect_true(all(LAB_INFO$short_name %in% names(LAB_QUANTILES)))
})

test_that("all labs have units conversion", {
    expect_true(all(names(UNITS_CONVERSION) %in% LAB_INFO$short_name))
    expect_true(all(LAB_INFO$short_name %in% names(UNITS_CONVERSION)))
})
