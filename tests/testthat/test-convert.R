test_that("unit conversion works", {
    # emulate a dataset with different units
    hemoglobin_diff_units <- hemoglobin_data
    # first 500 values will be in mg/ML
    hemoglobin_diff_units$value[1:500] <- hemoglobin_diff_units$value[1:500] * 10
    # last 500 values will be in mmol/L
    hemoglobin_diff_units$value[501:1000] <- hemoglobin_diff_units$value[501:1000] / 1.61

    converted <- ln_convert_units(
        hemoglobin_diff_units$value,
        c(rep("mg/mL", 500), rep("mmol/L", 500)),
        "Hemoglobin"
    )

    expect_equal(converted[1:500], hemoglobin_data$value[1:500])
    expect_equal(converted[501:1000], hemoglobin_data$value[501:1000])
})

test_that("unit conversion works with non-default units", {
    # emulate a dataset with different units
    hemoglobin_diff_units <- hemoglobin_data
    # all values will be in mg/ML
    hemoglobin_diff_units$value <- hemoglobin_diff_units$value * 10

    converted <- ln_convert_units(
        hemoglobin_diff_units$value,
        "mg/mL",
        "Hemoglobin"
    )

    expect_equal(converted, hemoglobin_data$value)
})

test_that("unit conversion works with default units", {
    # emulate a dataset with different units
    hemoglobin_diff_units <- hemoglobin_data
    # all values will be in the default units
    hemoglobin_diff_units$value <- hemoglobin_diff_units$value

    converted <- ln_convert_units(
        hemoglobin_diff_units$value,
        "g/dL", # assuming "g/dL" is the default units for Hemoglobin
        "Hemoglobin"
    )

    expect_equal(converted, hemoglobin_data$value)
})

test_that("unit conversion works with mixed units", {
    # emulate a dataset with different units
    hemoglobin_diff_units <- hemoglobin_data
    # first 500 values will be in mg/ML
    hemoglobin_diff_units$value[1:500] <- hemoglobin_diff_units$value[1:500] * 10
    # last 500 values will be in mmol/L
    hemoglobin_diff_units$value[501:1000] <- hemoglobin_diff_units$value[501:1000] / 1.61

    converted <- ln_convert_units(
        hemoglobin_diff_units$value,
        c(rep("mg/mL", 500), rep("mmol/L", 500)),
        "Hemoglobin"
    )

    expect_equal(converted[1:500], hemoglobin_data$value[1:500])
    expect_equal(converted[501:1000], hemoglobin_data$value[501:1000])
})

test_that("unit conversion works with edge cases", {
    # test with empty input values
    converted <- ln_convert_units(
        numeric(),
        "g/dL",
        "Hemoglobin"
    )
    expect_equal(converted, numeric(0))

    # test with units and values of different lengths
    expect_error(ln_convert_units(
        c(1, 2, 3),
        c("g/dL", "g/L"),
        "Hemoglobin"
    ))
})

test_that("unit conversion handles invalid input", {
    # test with invalid units
    expect_error(ln_convert_units(
        c(1, 2, 3),
        "mg/L", # assuming "mg/L" is not a valid unit for Hemoglobin
        "Hemoglobin"
    ))

    # test with invalid lab
    expect_error(ln_convert_units(
        c(1, 2, 3),
        "g/dL",
        "savta"
    ))
})

test_that("ln_lab_units() throws an error if the lab parameter is not valid", {
    expect_error(ln_lab_units("InvalidLab"))
})

test_that("ln_lab_units() returns a vector of available units for the lab", {
    units <- ln_lab_units("Hemoglobin")
    expect_true(is.character(units))
    expect_true(length(units) > 0)
})
