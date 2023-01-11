test_that("ln_normalize returns values", {
    expect_type(ln_normalize(c(1, 2, 3), c(25, 30, 35), c("male", "female", "male"), "Hemoglobin", reference = "Clalit-demo"), "double")
})

test_that("ln_normalize throws errors on invalid inputs", {
    expect_error(ln_normalize(c(1, 2, 3), c(25, 30), c("male", "female", "male"), "Hemoglobin"))
    expect_error(ln_normalize(c(1, 2, 3), c(25, 30, 35), c("male", "female"), "Hemoglobin"))
    expect_error(ln_normalize(c(1, 2, 3), c(25, 30, 35), c("male", "female", "male"), "InvalidLab"))
    expect_error(ln_normalize(c(1, 2, 3), c(25, 30, 35), c("male", "female", "male"), "Hemoglobin", "InvalidUnits"))
    expect_error(ln_normalize(c(1, 2, 3), c(25, 30, 35, 40), c("male", "female", "male"), "Hemoglobin"))
    expect_error(ln_normalize(c(1, 2, 3), c(25, 30), c("male", "female", "male", "female"), "Hemoglobin"))
})

test_that("ln_normalize handles age and sex vectors of length 1", {
    expect_type(ln_normalize(c(1, 2, 3), 25, "male", "Hemoglobin", reference = "Clalit-demo"), "double")
    expect_type(ln_normalize(c(1, 2, 3), c(25), "male", "Hemoglobin", reference = "Clalit-demo"), "double")
    expect_type(ln_normalize(c(1, 2, 3), 25, c("male"), "Hemoglobin", reference = "Clalit-demo"), "double")
    expect_type(ln_normalize(c(1, 2, 3), c(25), c("male"), "Hemoglobin", reference = "Clalit-demo"), "double")
})

test_that("ln_normalize works", {
    hemoglobin_50 <- hemoglobin_data %>%
        filter(age == 50, sex == "male")

    q <- ln_normalize(
        hemoglobin_50$value,
        hemoglobin_50$age,
        hemoglobin_50$sex,
        "Hemoglobin",
        reference = "Clalit-demo"
    )

    expect_equal(q, LAB_QUANTILES[["Hemoglobin"]][["50.male"]](hemoglobin_50$value))
})

test_that("ln_normalize returns NA when age is out of range", {
    expect_warning(q <- ln_normalize(c(1:10), 5:14, "male", "Hemoglobin", reference = "Clalit-demo"))
    expect_true(all(is.na(q)))
})

test_that("normalization works with different units", {
    hemoglobin_50 <- hemoglobin_data %>%
        filter(age == 50, sex == "male")

    hemoglobin_diff_units <- hemoglobin_50[1:2, ]

    hemoglobin_diff_units$value[1] <- hemoglobin_diff_units$value[1] * 10
    hemoglobin_diff_units$value[2] <- hemoglobin_diff_units$value[2] / 1.61

    q <- ln_normalize(
        hemoglobin_diff_units$value,
        hemoglobin_diff_units$age,
        hemoglobin_diff_units$sex,
        "Hemoglobin",
        c("mg/mL", "mmol/L"),
        reference = "Clalit-demo"
    )

    expect_equal(q, LAB_QUANTILES[["Hemoglobin"]][["50.male"]](hemoglobin_50$value[1:2]))
})

test_that("ln_normalize works with high resolution", {
    skip_on_cran()

    hemoglobin_50 <- hemoglobin_data %>%
        filter(age == 50, sex == "male")

    withr::defer(pkgenv$yesno2 <- yesno::yesno2)
    pkgenv$yesno2 <- function(prompt) TRUE

    q <- ln_normalize(
        hemoglobin_50$value,
        hemoglobin_50$age,
        hemoglobin_50$sex,
        "Hemoglobin",
        reference = "Clalit"
    )

    expect_equal(q, pkgenv$Clalit[["Hemoglobin"]][["50.male"]](hemoglobin_50$value))
})

test_that("ln_normalize works with UKBB", {
    skip_on_cran()

    hemoglobin_50 <- hemoglobin_data %>%
        filter(age == 50, sex == "male")

    withr::defer(pkgenv$yesno2 <- yesno::yesno2)
    pkgenv$yesno2 <- function(prompt) TRUE

    q <- ln_normalize(
        hemoglobin_50$value,
        hemoglobin_50$age,
        hemoglobin_50$sex,
        "Hemoglobin",
        reference = "UKBB"
    )

    expect_equal(q, pkgenv$UKBB[["Hemoglobin"]][["[50,55).male"]](hemoglobin_50$value))
})
