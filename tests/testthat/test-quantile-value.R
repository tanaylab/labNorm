test_that("quantile less than 0 throws error", {
    expect_error(ln_quantile_value(-0.01, 50, "male", "WBC"))
})

test_that("quantile greater than 1 throws error", {
    expect_error(ln_quantile_value(1.01, 50, "male", "WBC"))
})

test_that("invalid sex throws error", {
    expect_error(ln_quantile_value(c(0.25, 0.5, 0.75), 50, "other", "WBC"))
})

test_that("invalide age throws warning and returns NA", {
    expect_warning(a <- ln_quantile_value(c(0.25, 0.5, 0.75), -10, "male", "WBC", reference = "Clalit-demo"))
    expect_true(all(is.na(a$value)))
})

test_that("correct number of rows returned for multiple quantiles and ages/sexes", {
    res <- ln_quantile_value(c(0, 0.25, 0.5, 0.75, 1), c(50, 60), c("male", "female"), "WBC", reference = "Clalit-demo")
    expect_equal(nrow(res), 20) # 5 quantiles * 2 ages * 2 sexes
})

test_that("correct lab values are returned for single quantile", {
    res <- ln_quantile_value(0.5, 50, "male", "WBC", reference = "Clalit-demo")
    expect_equal(res$value[1], 7.182971, tolerance = 1e-5)
})

test_that("NA is returned when there is no reference data", {
    res <- ln_quantile_value(0.5, 50, "male", "Estradiol", reference = "Clalit-demo")
    expect_true(is.na(res$value[1]))
})

test_that("ln_quantile_value() throws a warning if the quantiles parameter is not unique", {
    expect_warning(ln_quantile_value(c(0.5, 0.5), 25, "male", "Hemoglobin", reference = "Clalit-demo"))
})

test_that("ln_quantile_value() throws a warning if the age parameter is not unique", {
    expect_warning(ln_quantile_value(c(0.5, 0.6), c(25, 25), "male", "Hemoglobin", reference = "Clalit-demo"))
})

test_that("ln_quantile_value() throws an error if the sex parameter is not 'male' or 'female'", {
    expect_error(ln_quantile_value(c(0.5), 25, "invalid", "Hemoglobin", reference = "Clalit-demo"))
    expect_error(ln_quantile_value(c(0.5), 25, rep("male", 3), "Hemoglobin", reference = "Clalit-demo"))
})

test_that("correct lab values are returned for multiple quantiles", {
    res <- ln_quantile_value(c(0.1, 0.9), 50, "male", "WBC", reference = "Clalit-demo")
    expect_equal(res$value[1], 5.08879, tolerance = 1e-5)
    expect_equal(res$value[2], 10.38806, tolerance = 1e-5)
})

test_that("correct units are returned", {
    res <- ln_quantile_value(0.5, 50, "male", "WBC", reference = "Clalit-demo")
    expect_equal(res$unit[1], "x10E3/uL")
})

test_that("correct lab name is returned", {
    res <- ln_quantile_value(0.5, 50, "male", "WBC", reference = "Clalit-demo")
    expect_equal(res$lab[1], "WBC")
})

test_that("NAs are returned for edge quantiles", {
    res <- ln_quantile_value(c(0, 1), 50, "male", "WBC", reference = "Clalit-demo")
    expect_equal(res$value, as.numeric(c(NA, NA)))
})

test_that("age is floored to the nearest integer", {
    res <- ln_quantile_value(0.5, 50.3, "male", "WBC", reference = "Clalit-demo")
    expect_equal(res$value[1], 7.182971, tolerance = 1e-5)
    expect_equal(res$age[1], 50, tolerance = 1e-5)
})

test_that("ln_patients_quantile_value returns correct values", {
    skip_on_cran()
    clean_downloaded_data()

    mockery::stub(ln_normalize, "yesno2", FALSE, depth = 2)
    hemoglobin_data$quantile <- ln_normalize(
        hemoglobin_data$value,
        hemoglobin_data$age,
        hemoglobin_data$sex,
        "Hemoglobin"
    )

    clean_downloaded_data()
    mockery::stub(ln_patients_quantile_value, "yesno2", FALSE, depth = 2)
    hemoglobin_data$value1 <- ln_patients_quantile_value(
        hemoglobin_data$quantile,
        hemoglobin_data$age,
        hemoglobin_data$sex,
        "Hemoglobin",
        allow_edge_quantiles = TRUE
    )
    expect_equal(hemoglobin_data$value, hemoglobin_data$value1, tolerance = 1e-5, ignore_attr = TRUE)

    expect_error(
        ln_patients_quantile_value(
            hemoglobin_data$quantile,
            hemoglobin_data$age[1:5],
            hemoglobin_data$sex[1:5],
            "Hemoglobin",
            allow_edge_quantiles = FALSE
        )
    )
})

test_that("ln_quantile_value downloads data if needed", {
    skip_on_cran()
    clean_downloaded_data()

    mockery::stub(ln_quantile_value, "yesno2", FALSE, depth = 2)
    res <- ln_quantile_value(0.5, 50, "male", "WBC", reference = "Clalit")
    expect_equal(res$value[1], 7.182971, tolerance = 1e-5)
})
