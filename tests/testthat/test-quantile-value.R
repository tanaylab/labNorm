test_that("quantile less than 0 throws error", {
    expect_error(ln_quantile_value(-0.01, 50, "male", "WBC"))
})

test_that("quantile greater than 1 throws error", {
    expect_error(ln_quantile_value(1.01, 50, "male", "WBC"))
})

test_that("invalid age or sex throws error", {
    expect_error(ln_quantile_value(c(0.25, 0.5, 0.75), -10, "male", "WBC"))
    expect_error(ln_quantile_value(c(0.25, 0.5, 0.75), 50, "other", "WBC"))
})

test_that("correct number of rows returned for multiple quantiles and ages/sexes", {
    res <- ln_quantile_value(c(0, 0.25, 0.5, 0.75, 1), c(50, 60), c("male", "female"), "WBC", reference = "Clalit-demo")
    expect_equal(nrow(res), 20) # 5 quantiles * 2 ages * 2 sexes
})

test_that("correct lab values are returned for single quantile", {
    res <- ln_quantile_value(0.5, 50, "male", "WBC", reference = "Clalit-demo")
    expect_equal(res$value[1], 7.182971, tolerance = 1e-5)
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
