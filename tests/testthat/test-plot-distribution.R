test_that("an error is thrown when an invalid lab is passed", {
    expect_error(ln_plot_dist("invalid_lab", reference = "Clalit-demo"))
})

test_that("an error is thrown when quantiles are not in the range (0, 1)", {
    expect_error(ln_plot_dist("Hemoglobin", quantiles = c(1.5, 2, 3), pal = c("red", "yellow"), reference = "Clalit-demo"))
})

test_that("an error is thrown when quantiles have 0 or 1", {
    expect_warning(ln_plot_dist("Hemoglobin", quantiles = c(0, 1), pal = "red", reference = "Clalit-demo"))
})

test_that("an error is thrown when sex in invalid", {
    expect_error(ln_plot_dist("Hemoglobin", sex = "savta", reference = "Clalit-demo"))
})

test_that("an error is thrown when the length of pal is not one less than the length of quantiles", {
    expect_error(ln_plot_dist("Hemoglobin", pal = c("red", "green", "blue", "purple"), reference = "Clalit-demo"))
})

test_that("an error is thrown when pal is not a vector", {
    expect_error(ln_plot_dist("Hemoglobin", pal = function(x) "red", quantiles = c(0.1, 0.9), reference = "Clalit-demo"))
})

test_that("an error is thrown when ylim is invalid", {
    expect_error(ln_plot_dist("Hemoglobin", ylim = c(0, 1, 2), reference = "Clalit-demo"))
})

test_that("ln_plot_dist() can project the distribution of three Hemoglobin values", {
    p4 <- ln_plot_dist("Hemoglobin", patients = dplyr::sample_n(hemoglobin_data, 3), reference = "Clalit-demo")
    expect_true("ggplot" %in% class(p4))
})

test_that("ln_plot_dist() returns a ggplot object", {
    p1 <- ln_plot_dist("Hemoglobin", reference = "Clalit-demo")
    expect_true("ggplot" %in% class(p1))
})

test_that("ln_plot_dist() can plot only females", {
    p2 <- ln_plot_dist("Creatinine", sex = "female", ylim = c(0, 2), reference = "Clalit-demo")
    expect_true("ggplot" %in% class(p2))
})

test_that("ln_plot_dist() can set the ylim", {
    p3 <- ln_plot_dist("BMI", ylim = c(8, 50), reference = "Clalit-demo")
    expect_true("ggplot" %in% class(p3))
})

test_that("ln_plot_dist() can project the distribution of three Hemoglobin values", {
    set.seed(60427)
    p4 <- ln_plot_dist("Hemoglobin", patients = dplyr::sample_n(hemoglobin_data, 3), reference = "Clalit-demo")
    expect_true("ggplot" %in% class(p4))
})

test_that("ln_plot_dist() can change the quantiles", {
    p5 <- ln_plot_dist("Hemoglobin", quantiles = seq(0.05, 0.95, length.out = 10), reference = "Clalit-demo")
    expect_true("ggplot" %in% class(p5))
})

test_that("ln_plot_dist() can change the colors", {
    p6 <- ln_plot_dist("Hemoglobin", quantiles = c(0.03, 0.1, 0.25, 0.5, 0.75, 0.9, 0.97), pal = c("red", "orange", "yellow", "green", "blue", "purple"), reference = "Clalit-demo")
    expect_true("ggplot" %in% class(p6))
})

test_that("ln_plot_dist() can change the reference distribution", {
    skip_on_cran()
    withr::defer(pkgenv$yesno2 <- yesno::yesno2)
    pkgenv$yesno2 <- function(prompt) TRUE
    p7 <- ln_plot_dist("Hemoglobin", reference = "UKBB")
    expect_true("ggplot" %in% class(p7))
})

test_that("ln_plot_dist() can plot the demo data", {
    p8 <- ln_plot_dist("Hemoglobin", reference = "Clalit-demo")
    expect_true("ggplot" %in% class(p8))
})

test_that("ln_plot_dist() throws an error if the patients parameter is not a data frame", {
    expect_error(ln_plot_dist("Hemoglobin", patients = c(1, 2, 3), reference = "Clalit-demo"))
})
