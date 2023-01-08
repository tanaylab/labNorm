test_that("an error is thrown when an invalid lab is passed", {
    expect_error(ln_plot_dist("invalid_lab"))
})

test_that("an error is thrown when quantiles are not in the range (0, 1)", {
    expect_error(ln_plot_dist("Hemoglobin", quantiles = c(1.5, 2, 3)))
})

test_that("an error is thrown when the length of pal is not one less than the length of quantiles", {
    expect_error(ln_plot_dist("Hemoglobin", pal = c("red", "green", "blue", "purple")))
})

test_that("an error is thrown when pal is not a vector", {
    expect_error(ln_plot_dist("Hemoglobin", pal = "red"))
})

test_that("a ggplot object is returned when called with default arguments", {
    expect_true("ggplot" %in% class(ln_plot_dist("Hemoglobin")))
})
