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
    expect_error(ln_normalize(c(1, 2, 3), c(25, 30), c("male", "female"), "Hemoglobin"))
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

test_that("ln_normalize_multi works", {
    multi_labs_df <- bind_rows(
        hemoglobin_data %>% mutate(lab = "Hemoglobin"),
        creatinine_data %>% mutate(lab = "Creatinine")
    ) %>%
        filter(age == 50, sex == "male")

    multi_labs_df$quantile <- ln_normalize_multi(multi_labs_df, reference = "Clalit-demo")

    expect_equal(
        multi_labs_df$quantile[multi_labs_df$lab == "Hemoglobin"],
        LAB_QUANTILES[["Hemoglobin"]][["50.male"]](multi_labs_df$value[multi_labs_df$lab == "Hemoglobin"])
    )

    expect_equal(
        multi_labs_df$quantile[multi_labs_df$lab == "Creatinine"],
        LAB_QUANTILES[["Creatinine"]][["50.male"]](multi_labs_df$value[multi_labs_df$lab == "Creatinine"])
    )
})

test_that("ln_normalize returns NA when quantiles are missing", {
    expect_equal(ln_normalize(5, 50, "male", "Estradiol", reference = "Clalit-demo"), NA)
})

test_that("ln_normalize_multi fails with missing columns", {
    expect_error(ln_normalize_multi(hemoglobin_data %>% select(-lab), reference = "Clalit-demo"))
    expect_error(ln_normalize_multi(hemoglobin_data %>% select(-age), reference = "Clalit-demo"))
    expect_error(ln_normalize_multi(hemoglobin_data %>% select(-sex), reference = "Clalit-demo"))
})

test_that("ln_normalize returns NA when age is out of range", {
    expect_warning(q <- ln_normalize(c(1:10), 5:14, "male", "Hemoglobin", reference = "Clalit-demo"))
    expect_true(all(is.na(q)))

    expect_warning(q <- ln_normalize(c(1:10), 125:134, "male", "Hemoglobin", reference = "Clalit-demo"))
    expect_true(all(is.na(q)))
})

test_that("ln_normalize fails on NA values when na.rm=FALSE", {
    expect_error(ln_normalize(c(15, 16),
        age = c(50, NA), sex = c("male", "male"), lab = "WBC",
        reference = "Clalit-demo", na.rm = FALSE
    ))
    expect_error(ln_normalize(c(15, 16),
        age = c(50, 60), sex = c("male", NA), lab = "WBC",
        reference = "Clalit-demo", na.rm = FALSE
    ))
    expect_error(ln_normalize(c(15, NA),
        age = c(50, 60), sex = c("male", "male"), lab = "WBC",
        reference = "Clalit-demo", na.rm = FALSE
    ))
})

test_that("ln_normalize_multi fails on NA values when na.rm=FALSE", {
    expect_error(ln_normalize_multi(
        data.frame(
            value = c(15, 16),
            age = c(50, NA), sex = c("male", "male"), lab = "WBC"
        ),
        reference = "Clalit-demo", na.rm = FALSE
    ))

    expect_error(ln_normalize_multi(
        data.frame(
            value = c(15, 16),
            age = c(50, 60), sex = c("male", NA), lab = "WBC"
        ),
        reference = "Clalit-demo", na.rm = FALSE
    ))
    expect_error(ln_normalize_multi(
        data.frame(
            value = c(15, NA),
            age = c(50, 60), sex = c("male", "male"), lab = "WBC"
        ),
        reference = "Clalit-demo", na.rm = FALSE
    ))
})

test_that("ln_normalize works with NA values when na.rm=TRUE", {
    q <- ln_normalize(c(15, 16),
        age = c(50, NA), sex = c("male", "male"), lab = "WBC",
        reference = "Clalit-demo", na.rm = TRUE
    )
    expect_equal(q, c(0.951147651603799, NA), tolerance = 1e-5)
    q <- ln_normalize(c(15, 16),
        age = c(50, 60), sex = c("male", NA), lab = "WBC",
        reference = "Clalit-demo", na.rm = TRUE
    )
    expect_equal(q, c(0.951147651603799, NA), tolerance = 1e-5)

    q <- ln_normalize(c(15, NA),
        age = c(50, 60), sex = c("male", "male"), lab = "WBC",
        reference = "Clalit-demo", na.rm = TRUE
    )
    expect_equal(q, c(0.951147651603799, NA), tolerance = 1e-5)
})

test_that("ln_normalize_multi works with NA values when na.rm=TRUE", {
    q <- ln_normalize_multi(
        data.frame(
            value = c(15, 16),
            age = c(50, NA), sex = c("male", "male"), lab = "WBC"
        ),
        reference = "Clalit-demo", na.rm = TRUE
    )
    expect_equal(q, c(0.951147651603799, NA), tolerance = 1e-5)
    q <- ln_normalize_multi(
        data.frame(
            value = c(15, 16),
            age = c(50, 60), sex = c("male", NA), lab = "WBC"
        ),
        reference = "Clalit-demo", na.rm = TRUE
    )
    expect_equal(q, c(0.951147651603799, NA), tolerance = 1e-5)

    q <- ln_normalize_multi(
        data.frame(
            value = c(15, NA),
            age = c(50, 60), sex = c("male", "male"), lab = "WBC"
        ),
        reference = "Clalit-demo", na.rm = TRUE
    )
    expect_equal(q, c(0.951147651603799, NA), tolerance = 1e-5)
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
    clean_downloaded_data()

    hemoglobin_50 <- hemoglobin_data %>%
        filter(age == 50, sex == "male")

    mockery::stub(ln_normalize, "yesno2", FALSE, depth = 2)

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
    clean_downloaded_data()

    hemoglobin_50 <- hemoglobin_data %>%
        filter(age == 50, sex == "male")

    mockery::stub(ln_normalize, "yesno2", FALSE, depth = 2)
    q <- ln_normalize(
        hemoglobin_50$value,
        hemoglobin_50$age,
        hemoglobin_50$sex,
        "Hemoglobin",
        reference = "UKBB"
    )

    expect_equal(q, pkgenv$UKBB[["Hemoglobin"]][["[50,55).male"]](hemoglobin_50$value))
})


test_that("ln_normalize_ukbb works", {
    skip_on_cran()
    clean_downloaded_data()

    hemoglobin_50 <- hemoglobin_data %>%
        filter(age == 50, sex == "male")

    mockery::stub(ln_normalize_ukbb, "yesno2", FALSE, depth = 2)
    q <- ln_normalize_ukbb(
        hemoglobin_50$value,
        hemoglobin_50$age,
        hemoglobin_50$sex,
        "30020"
    )

    expect_equal(q, pkgenv$UKBB[["Hemoglobin"]][["[50,55).male"]](hemoglobin_50$value))
})

test_that("ln_normalize_clalit works", {
    skip_on_cran()
    clean_downloaded_data()

    hemoglobin_50 <- hemoglobin_data %>%
        filter(age == 50, sex == "male")

    mockery::stub(ln_normalize_clalit, "yesno2", FALSE, depth = 2)
    q <- ln_normalize_clalit(
        hemoglobin_50$value,
        hemoglobin_50$age,
        hemoglobin_50$sex,
        "lab.103"
    )

    expect_equal(q, pkgenv$Clalit[["Hemoglobin"]][["50.male"]](hemoglobin_50$value))
})

test_that("ln_normalize_multi_clalit works", {
    skip_on_cran()
    clean_downloaded_data()

    multi_labs_df <- bind_rows(
        hemoglobin_data %>% mutate(lab_code = "lab.103"),
        creatinine_data %>% mutate(lab_code = "lab.20300")
    ) %>%
        filter(age == 50, sex == "male")

    mockery::stub(ln_normalize_multi_clalit, "yesno2", FALSE, depth = 2)

    multi_labs_df$quantile <- ln_normalize_multi_clalit(
        multi_labs_df
    )

    expect_equal(
        multi_labs_df$quantile[multi_labs_df$lab_code == "lab.103"],
        pkgenv$Clalit[["Hemoglobin"]][["50.male"]](multi_labs_df$value[multi_labs_df$lab_code == "lab.103"])
    )

    expect_equal(
        multi_labs_df$quantile[multi_labs_df$lab_code == "lab.20300"],
        pkgenv$Clalit[["Creatinine"]][["50.male"]](multi_labs_df$value[multi_labs_df$lab_code == "lab.20300"])
    )
})

test_that("ln_normalize_multi_ukbb works", {
    skip_on_cran()
    clean_downloaded_data()

    multi_labs_df <- bind_rows(
        hemoglobin_data %>% mutate(lab_code = "30020"),
        creatinine_data %>% mutate(lab_code = "30700")
    ) %>%
        filter(age == 50, sex == "male")

    mockery::stub(ln_normalize_multi_ukbb, "yesno2", FALSE, depth = 2)

    multi_labs_df$quantile <- ln_normalize_multi_ukbb(
        multi_labs_df
    )

    expect_equal(
        multi_labs_df$quantile[multi_labs_df$lab_code == "30020"],
        pkgenv$UKBB[["Hemoglobin"]][["[50,55).male"]](multi_labs_df$value[multi_labs_df$lab_code == "30020"])
    )

    expect_equal(
        multi_labs_df$quantile[multi_labs_df$lab_code == "30700"],
        pkgenv$UKBB[["Creatinine"]][["[50,55).male"]](ln_convert_units(multi_labs_df$value[multi_labs_df$lab_code == "30700"], units = "umol/L", lab = "Creatinine"))
    )
})
