test_that("pkgenv has the correct structure", {
    expect_true("asked_to_download" %in% ls(pkgenv))
    expect_true("alerted_about_download" %in% ls(pkgenv))
    expect_true("age_limits" %in% ls(pkgenv))
    expect_type(pkgenv$asked_to_download, "logical")
    expect_type(pkgenv$alerted_about_download, "logical")
    expect_type(pkgenv$age_limits, "list")
    expect_type(pkgenv$age_limits$Clalit, "double")
    expect_equal(length(pkgenv$age_limits$Clalit), 2)
})

test_that(".onLoad function exists and sets options correctly", {
    expect_type(.onLoad, "closure")
    .onLoad(NULL, NULL)
    expect_equal(options()$labNorm.dir, rappdirs::user_data_dir("labNorm"))
})
