setup_test <- function(...) {
    default_dir <- rappdirs::user_data_dir("labNorm")
    dir.create(default_dir, recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(file.path(default_dir, "Clalit"))) {
        unlink(file.path(default_dir, "Clalit"), recursive = TRUE)
    }
    if (dir.exists(file.path(default_dir, "UKBB"))) {
        unlink(file.path(default_dir, "UKBB"), recursive = TRUE)
    }

    withr::defer(pkgenv$yesno2 <- yesno::yesno2)
    pkgenv$yesno2 <- function(prompt) FALSE

    # Test function
    ln_download_data(...)

    return(default_dir)
}

# Test that the function downloads the data file to a temporary directory if the user does not approve or if `dir` is not provided.
test_that("ln_download_data downloads to temp dir if not approved or if dir not provided", {
    skip_on_cran()

    # Set up test
    default_dir <- setup_test()

    # Check that the data file was downloaded to a temporary directory
    expect_true(dir.exists(file.path(getOption("labNorm.dir"), "Clalit")))
    expect_true(dir.exists(file.path(getOption("labNorm.dir"), "UKBB")))

    ukbb_labs <- c(
        "ALBUMIN", "BASO_perc", "BASOPHILES_abs", "BILIRUBIN_DIRECT",
        "BILIRUBIN_TOTAL", "BMI", "BP DIASTOLIC", "BP SYSTOLIC", "C_REACTIVE_PROTEIN_CRP",
        "CALCIUM_BLOOD", "CHOLESTEROL_HDL", "CHOLESTEROL_LDL", "CHOLESTEROL",
        "CREATININE_BLOOD", "CREATININE_URINE_SAMPLE", "EOS_perc", "EOS(abs)",
        "ESTRADIOL_E_2", "GAMMA_GLUTAMYL_TRANSPEPTIDASE", "GLUCOSE_BLOOD",
        "GLUTAMIC_OXALOACETIC_TRANSAMINASE", "GLUTAMIC_PYRUVIC_TRANSAMINASE",
        "HCT", "HEMOGLOBIN_A1C_CALCULATED", "HGB", "LYMP(abs)", "LYMperc",
        "MCH", "MCHC", "MCV", "MONO(abs)", "MONperc", "MPV", "NEUT(abs)",
        "NEUTperc", "PCT", "PDW", "PHOSPHATASE_ALKALINE", "PHOSPHORUS_BLOOD",
        "PLT", "PROTEIN_TOTAL_BLOOD", "RBC", "RDW", "TRIGLYCERIDES",
        "UREA_BLOOD", "VITAMIN_D3_25_0H_RIA", "WBC"
    )

    expect_true(all(file.exists(file.path(getOption("labNorm.dir"), "UKBB", paste0(ukbb_labs, ".rds")))))

    expect_true(all(file.exists(file.path(getOption("labNorm.dir"), "Clalit", paste0(LAB_TO_FILENAME, ".rds")))))

    # Check that the quantile data was read and stored correctly
    expect_equal(load_quantiles("Clalit", "WBC"), readRDS(file.path(getOption("labNorm.dir"), "Clalit", "WBC.rds")))
    expect_equal(load_quantiles("UKBB", "WBC"), readRDS(file.path(getOption("labNorm.dir"), "UKBB", "WBC.rds")))

    pkgenv$yesno2 <- yesno::yesno2 # Reset the function
})
