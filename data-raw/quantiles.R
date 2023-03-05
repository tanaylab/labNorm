
# run "prepare-metadata.R" before running this script

library(tidyverse)
library(glue)

features <- readr::read_csv("data-raw/quantile2feature.csv", show_col_types = FALSE)
features$units <- strsplit(features$units, "\\|")
features$units <- lapply(features$units, function(x) gsub("\"", "", x))
features$default_units <- gsub("\"", "", features$default_units)
features$conversion <- strsplit(features$conversion, "\\|")

example_labs <- readr::read_csv("data-raw/example-labs.csv", show_col_types = FALSE)
example_features <- features %>% filter(short_name %in% example_labs$short_name)



## Import the quantile files

#' Import quantiles from raw data
#'
#' @param lab the lab to import
#' @param raw_quantiles_dir the directory containing the raw quantiles
#' @param small_size the number of points to use for the Clalit-demo dataset
#' @param min_n minimal number of values in order to use the quantile
#'
#' @return an rds file with the quantiles
import_lab_clalit <- function(lab, raw_quantiles_dir = "/net/mraid14/export/tgdata/users/aviezerl/src/mldpEHR-app/backend/rawdata/lab_quantiles_raw", small_size = 21, min_n = 250) {
    cli::cli_alert("Importing {.field {lab}}")
    # Read the raw quantiles
    raw_quantiles <- readr::read_csv(file.path(raw_quantiles_dir, paste0(lab, ".csv")), show_col_types = FALSE) %>%
        mutate(
            age = gsub(",.*", "", age),
            age = gsub("\\[", "", age),
            age = as.numeric(age),
            gender = factor(gender, levels = c("male", "female"))
        ) %>%
        as.data.frame()

    out_fn_large <- file.path("data-raw", "Clalit", paste0(lab, ".rds"))
    out_fn_small <- file.path("data-raw", "Clalit-demo", paste0(lab, ".rds"))

    # create directories if they don't exist
    dir.create(dirname(out_fn_large), showWarnings = FALSE, recursive = TRUE)
    dir.create(dirname(out_fn_small), showWarnings = FALSE, recursive = TRUE)

    age_sex <- expand.grid(sex = c("male", "female"), age = 20:89)

    # Create a list of approxfun per age and gender
    quantiles_large <- plyr::dlply(age_sex, c("age", "sex"), function(specs) {
        x <- raw_quantiles %>% filter(age == specs$age, gender == specs$sex)

        if (nrow(x) == 0) {
            cli::cli_warn("No values at age {.field {specs$age}} and sex {.field {specs$sex}}.")
            return(NA)
        }
        num_vals <- x$n[1]
        if (num_vals < min_n) {
            cli::cli_warn("Number of values is less than {.field {min_n}} at age {.field {specs$age}} and sex {.field {specs$sex}}.")
            return(NA)
        }
        func <- approxfun(x = x$value, y = x$quant, rule = 2)
        if (!all(func(x$value) == x$quant)) {
            cli::cli_abort("The quantiles do not match for {.field {lab}} at age {.field {specs$age}}")
        }
        return(func)
    })
    attr(quantiles_large, "split_type") <- NULL
    attr(quantiles_large, "split_labels") <- NULL

    readr::write_rds(quantiles_large, out_fn_large, compress = "xz", compression = 9)

    # create a smaller version of the quantiles
    quantiles_small <- plyr::dlply(age_sex, c("age", "sex"), function(specs) {
        x <- raw_quantiles %>% filter(age == specs$age, gender == specs$sex)

        if (nrow(x) == 0) {
            cli::cli_warn("No values at age {.field {specs$age}} and sex {.field {specs$sex}}.")
            return(NA)
        }
        num_vals <- x$n[1]
        if (num_vals < min_n) {
            cli::cli_warn("Number of values is less than {.field {min_n}} at age {.field {specs$age}} and sex {.field {specs$sex}}.")
            return(NA)
        }

        f <- approxfun(x = x$value, y = x$quant, rule = 2)
        f_quant_to_val <- approxfun(x = x$quant, y = x$value, rule = 2)
        val1 <- f_quant_to_val(seq(0, 1, length.out = small_size))
        quant1 <- f(val1)
        func1 <- approxfun(val1, quant1)
        if (!all(f(x$value) == x$quant)) {
            cli::cli_abort("The quantiles do not match for {.field {lab}} at age {.field {specs$age}}")
        }
        return(func1)
    })
    attr(quantiles_small, "split_type") <- NULL
    attr(quantiles_small, "split_labels") <- NULL
    readr::write_rds(quantiles_small, out_fn_small, compress = "xz", compression = 9)

    cli::cli_alert_success("Done importing {.field {lab}}. Large file size is {.val {fs::fs_bytes(file.size(out_fn_large))}}. Small file size is {.val {fs::fs_bytes(file.size(out_fn_small))}}")
}

#' Import all labs
#'
#' @inheritParams import_lab_clalit
#' @param ncores the number of cores to use
#' @param parallel whether to use parallel processing
import_all_labs_clalit <- function(raw_quantiles_dir = "/net/mraid14/export/tgdata/users/aviezerl/src/mldpEHR-app/backend/rawdata/lab_quantiles_raw", small_size = 21, ncores = 40, parallel = TRUE) {
    doMC::registerDoMC(ncores)

    labs <- features$quantile_file[!is.na(features$lab)]
    plyr::l_ply(labs, import_lab_clalit, raw_quantiles_dir = raw_quantiles_dir, small_size = small_size, .parallel = parallel)


    cli::cli_alert_success("Done importing all labs. Size of Clalit-demo dir is {.val {fs::fs_bytes(sum(fs::dir_info('data-raw/Clalit-demo')$size))}}. Size of Clalit dir is {.val {fs::fs_bytes(sum(fs::dir_info('data-raw/Clalit')$size))}}")
}

#' Import quantiles from raw data
#'
#' @param lab the lab to import
#' @param raw_quantiles_dir the directory containing the raw quantiles
#' @param min_n minimal number of values in order to use the quantile
#'
#' @return an rds file with the quantiles
import_lab_ukbb <- function(lab, raw_quantiles_dir = "/net/mraid14/export/data/users/nettam/projects/emr/labs/ukbb.lab.quantiles", min_n = 250) {
    cli::cli_alert("Importing {.field {lab}}")
    # Read the raw quantiles
    raw_quantiles <- readr::read_csv(file.path(raw_quantiles_dir, paste0(lab, ".csv")), show_col_types = FALSE) %>%
        mutate(
            sex = factor(sex, levels = c("male", "female"))
        ) %>%
        as.data.frame()

    out_fn <- file.path("data-raw", "UKBB", paste0(lab, ".rds"))

    # create directories if they don't exist
    dir.create(dirname(out_fn), showWarnings = FALSE, recursive = TRUE)

    age_sex <- expand.grid(sex = c("male", "female"), age = levels(cut(35:80, seq(35, 80, 5), right = FALSE)))

    # Create a list of approxfun per age and gender
    quantiles <- plyr::dlply(age_sex, c("age", "sex"), function(specs) {
        x <- raw_quantiles %>% filter(age == specs$age, sex == specs$sex)

        if (nrow(x) == 0) {
            cli::cli_warn("No values at age {.field {specs$age}} and sex {.field {specs$sex}}.")
            return(NA)
        }
        num_vals <- x$n[1]
        if (num_vals < min_n) {
            cli::cli_warn("Number of values is less than {.field {min_n}} at age {.field {specs$age}} and sex {.field {specs$sex}}.")
            return(NA)
        }
        func <- approxfun(x = x$value, y = x$quant, rule = 2)
        if (!all(func(x$value) == x$quant)) {
            cli::cli_abort("The quantiles do not match for {.field {lab}} at age {.field {specs$age}}")
        }
        return(func)
    })
    attr(quantiles, "split_type") <- NULL
    attr(quantiles, "split_labels") <- NULL

    readr::write_rds(quantiles, out_fn, compress = "xz", compression = 9)
}

#' Import all labs
#'
#' @inheritParams import_lab_clalit
#' @param ncores the number of cores to use
#' @param parallel whether to use parallel processing
import_all_labs_ukbb <- function(raw_quantiles_dir = "/net/mraid14/export/data/users/nettam/projects/emr/labs/ukbb.lab.quantiles", ncores = 40, parallel = TRUE) {
    doMC::registerDoMC(ncores)

    labs <- features$quantile_file
    labs_ukbb <<- list.files(raw_quantiles_dir, pattern = ".csv", full.names = FALSE) %>% stringr::str_remove(".csv")
    labs_ukbb <<- labs[labs %in% labs_ukbb]
    cli::cli_alert_info("The following labs are missing from UKBB: {.val {setdiff(labs, labs_ukbb)}}")
    plyr::l_ply(labs_ukbb, import_lab_ukbb, raw_quantiles_dir = raw_quantiles_dir, .parallel = parallel)


    cli::cli_alert_success("Done importing all ukbb labs. Size of dir is {.val {fs::fs_bytes(sum(fs::dir_info('data-raw/UKBB')$size))}}.")
}


## Create package data

create_labs_data <- function() {
    LAB_QUANTILES <- plyr::llply(example_features$quantile_file, function(lab) {
        readr::read_rds(file.path("data-raw", "Clalit-demo", paste0(lab, ".rds")))
    })

    names(LAB_QUANTILES) <- example_features$short_name

    UNITS_CONVERSION <- map(features$short_name, function(feature) {
        units <- features %>%
            filter(short_name == feature) %>%
            pull(units)
        conversion <- features %>%
            filter(short_name == feature) %>%
            pull(conversion)
        res <- map(conversion[[1]], ~ eval(parse(text = glue("function(x) {.x}"))))
        names(res) <- units[[1]]
        return(res)
    })
    names(UNITS_CONVERSION) <- features$short_name

    LAB_TO_FILENAME <- features %>%
        select(short_name, quantile_file) %>%
        tibble::deframe()

    usethis::use_data(LAB_QUANTILES, UNITS_CONVERSION, LAB_TO_FILENAME, overwrite = TRUE, internal = TRUE, compress = "xz")
}

create_high_res_labs_data <- function() {
    system(glue("tar -czf data-raw/Clalit.tar.gz -C data-raw Clalit"))
}

create_ukbb_labs_data <- function() {
    system(glue("tar -czf data-raw/UKBB.tar.gz -C data-raw UKBB"))
}

create_lab_info <- function() {
    LAB_DETAILS <- as.data.frame(features) %>%
        select(short_name = short_name, long_name = full_name, units, default_units, low_male, high_male, low_female, high_female, clalit_code = lab, ukbb_code, ukbb_units) %>%
        mutate(ukbb_code = as.character(ukbb_code))

    usethis::use_data(LAB_DETAILS, overwrite = TRUE, internal = FALSE, compress = "xz")
}

## Diagnostics

compute_large_vs_small <- function(lab, raw_quantiles_dir = "/net/mraid14/export/tgdata/users/aviezerl/src/mldpEHR-app/backend/rawdata/lab_quantiles_raw") {
    quantiles_small <- readr::read_rds(file.path("data-raw", "Clalit-demo", paste0(lab, ".rds")))

    raw_quantiles <- readr::read_csv(file.path(raw_quantiles_dir, paste0(lab, ".csv")), show_col_types = FALSE) %>%
        mutate(
            age = gsub(",.*", "", age),
            age = gsub("\\[", "", age),
            age = as.numeric(age),
            gender = factor(gender, levels = c("male", "female"))
        ) %>%
        as_tibble()

    df <- raw_quantiles %>%
        mutate(quant_small = purrr::pmap_dbl(., function(age, gender, value, quant) {
            quantiles_small[[paste0(age, ".", gender)]](value)
        }))

    return(df)
}

plot_large_vs_small <- function(lab, raw_quantiles_dir = "/net/mraid14/export/tgdata/users/aviezerl/src/mldpEHR-app/backend/rawdata/lab_quantiles_raw") {
    df <- compute_large_vs_small(lab, raw_quantiles_dir)

    mse_f <- function(x, y) mean((x - y)^2)
    mse <- mse_f(df$quant, df$quant_small)
    max_diff <- max(abs(df$quant - df$quant_small))
    cli::cli_alert_info("MSE between large and Clalit-demo quantiles is {.val {mse}}. Max difference is {.val {max_diff}}")

    p <- df %>%
        ggplot(aes(x = quant, y = quant_small)) +
        scattermore::geom_scattermore() +
        ggtitle(glue("{lab} (MSE: {mse}, Maximal diff: {max_diff})")) +
        facet_grid(. ~ gender) +
        theme_bw() +
        theme(aspect.ratio = 1)

    return(p)
}

plot_all_large_vs_small <- function(raw_quantiles_dir = "/net/mraid14/export/tgdata/users/aviezerl/src/mldpEHR-app/backend/rawdata/lab_quantiles_raw") {
    labs <- features$quantile_file

    # create plots dir
    dir.create(file.path("data-raw", "plots"), showWarnings = FALSE, recursive = TRUE)

    stats <- plyr::ldply(labs, function(.x) {
        cli::cli_alert("Plotting {.field {.x}}")
        p <- plot_large_vs_small(.x, raw_quantiles_dir = raw_quantiles_dir)
        png(file.path("data-raw", "plots", paste0(.x, ".png")), width = 1000, height = 1000)
        print(p)
        dev.off()
        data <- compute_large_vs_small(.x, raw_quantiles_dir)
        mse_f <- function(x, y) mean((x - y)^2)
        mse <- mse_f(data$quant, data$quant_small)
        max_diff <- max(abs(data$quant - data$quant_small))
        return(data.frame(lab = .x, mse = mse, max_diff = max_diff))
    }, .parallel = TRUE)

    cli::cli_alert_success("Done plotting all labs. Size of plots dir is {.val {fs::fs_bytes(sum(fs::dir_info('data-raw/plots')$size))}}")
    stats <- arrange(stats, desc(max_diff))

    readr::write_csv(stats, file.path("data-raw", "plots", "stats.csv"))
    return(stats)
}

import_all_labs_clalit()
import_all_labs_ukbb()
create_labs_data()
create_lab_info()
create_high_res_labs_data()
create_ukbb_labs_data()
# plot_all_large_vs_small()
