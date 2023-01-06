
library(tidyverse)
library(glue)

features <- readr::read_csv("data-raw/quantile2feature.csv", show_col_types = FALSE) %>%
    mutate(feature_name = ifelse(is.na(feature_name), quantile_file, feature_name)) %>%
    mutate(feature_name = ifelse(feature_name == "", quantile_file, feature_name)) %>%
    mutate(full_name = ifelse(is.na(full_name), feature_name, full_name))

features$units <- strsplit(features$units, "\\|")
features$units <- lapply(features$units, function(x) gsub("\"", "", x))
features$default_units <- gsub("\"", "", features$default_units)
features$conversion <- strsplit(features$conversion, "\\|")

## Import the quantile files

#' Import quantiles from raw data
#'
#' @param lab the lab to import
#' @param raw_quantiles_dir the directory containing the raw quantiles
#' @param small_size the number of points to use for the small dataset
#'
#' @return an rds file with the quantiles
import_lab <- function(lab, raw_quantiles_dir = "/net/mraid14/export/tgdata/users/aviezerl/src/mldpEHR-app/backend/rawdata/lab_quantiles_raw", small_size = 21) {
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

    out_fn_large <- file.path("data-raw", "large", paste0(lab, ".rds"))
    out_fn_small <- file.path("data-raw", "small", paste0(lab, ".rds"))

    # create directories if they don't exist
    dir.create(dirname(out_fn_large), showWarnings = FALSE, recursive = TRUE)
    dir.create(dirname(out_fn_small), showWarnings = FALSE, recursive = TRUE)

    age_sex <- expand.grid(sex = c("male", "female"), age = 20:99)

    # Create a list of approxfun per age and gender
    quantiles_large <- plyr::dlply(age_sex, c("age", "sex"), function(specs) {
        x <- raw_quantiles %>% filter(age == specs$age, gender == specs$sex)
        if (nrow(x) == 0) {
            # set x to the closest age
            closest_age <- raw_quantiles %>%
                filter(gender == specs$sex) %>%
                arrange(abs(age - specs$age)) %>%
                slice(1) %>%
                pull(age)
            x <- raw_quantiles %>% filter(age == closest_age, gender == specs$sex)
            cli::cli_warn("No values at age {.field {specs$age}}. Using closest age {.field {closest_age}} for {.field {lab}}")
        }
        if (nrow(x) == 1) {
            cli::cli_warn("Only a single value at age {.field {x$age}}, gender {.field {x$gender}} for {.field {lab}}")
            return(function(x) x$quant[1])
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
            closest_age <- raw_quantiles %>%
                filter(gender == specs$sex) %>%
                arrange(abs(age - specs$age)) %>%
                slice(1) %>%
                pull(age)
            x <- raw_quantiles %>% filter(age == closest_age, gender == specs$sex)
            cli::cli_warn("No values at age {.field {specs$age}}. Using closest age {.field {closest_age}} for {.field {lab}}")
        }
        if (nrow(x) == 1) {
            # return a function that would always return the value that is now at x$quant[1].
            z <- x$quant[1]
            y <- substitute(z)
            return(function(x) force(y))
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
#' @inheritParams import_lab
#' @param ncores the number of cores to use
#' @param parallel whether to use parallel processing
import_all_labs <- function(raw_quantiles_dir = "/net/mraid14/export/tgdata/users/aviezerl/src/mldpEHR-app/backend/rawdata/lab_quantiles_raw", small_size = 21, ncores = 40, parallel = TRUE) {
    doMC::registerDoMC(ncores)

    labs <- features$quantile_file
    plyr::l_ply(labs, import_lab, raw_quantiles_dir = raw_quantiles_dir, small_size = small_size, .parallel = parallel)


    cli::cli_alert_success("Done importing all labs. Size of small dir is {.val {fs::fs_bytes(sum(fs::dir_info('data-raw/small')$size))}}. Size of large dir is {.val {fs::fs_bytes(sum(fs::dir_info('data-raw/large')$size))}}")
}



## Create package data

create_labs_data <- function() {
    LAB_QUANTILES <- plyr::llply(features$quantile_file, function(lab) {
        readr::read_rds(file.path("data-raw", "small", paste0(lab, ".rds")))
    })

    names(LAB_QUANTILES) <- features$feature_name

    UNITS_CONVERSION <- map(features$feature_name, function(feature) {
        units <- features %>%
            filter(feature_name == feature) %>%
            pull(units)
        conversion <- features %>%
            filter(feature_name == feature) %>%
            pull(conversion)

        res <- map(conversion[[1]], ~ eval(parse(text = glue("function(x) {.x}"))))
        names(res) <- units[[1]]
        return(res)
    })
    names(UNITS_CONVERSION) <- features$feature_name

    usethis::use_data(LAB_QUANTILES, UNITS_CONVERSION, overwrite = TRUE, internal = TRUE, compress = "xz")
}

create_lab_info <- function() {
    LAB_INFO <- as.data.frame(features) %>%
        select(short_name = feature_name, long_name = full_name, units, default_units)

    usethis::use_data(LAB_INFO, overwrite = TRUE, internal = FALSE, compress = "xz")
}




## Diagnostics

compute_large_vs_small <- function(lab, raw_quantiles_dir = "/net/mraid14/export/tgdata/users/aviezerl/src/mldpEHR-app/backend/rawdata/lab_quantiles_raw") {
    quantiles_small <- readr::read_rds(file.path("data-raw", "small", paste0(lab, ".rds")))

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
    cli::cli_alert_info("MSE between large and small quantiles is {.val {mse}}. Max difference is {.val {max_diff}}")

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

# import_all_labs()
create_labs_data()
create_lab_info()
# plot_all_large_vs_small()
