library(tidyverse)

generate_example_data <- function(lab, n = 1000, raw_quantiles_dir = "/net/mraid14/export/tgdata/users/aviezerl/src/mldpEHR-app/backend/rawdata/lab_quantiles_raw") {
    raw_quantiles <- readr::read_csv(file.path(raw_quantiles_dir, paste0(lab, ".csv")), show_col_types = FALSE) %>%
        mutate(
            age = gsub(",.*", "", age),
            age = gsub("\\[", "", age),
            age = as.numeric(age),
            gender = factor(gender, levels = c("male", "female"))
        )

    example <- raw_quantiles %>%
        mutate(age_grp = cut(age, breaks = seq(20, 100, 10), include.lowest = TRUE)) %>%
        mutate(quant_grp = cut(quant, breaks = seq(0, 1, length.out = 5), include.lowest = TRUE)) %>%
        group_by(age_grp, quant_grp) %>%
        sample_n(n()) %>%
        slice(1:(round(n / 30))) %>%
        ungroup() %>%
        sample_n(n) %>%
        select(age, sex = gender, value) %>%
        arrange(age, sex, value)

    example <- example %>%
        mutate(sex = as.character(sex), age = as.numeric(age), value = as.numeric(value))

    return(as.data.frame(example))
}

hemoglobin_data <- generate_example_data("HGB", n = 1000)
usethis::use_data(hemoglobin_data, overwrite = TRUE)

creatinine_data <- generate_example_data("CREATININE_BLOOD", n = 1000)
usethis::use_data(creatinine_data, overwrite = TRUE)
