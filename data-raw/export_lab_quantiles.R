#! /usr/local/bin/Rscript
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("doMC"))
registerDoMC(cores = 20)

args <- commandArgs(trailingOnly = TRUE)
export_dir <- args[1]
lab_def_file <- args[2]

library(mermaid)
emr_init()
if (!dir.exists(export_dir)) {
    dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
}
export_labs <- tgutil::fread(lab_def_file)
time_scope <- c(1208280, 1339752)
plyr::a_ply(export_labs, 1, function(l) {
    labq <- emr_lab_age_gender_quantiles_med_filtered(l$lab, 20:90, med_cohort_set = EMR_MED_COHORT_SET, time_scope = time_scope) %>%
        distinct(age, gender, quant, .keep_all = TRUE) %>%
        group_by(age, gender) %>%
        mutate(n = sum(n)) %>%
        ungroup() %>%
        select(age, gender, value, quant, n)
    tgutil::fwrite(labq, file = paste0(export_dir, l$quantile_file, ".csv"))
}, .parallel = T)
