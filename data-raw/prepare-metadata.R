library(tgutil)
library(tidyverse)

l2q <- fread("/net/mraid14/export/tgdata/users/aviezerl/src/mldpEHR-app/backend/rawdata/lab2quantile.csv")
aft <- fread("/net/mraid14/export/tgdata/users/aviezerl/src/mldpEHR-app/backend/rawdata/all_features_with_text.csv")
lab_info <- l2q %>%
    mutate(lab = gsub("\\.quantiles_.+", "", feature)) %>%
    select(feature, label, quantile_file, lab) %>%
    left_join(aft %>% select(feature, units, default_units, conversion, full_name = text_name)) %>%
    select(quantile_file, short_name = label, full_name, units, default_units, conversion, lab)



refs <- lab_info %>%
    select(quantile_file, short_name, lab) %>%
    left_join(fread("/home/aviezerl/src/lab-trends-site/src/data/tables/normal_reference_ranges_american_board_of_internal_medicine_tidy.tsv")) %>%
    select(short_name, units, variable, value, sex = gender) %>%
    filter(variable %in% c("low", "high")) %>%
    distinct(short_name, units, variable, sex, .keep_all = TRUE) %>%
    pivot_wider(names_from = c("variable", "sex"), values_from = "value") %>%
    as.data.frame() %>%
    select(short_name, low_male, high_male, low_female, high_female)

fwrite(refs, "/home/aviezerl/src/labNorm/data-raw/reference-ranges.csv")

lab_info <- lab_info %>%
    mutate(short_name = ifelse(is.na(short_name), quantile_file, short_name)) %>%
    mutate(short_name = ifelse(short_name == "", quantile_file, short_name)) %>%
    mutate(full_name = ifelse(is.na(full_name), short_name, full_name)) %>%
    left_join(readr::read_csv("data-raw/reference-ranges.csv", show_col_types = FALSE))

fwrite(lab_info, "/home/aviezerl/src/labNorm/data-raw/quantile2feature.csv")

example_labs <- c(
    "WBC", "RBC", "Hemoglobin", "Hematocrit", "Platelets", "MCV",
    "MCH", "MCHC", "RDW", "MPV", "Albumin", "Total Cholesterol",
    "Triglycerides", "BMI", "Iron", "Transferrin", "Ferritin", "Total Globulin",
    "Fibrinogen", "Lymphocytes, Abs", "Lymphocytes, %", "Neutrophils, Abs",
    "Neutrophils, %", "Monocytes, Abs", "Monocytes, %", "Eosinophils, Abs",
    "Eosinophils, %", "Basophils, Abs", "Basophils, %", "Glucose",
    "Urea", "Creatinine", "Uric Acid", "Calcium", "Phosphorus", "Total Protein",
    "HDL Cholesterol", "LDL Cholesterol", "Alk. Phosphatase", "AST",
    "ALT", "GGT", "LDH", "CPK", "Total Bilirubin", "Direct Bilirubin",
    "Hemoglobin A1c", "Sodium", "Potassium", "Vitamin D (25-OH)",
    "TSH", "T3, Free", "T4, Free", "Blood Pressure, Systolic", "Blood Pressure, Diastolic",
    "Vitamin B12", "PSA", "ESR", "CRP", "Amylase", "Folic Acid",
    "Magnesium", "Indirect Bilirubin", "LH", "Estradiol"
)

fwrite(tibble(short_name = example_labs), "/home/aviezerl/src/labNorm/data-raw/example-labs.csv")
