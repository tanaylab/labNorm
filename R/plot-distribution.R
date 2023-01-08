#' Plot age-sex distribution of a lab
#'
#' @param lab the lab name. See \code{LAB_INFO$short_name} for a list of available labs.
#' @param quantiles a vector of quantiles to plot, without 0 and 1. Default is \code{c(0.03, 0.1, 0.15, 0.25, 0.35, 0.5, 0.65, 0.75, 0.85, 0.9, 0.97)}.
#' @param pal a vector of colors to use for the quantiles. Should be of length \code{length(quantiles) - 1}.
#' @param sex Plot only a single sex ("male" or "female"). If NULL - \code{ggplot2::facet_grid} would be used to plot both sexes. Default is NULL.
#' @param patients (optional) a data frame of patients to plot as dots over the distribution. See the \code{df} parameter of \code{ln_normalize_multi} for details.
#' @param patient_color (optional) the color of the patient dots. Default is "yellow".
#' @param patient_point_size (optional) the size of the patient dots. Default is 2.
#' @param ylim (optional) a vector of length 2 with the lower and upper limits of the plot. Default would be determined based on the median value of the upper and lower percentiles of the lab in each age.
#' @param show_reference (optional) if TRUE, plot two lines of the upper and lower reference ranges. Default is TRUE.
#'
#' @return a \code{ggplot2} object
#'
#' @examples
#' \dontshow{
#' set.seed(60427)
#' }
#'
#' ln_plot_dist("Hemoglobin")
#'
#' # Plot only females
#' ln_plot_dist("Creatinine", sex = "female", ylim = c(0, 2))
#'
#' # Set the ylim
#' ln_plot_dist("BMI", ylim = c(8, 50))
#'
#' # Project the distribution of three Hemoglobin values
#' ln_plot_dist("Hemoglobin", patients = dplyr::sample_n(hemoglobin_data, 3))
#'
#' # Change the quantiles
#' ln_plot_dist("Hemoglobin",
#'     quantiles = seq(0.05, 0.95, length.out = 10)
#' )
#'
#' # Change the colors
#' ln_plot_dist(
#'     "Hemoglobin",
#'     quantiles = c(0.03, 0.1, 0.25, 0.5, 0.75, 0.9, 0.97),
#'     pal = c("red", "orange", "yellow", "green", "blue", "purple")
#' )
#'
#' @export
ln_plot_dist <- function(lab,
                         quantiles = c(0.03, 0.1, 0.15, 0.25, 0.35, 0.65, 0.75, 0.85, 0.9, 0.97),
                         pal = c("#D7DCE7", "#B0B9D0", "#8997B9", "#6274A2", "#3B528B", "#6274A2", "#8997B9", "#B0B9D0", "#D7DCE7"),
                         sex = NULL,
                         patients = NULL,
                         patient_color = "yellow",
                         patient_point_size = 2,
                         ylim = NULL,
                         show_reference = TRUE) {
    validate_lab(lab)
    validate_quantiles(quantiles)

    # make sure the palette is the right length
    if (length(pal) != length(quantiles) - 1) {
        cli::cli_abort("The length of the {.field pal} should be one less than the length of {.field quantiles}.")
    }

    # make sure the palette is a vector
    if (!is.vector(pal)) {
        cli::cli_abort("The {.field pal} should be a vector.")
    }

    if (!is.null(sex) && !(sex %in% c("male", "female"))) {
        cli::cli_abort("The {.field sex} can be either {.val male} or {.val female}")
    }

    if (1 %in% quantiles || 0 %in% quantiles) {
        cli::cli_abort("The {.field quantiles} should not contain 0 or 1. (The 0 and 1 quantiles are always plotted.)")
    }

    quantiles <- sort(quantiles)

    # get the data
    df_full <- ln_quantile_value(sort(unique(c(0.01, quantiles, 0.5, 0.99))), 20:99, c("male", "female"), lab)

    # get reference ranges
    lab_info <- get_lab_info(lab)
    refs_df <- tibble::tribble(
        ~sex, ~yintercept,
        "male", lab_info$low_male,
        "male", lab_info$high_male,
        "female", lab_info$low_female,
        "female", lab_info$high_female
    )

    if (!is.null(sex)) {
        df_full <- df_full %>%
            filter(sex == !!sex)
        refs_df <- refs_df %>%
            filter(sex == !!sex)
    }

    refs_df <- refs_df %>%
        filter(!is.na(yintercept))
    if (nrow(refs_df) == 0) {
        show_reference <- FALSE
    }

    df <- df_full %>%
        filter(quantile %in% quantiles) %>%
        mutate(
            Percentile = scales::percent(quantile),
            Percentile = factor(Percentile, levels = scales::percent(quantiles))
        ) %>%
        arrange(age, sex, desc(Percentile)) %>%
        group_by(age, sex) %>%
        mutate(strata_value = value - lead(value)) %>%
        mutate(strata_value = ifelse(is.na(strata_value), value, strata_value)) %>%
        ungroup() %>%
        mutate(transparent = ifelse(quantile %in% min(quantiles), 0, 1)) %>%
        rename(Age = age)

    df_medians <- df_full %>%
        filter(quantile == 0.5) %>%
        rename(Age = age, Percentile = quantile, Median = value)

    if (is.null(ylim)) {
        ylim <- c(
            median(df_full %>% filter(quantile == 0.01) %>% pull(value)),
            median(df_full %>% filter(quantile == 0.99) %>% pull(value))
        )
    } else {
        if (length(ylim) != 2) {
            cli::cli_abort("The {.field ylim} should be a vector of length 2.")
        }
    }

    colors <- c(pal, "white")

    # plot the distribution
    p <- df %>%
        mutate(Percentile = forcats::fct_rev(Percentile)) %>%
        ggplot(aes(x = Age, y = strata_value, fill = Percentile, alpha = transparent)) +
        geom_area() +
        coord_cartesian(ylim = ylim) +
        scale_fill_manual(name = "Percentile", values = colors) +
        guides(alpha = "none") +
        geom_line(data = df_medians, inherit.aes = FALSE, aes(x = Age, y = Median)) +
        ylab(glue("{lab} ({lab_info$default_units})")) +
        xlab("Age") +
        scale_x_continuous(breaks = seq(min(df_full$age), max(df_full$age), by = 10)) +
        scale_y_continuous(breaks = scales::extended_breaks(n = 4), minor_breaks = scales::extended_breaks(n = 20)) +
        theme_linedraw() +
        theme(strip.background = element_blank(), panel.border = element_rect(color = "black"), strip.text = element_text(color = "black"), panel.ontop = TRUE, panel.background = element_blank())

    if (is.null(sex)) {
        p <- p + facet_grid(. ~ sex)
    }

    if (show_reference) {
        p <- p +
            geom_hline(
                data = refs_df,
                aes(yintercept = yintercept),
                color = "black",
                linewidth = 0.5,
                linetype = "dashed"
            )
    }

    if (!is.null(patients)) {
        patients <- validate_patients_df(patients, lab)
        p <- p +
            geom_point(
                data = patients,
                aes(x = age, y = value),
                color = patient_color,
                size = patient_point_size,
                inherit.aes = FALSE
            )
    }



    return(p)
}

validate_patients_df <- function(df, lab) {
    validate_lab(lab)
    lab_info <- get_lab_info(lab)
    if (!is.data.frame(df)) {
        cli::cli_abort("The {.field patients} should be a data frame.")
    }

    validate_age_and_sex(df$age, df$sex)
    if (is.null(df$units)) {
        df$units <- lab_info$default_units
    }


    validate_units(df$units, lab)

    # convert units if needed
    df$value <- ln_convert_units(df$value, df$units, lab)

    return(df)
}
