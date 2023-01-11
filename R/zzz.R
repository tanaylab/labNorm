pkgenv <- new.env(parent = emptyenv())
pkgenv$asked_to_download <- FALSE
pkgenv$alerted_about_download <- FALSE
pkgenv$yesno2 <- yesno::yesno2 # used inside an environment so that it can be mocked
pkgenv$age_limits <- list(
    Clalit = c(20, 89),
    UKBB = c(35, 80),
    "Clalit-demo" = c(20, 89)
)

.onLoad <- function(lib, pkg) {
    options(labNorm.dir = rappdirs::user_data_dir("labNorm"))
}
