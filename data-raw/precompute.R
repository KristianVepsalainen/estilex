# data-raw/precompute.R
#
# Pre-computes the vignette.
#
# vignettes/estilex.Rmd.orig contains live calls to the Riigi Teataja.
# This script knits it into vignettes/estilex.Rmd, which holds the results
# as static output. Only the knitted file is shipped, so neither CRAN nor
# `R CMD check` makes a network request.
#
# Run after any change that affects vignette output, and before release:
#
#   source("data-raw/precompute.R")
#   devtools::build_readme()
#
# Then commit both vignettes/estilex.Rmd and README.md.
#
# No archive download is needed. The vignette's corpus section is written
# with eval = FALSE precisely so that this script stays cheap to run.

stopifnot(
  "run this from the package root" = file.exists("DESCRIPTION"),
  "vignettes/estilex.Rmd.orig is missing"  =
    file.exists("vignettes/estilex.Rmd.orig")
)

for (pkg in c("knitr", "withr", "curl", "pkgload")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("package '", pkg, "' is needed to pre-compute the vignette")
  }
}

if (!curl::has_internet()) {
  stop("no internet connection; the vignette cannot be pre-computed offline")
}

# Knit against the sources, not against whatever happens to be installed.
pkgload::load_all(quiet = TRUE)

message("Knitting vignettes/estilex.Rmd.orig ...")

withr::with_dir("vignettes", {
  knitr::opts_chunk$set(error = FALSE)
  knitr::knit("estilex.Rmd.orig", output = "estilex.Rmd", encoding = "UTF-8")
})

# knitr stops on error by default, but a chunk can still produce an error
# message in the output without halting under some settings. Check the
# result rather than trusting that it ran clean.
out <- readLines("vignettes/estilex.Rmd", warn = FALSE, encoding = "UTF-8")

bad <- grep("^#> *(Error|Warning in)", out, value = TRUE)
if (length(bad)) {
  warning("the knitted vignette contains errors or warnings:\n",
          paste(" ", bad, collapse = "\n"), call. = FALSE)
}

if (!any(grepl("Eesti Vabariigi p", out, fixed = TRUE))) {
  warning("the knitted vignette does not contain the expected document ",
          "title; check that the requests succeeded", call. = FALSE)
}

message("Wrote vignettes/estilex.Rmd (", length(out), " lines, ",
        format(Sys.time(), "%Y-%m-%d %H:%M"), ")")
message("Remember: devtools::build_readme(), then commit both files.")
