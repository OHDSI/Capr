# Force compile packages from source before installing
options(
  repos = c(CRAN = "https://cran.rstudio.com"),
  install.packages.compile.from.source = TRUE,
  pkgType = "source"
)

if (any(list.files() == "renv")) {
  source("renv/activate.R")
} else {
  message("No renv initialized, calling renv::init()")
  message("Please select to use only the DESCRIPTION file for dependency discovery if prompted, and next select 'Restore the project from the lockfile'")
  renv::init()
}