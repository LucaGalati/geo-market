# R dependencies of analysis/src/d02_results/*.R - installs what is missing.
# R itself is the system R (>= 4.4); Python lives in the conda env (environment.yml).
# fixest >= 0.14 is required: 0.13.2 corrupts the heap in its demeaning code on this panel
# (R aborts with SIGTRAP inside demean_acc_gnl during the pre-trend regressions).
pkgs <- c("arrow", "data.table", "fixest", "contdid", "ggplot2")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (!"fixest" %in% missing && packageVersion("fixest") < "0.14.0") missing <- c(missing, "fixest")
if (length(missing)) {
  message("Installing missing R packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
