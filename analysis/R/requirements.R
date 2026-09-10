# R dependencies of analysis/src/d02_results/*.R - installs what is missing.
# R itself is the system R (>= 4.4); Python lives in the conda env (environment.yml).
pkgs <- c("arrow", "data.table", "fixest", "contdid", "ggplot2")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing R packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
