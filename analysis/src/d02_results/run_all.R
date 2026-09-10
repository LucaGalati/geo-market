# Run every regression block on the headline (matched) and appendix (main) samples, and
# the balanced / entropy-balanced variants internally. Tables: analysis/output/tables/regressions/.
HERE <- (function() { a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
                      if (length(f)) dirname(normalizePath(f)) else getwd() })()
source(file.path(HERE, "common.R"))
D_MAIN <- load_panel("main")
D_BAL <- load_panel("balanced")
for (block in c("descriptives", "pretrends", "baseline", "dose_response", "horserace", "mechanism", "intraday")) {
  cat(sprintf("\n=== %s  %s ===\n", block, format(Sys.time(), "%H:%M:%S")))
  t0 <- Sys.time()
  source(file.path(HERE, paste0(block, ".R")), local = TRUE)
  cat(sprintf("=== %s done in %.0f s\n", block, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
cat("\nall regression blocks done\n")
