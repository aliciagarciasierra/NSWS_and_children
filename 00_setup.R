###############################################################################
# 00_setup.R
# Common setup sourced at the top of every script: packages, paths, helpers.
#
# HOW TO RUN: open ANALYSIS_NOV2026.Rproj in RStudio (this sets the working
# directory to the ANALYSIS_NOV2026 folder) and run code/run_all.R, or run the
# numbered scripts one by one.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(fixest)
  library(modelsummary)
})

# ---- Paths (relative to the ANALYSIS_NOV2026 folder) -----------------------
RAW  <- file.path("..", "NSWS")        # original NLS extracts (read only)
DATA <- "data"                         # intermediate datasets created here
TAB  <- file.path("results", "tables")
FIG  <- file.path("results", "figures")
for (d in c(DATA, TAB, FIG)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# ---- Analysis window ---------------------------------------------------------
# Child assessments (NLSY79-CYA) take place in even years. We use every
# even-year wave from 1990 to 2018 in which mothers report their work schedule.
# NOTE: the old code skipped 2006 by mistake.
WAVES <- seq(1990, 2018, by = 2)
AGE_MIN <- 3
AGE_MAX <- 14

# ---- Helper: read an NLS Investigator extract --------------------------------
# NLS Investigator gives a .dat file (numbers only) plus an .R file whose
# qnames() function lists the question names in column order. We read the
# names straight from that .R file, so the column order can never get out of
# sync, and we set every negative NLS code to NA:
#   -1 refused, -2 don't know, -3 invalid skip, -4 valid skip,
#   -5 non-interview, -7 missing
read_nls <- function(dat_file, r_file) {
  dat <- read.table(dat_file, sep = " ")
  txt <- paste(readLines(r_file, warn = FALSE, encoding = "latin1"), collapse = "\n")
  start <- regexpr("qnames = function", txt)
  stop_ <- regexpr("return\\(data\\)", substring(txt, start)) + start
  block <- substring(txt, start, stop_)
  qn <- regmatches(block, gregexpr('"[^"]+"', block))[[1]]
  qn <- gsub('"', "", qn)
  stopifnot(length(qn) == ncol(dat))
  names(dat) <- qn
  dat[dat < 0] <- NA
  dat
}

# Safe column getter: returns the column if it exists, otherwise NAs
col_or_na <- function(df, name) if (name %in% names(df)) df[[name]] else rep(NA_real_, nrow(df))

# ---- Model settings ----------------------------------------------------------
# Standard errors are clustered by MOTHER in every model: siblings share a
# mother, and each child is observed repeatedly. (The old code used iid SEs.)
CLUSTER <- ~motherID
STARS <- c("*" = .05, "**" = .01, "***" = .001)

# Save a data frame as a Word table (flextable). Used for all .docx outputs.
save_docx <- function(df, path, title = NULL, notes = NULL) {
  ft <- flextable::flextable(df)
  ft <- flextable::autofit(ft)
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  if (!is.null(title)) ft <- flextable::set_caption(ft, title)
  if (!is.null(notes)) ft <- flextable::add_footer_lines(ft, notes)
  flextable::save_as_docx(ft, path = path,
                          pr_section = officer::prop_section(page_size = officer::page_size(orient = "landscape")))
}

# Small helper to print a section header in the console / log
say <- function(...) cat("\n====", ..., "====\n")
