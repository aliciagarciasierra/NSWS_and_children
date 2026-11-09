###############################################################################
# 01_load_raw.R
# Reads the three NLS Investigator extracts and saves them as .rds files.
#   - NSWS.dat          mothers (NLSY79 main survey)
#   - NSWSchildren.dat  children (NLSY79 Children & Young Adults)
#   - occupations.dat   mothers' occupation codes, all years
# Output: data/raw_mothers.rds, data/raw_children.rds, data/raw_occupations.rds
###############################################################################
source("code/00_setup.R")

say("Mothers extract")
mothers <- read_nls(file.path(RAW, "NSWS.dat"), file.path(RAW, "NSWS.R"))
cat("rows:", nrow(mothers), " columns:", ncol(mothers), "\n")
cat("women (SAMPLE_SEX_1979 == 2):", sum(mothers$SAMPLE_SEX_1979 == 2, na.rm = TRUE), "\n")
saveRDS(mothers, file.path(DATA, "raw_mothers.rds"))

say("Children extract")
children <- read_nls(file.path(RAW, "NSWSchildren.dat"), file.path(RAW, "NSWSchildren.R"))
cat("rows (children):", nrow(children), " columns:", ncol(children), "\n")
saveRDS(children, file.path(DATA, "raw_children.rds"))

say("Occupations extract")
# The occupations extract has no qnames() helper; its column names (in order)
# are listed in the old script NSWS/03_PREPARING_OCCUPATIONS_VARIABLE.R.
occ <- read.table(file.path(RAW, "occupations.dat"), sep = " ")
names(occ) <- c("CASEID_1979", "CPSOCC70_1979", "SAMPLE_ID_1979", "SAMPLE_RACE_78SCRN",
                "SAMPLE_SEX_1979", paste0("CPSOCC70_", 1980:1993),
                paste0("OCCALL-EMP.01_", c(1994, 1996, 1998, 2000, 2002, 2004, 2006,
                                            2008, 2010, 2012, 2014, 2016, 2018)))
occ[occ < 0] <- NA
cat("rows:", nrow(occ), " columns:", ncol(occ), "\n")
saveRDS(occ, file.path(DATA, "raw_occupations.rds"))
