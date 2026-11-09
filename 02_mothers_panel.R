###############################################################################
# 02_mothers_panel.R
# Builds a mother-by-wave panel (one row per mother and even-year wave,
# 1990-2018) with work schedule, employment, family and job characteristics.
# Input : data/raw_mothers.rds, data/raw_occupations.rds, NSWS/crosswalks
# Output: data/mothers_panel.rds
###############################################################################
source("code/00_setup.R")
m   <- readRDS(file.path(DATA, "raw_mothers.rds"))
occ <- readRDS(file.path(DATA, "raw_occupations.rds"))

m <- m %>% filter(SAMPLE_SEX_1979 == 2)          # women only

# CPI-U annual averages (BLS), used to express income in 2018 dollars.
# TNFI refers to the PREVIOUS calendar year, so income reported in wave y
# is deflated with the CPI of year y-1.
cpi <- c(`1989`=124.0, `1991`=136.2, `1993`=144.5, `1995`=152.4, `1997`=160.5,
         `1999`=166.6, `2001`=177.1, `2003`=184.0, `2005`=195.3, `2007`=207.3,
         `2009`=214.5, `2011`=224.9, `2013`=232.957, `2015`=237.017, `2017`=245.120)
CPI_2018 <- 251.107

# ---------------------------------------------------------------------------
# 1. Work schedule of the main job, wave by wave
# ---------------------------------------------------------------------------
# The question wording changes over time:
#  1990-1993: CPS-QES-86A.01   1=day 2=evening 3=night 4=rotating 5=split
#                              6=irregular 7=other
#  1994-2000: QES-86A.01       same codes (2000 adds 8 = irregular, arranged
#                              by employee)
#  2002-    : QES-86A_0.01     1 = "a regular day schedule", 2 = "some other"
#             QES-86A.01       (if "some other") 2-8 as above;
#                              9 = "a regular day shift" (2018-2020 only)
# "Other" (7) and any unlabelled code are set to missing.
shift_detail_for_year <- function(y) {
  if (y <= 1993) {
    code <- col_or_na(m, paste0("CPS-QES-86A.01_", y)); day_flag <- rep(NA, nrow(m))
  } else {
    code <- col_or_na(m, paste0("QES-86A.01_", y))
    day_flag <- if (y >= 2002) col_or_na(m, paste0("QES-86A_0.01_", y)) else rep(NA, nrow(m))
  }
  case_when(
    day_flag %in% 1 | code %in% c(1, 9) ~ "day",
    code == 2 ~ "evening",
    code == 3 ~ "night",
    code == 4 ~ "rotating",
    code == 5 ~ "split",
    code == 6 ~ "irregular",
    code == 8 ~ "irregular_employee",
    TRUE ~ NA_character_
  )
}

# ---------------------------------------------------------------------------
# 2. Reshape everything to long format (mother x wave)
# ---------------------------------------------------------------------------
panel <- lapply(WAVES, function(y) {
  yy <- sprintf("%02d", y %% 100)
  jobs_current <- sapply(1:5, function(k) col_or_na(m, sprintf("QES-23.%02d_%d", k, y)) == 1)
  jobs_hours   <- sapply(1:5, function(k) col_or_na(m, sprintf("QES-52A.%02d_%d", k, y)))
  hrs_current  <- ifelse(jobs_current %in% TRUE, jobs_hours, NA)   # hours only at current jobs
  dim(hrs_current) <- dim(jobs_hours)
  tibble(
    motherID       = m$CASEID_1979,
    year           = y,
    shift_detail   = shift_detail_for_year(y),
    employed       = col_or_na(m, paste0("DOI_EMPLOYED_", y)),
    job1_current   = col_or_na(m, paste0("QES-23.01_", y)),
    n_current_jobs = rowSums(jobs_current, na.rm = TRUE),
    hours_week     = ifelse(rowSums(!is.na(hrs_current)) == 0, NA, rowSums(hrs_current, na.rm = TRUE)),
    marstat        = col_or_na(m, paste0("MARSTAT-KEY_", y)),
    family_size    = col_or_na(m, paste0("FAMSIZE_", y)),
    n_children     = col_or_na(m, paste0("NUMCH", yy, "_", y)),
    income_nominal = col_or_na(m, paste0("TNFI_TRUNC_", y)),
    mother_health_limits_work = col_or_na(m, paste0("Q11-4_", y)),
    race_mother    = m$SAMPLE_RACE_78SCRN,
    educ_years     = m$HGC_EVER_XRND
  )
}) %>% bind_rows()

# QES-52A (hours per job) was not asked after 2012
panel$hours_week[panel$year > 2012] <- NA

# ---------------------------------------------------------------------------
# 3. Derived variables
# ---------------------------------------------------------------------------
panel <- panel %>%
  mutate(
    # Main 4-category schedule (rotating, split and irregular pooled)
    shift = case_when(
      shift_detail == "day"     ~ "Regular day",
      shift_detail == "evening" ~ "Regular evening",
      shift_detail == "night"   ~ "Regular night",
      shift_detail %in% c("rotating", "split", "irregular", "irregular_employee") ~ "Irregular",
      TRUE ~ NA_character_),
    shift = factor(shift, levels = c("Regular day", "Regular evening", "Regular night", "Irregular")),
    nonstandard = as.integer(shift != "Regular day"),

    # Family structure. MARSTAT-KEY: 0 never married, 1 married, 2 separated,
    # 3 divorced, 5 remarried, 6 widowed. FIX: the old code left out 2 and 3.
    lone_mother = case_when(marstat %in% c(0, 2, 3, 6) ~ 1L,
                            marstat %in% c(1, 5)       ~ 0L),

    # Real family income (2018 dollars) and its log
    income = income_nominal * CPI_2018 / cpi[as.character(year - 1)],
    log_income = log(income + 1),

    # Education (highest grade ever completed). FIX: 16 years used to be NA.
    educ = case_when(educ_years < 12 ~ "Less than high school",
                     educ_years == 12 ~ "High school",
                     educ_years %in% 13:15 ~ "Some college",
                     educ_years >= 16 ~ "College degree"),
    educ = factor(educ, levels = c("Less than high school", "High school",
                                   "Some college", "College degree")),
    race_mother = factor(race_mother, levels = c(3, 1, 2),
                         labels = c("Non-Black non-Hispanic", "Hispanic", "Black")),
    multi_job = as.integer(n_current_jobs >= 2),
    hours_cat = cut(hours_week, c(-Inf, 34, 44, Inf),
                    labels = c("Part time (<35)", "Full time (35-44)", "Long hours (45+)"))
  )

# ---------------------------------------------------------------------------
# 4. Occupation (job 1) -> occ1990dd -> broad groups
# ---------------------------------------------------------------------------
# 1990-2000 codes follow the 1970 Census scheme, 2002-2018 the 2000 Census
# scheme. Both are harmonised to occ1990dd (Dorn) with the crosswalks.
cw70 <- haven::read_dta(file.path(RAW, "crosswalks", "occ1970_occ1990dd.dta"))
cw00 <- haven::read_dta(file.path(RAW, "crosswalks", "occ2000_occ1990dd.dta"))
occ_long <- lapply(WAVES, function(y) {
  raw <- if (y <= 1993) occ[[paste0("CPSOCC70_", y)]] else occ[[paste0("OCCALL-EMP.01_", y)]]
  # From 2004 on, NLSY stores Census codes with 4 digits (e.g. 3600 = 360).
  # 2010+ waves use the 2010 Census list, which is almost identical to 2000
  # at the 3-digit level; codes that do not match the crosswalk stay missing.
  if (y >= 2004) raw <- floor(raw / 10)
  cw  <- if (y <= 2000) cw70 else cw00
  tibble(motherID = occ$CASEID_1979, year = y, occ_raw = raw) %>%
    left_join(cw, by = c("occ_raw" = "occ"))
}) %>% bind_rows()

# Broad groups using the occ1990dd code ranges (Autor & Dorn).
# FIX: the old code applied 1990-Census ranges to occ1990dd codes, which put
# e.g. nursing aides (447) in "craft" instead of "service".
occ_long <- occ_long %>%
  mutate(occ_group = case_when(
    occ1990dd >= 3   & occ1990dd <= 235 ~ "Managerial/professional/technical",
    occ1990dd >= 243 & occ1990dd <= 290 ~ "Sales",
    occ1990dd >= 303 & occ1990dd <= 389 ~ "Clerical/administrative",
    occ1990dd >= 405 & occ1990dd <= 472 ~ "Service",
    occ1990dd >= 473 & occ1990dd <= 889 ~ "Production/transport/farm",
    TRUE ~ NA_character_),
    occ_group = factor(occ_group, levels = c("Managerial/professional/technical", "Sales",
      "Clerical/administrative", "Service", "Production/transport/farm")))

panel <- panel %>% left_join(occ_long %>% select(motherID, year, occ1990dd, occ_group),
                             by = c("motherID", "year"))

# ---------------------------------------------------------------------------
# 5. Checks
# ---------------------------------------------------------------------------
say("Work schedule by wave (all women, incl. not employed)")
print(table(panel$year, panel$shift, useNA = "ifany"))
say("Employed vs schedule reported")
print(table(employed = panel$employed, has_shift = !is.na(panel$shift), useNA = "ifany"))
say("Lone mother share (rows with marital status)")
print(round(mean(panel$lone_mother, na.rm = TRUE), 3))
say("Education")
print(table(panel$educ[panel$year == 1990], useNA = "ifany"))
say("Occupation group (employed rows)")
print(table(panel$occ_group[panel$employed %in% 1], useNA = "ifany"))
say("Share of employed rows without occupation group, by wave")
print(round(tapply(is.na(panel$occ_group[panel$employed %in% 1]), panel$year[panel$employed %in% 1], mean), 2))

saveRDS(panel, file.path(DATA, "mothers_panel.rds"))
cat("\nSaved data/mothers_panel.rds:", nrow(panel), "rows\n")
