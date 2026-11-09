###############################################################################
# 03_children_panel.R
# Builds a child-by-wave panel (even years 1990-2018) with the three outcomes:
#   1. Health-limiting conditions (count of 6 mother-reported items)
#   2. Mother-rated poor health (1 = excellent ... 4 = poor; reverse-coded
#      from the original item so that higher = worse, like the other outcomes)
#   3. Behavior Problems Index (BPI) total standard score
#      (mean 100, SD 15 in the norming sample; HIGHER = MORE problems)
# Input : data/raw_children.rds
# Output: data/children_panel.rds
###############################################################################
source("code/00_setup.R")
k <- readRDS(file.path(DATA, "raw_children.rds"))

# ---------------------------------------------------------------------------
# 1. Question names of the six health items, by wave
#    (2A limits school attendance, 2B limits schoolwork, 2C limits play/usual
#     activities, 3A requires medical treatment, 3B requires medicine,
#     3C requires special equipment)
# ---------------------------------------------------------------------------
health_items <- function(y) {
  if (y == 1990) return(paste0("CS9016", c(31, 33, 35, 37, 39, 41), "_1990"))
  if (y == 1992) return(paste0("CS9217", c(58, 60, 62, 64, 66, 68), "_1992"))
  suffix <- c("2A", "2B", "2C", "3A", "3B", "3C")
  if (y <= 1998) return(paste0("CS", y %% 100, "H-", suffix, "_", y))
  if (y <= 2004) return(paste0("HLTH-", suffix, "_", y))
  paste0("MS-HLTH-", suffix, "_", y)
}
health_rating_var <- function(y) {
  switch(as.character(y),
         "1990" = NA, "1992" = "MS921515_1992", "1994" = "MS941711_1994",
         "1996" = "MS961711_1996", "1998" = "MS985019A_1998",
         if (y <= 2004) paste0("BKGN-46A_", y) else paste0("MS-BKGN-46A_", y))
}

panel <- lapply(WAVES, function(y) {
  it <- health_items(y)
  items <- sapply(it, function(v) col_or_na(k, v))
  colnames(items) <- c("school_attend", "school_work", "activities",
                       "treatment", "medicine", "equipment")
  hr <- health_rating_var(y)
  tibble(
    childID   = k$CPUBID_XRND,
    motherID  = k$MPUBID_XRND,
    year      = y,
    child_age = y - k$CYRB_XRND,
    child_female = as.integer(k$CSEX_XRND == 2),
    race_child = k$CRACE_XRND,
    !!!as_tibble(items),
    health_rating = if (is.na(hr)) NA_real_ else col_or_na(k, hr),
    bpi  = col_or_na(k, paste0("BPIZ", y, "_", y)),
    home = col_or_na(k, paste0("HOMEZ", y, "_", y))
  )
}) %>% bind_rows()

# ---------------------------------------------------------------------------
# 2. Health-limiting conditions index (0-6)
# ---------------------------------------------------------------------------
# FIX: the old code summed the items with na.rm = TRUE, so a child with all six
# items missing (not assessed that wave) got a score of 0 = "perfect health".
# Rule now:
#  - the four general items (activities, treatment, medicine, equipment) must
#    all be answered; otherwise the index is missing;
#  - the two school items count as 0 when the answer is "no", when the child
#    does not attend school (code 2, 1990-1998) or when they are not asked.
panel <- panel %>%
  mutate(
    across(c(school_attend, school_work), ~ ifelse(.x %in% 1, 1, 0)),
    core_answered = !is.na(activities) & !is.na(treatment) & !is.na(medicine) & !is.na(equipment),
    hlc = ifelse(core_answered,
                 school_attend + school_work + activities + treatment + medicine + equipment,
                 NA_real_),
    any_condition = as.integer(hlc > 0),
    health_rating = ifelse(health_rating %in% 1:4, health_rating, NA),
    # Reverse-coded so that, like the other two outcomes, HIGHER = WORSE:
    # 1 = excellent, 2 = good, 3 = fair, 4 = poor
    poor_health = 5 - health_rating
  )

# Keep children aged 3-14 (ages for which the outcomes are collected)
panel <- panel %>% filter(child_age >= AGE_MIN, child_age <= AGE_MAX)

# ---------------------------------------------------------------------------
# 3. Checks
# ---------------------------------------------------------------------------
say("Child-years aged 3-14, by wave, and how many have each outcome")
print(panel %>% group_by(year) %>%
        summarise(child_years = n(), hlc = sum(!is.na(hlc)),
                  poor_health = sum(!is.na(poor_health)), bpi = sum(!is.na(bpi))),
      n = Inf)
say("Distribution of the health-limiting conditions index")
print(table(panel$hlc, useNA = "ifany"))
say("Mother-rated poor health (1 excellent - 4 poor)")
print(table(panel$poor_health, useNA = "ifany"))
say("BPI standard score")
print(summary(panel$bpi))

saveRDS(panel, file.path(DATA, "children_panel.rds"))
cat("\nSaved data/children_panel.rds:", nrow(panel), "rows\n")
