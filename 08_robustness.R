###############################################################################
# 08_robustness.R
# Robustness checks of the main child fixed-effects results (FE1 spec unless
# stated). Each block prints the schedule coefficients next to the main FE.
#
#  R1. Alternative measures of health-limiting conditions:
#      4-item index (general items only, 0-4) and any condition (0/1).
#  R2. Timing: schedule two years earlier (t-2, lag) and two years later
#      (t+2, lead), taken from the mother's panel by calendar wave (the old
#      code used the previous observed row, which could be 4+ years apart).
#  R3. Other job characteristics: multiple job holding and weekly hours
#      (1990-2012), as controls.
#  R4. Pooled contrast used in most prior work: any nonstandard vs day.
#  R5. Inverse probability weighting for TIME-VARYING observed confounding:
#      multinomial model of the schedule on lagged family/job characteristics;
#      stabilized weights, trimmed at the 1st/99th percentiles.
#  R6. Children observed at least 3 times.
#  R7. Excluding children whose mother was ever a physician (occ1990dd 84), and
#      excluding those whose mother was ever in any health occupation (84-106:
#      physicians, nurses, pharmacists, therapists...), which often involve
#      irregular or night schedules.
# Input : data/analysis.rds, data/mothers_panel.rds
# Output: results/tables/robustness.(csv|docx)
###############################################################################
source("code/00_setup.R")
setFixest_notes(FALSE)
d  <- readRDS(file.path(DATA, "analysis.rds"))
mp <- readRDS(file.path(DATA, "mothers_panel.rds"))

OUT <- c(hlc = "Health-limiting conditions", poor_health = "Mother-rated poor health",
         bpi = "Behavior Problems Index")
SMP <- c(hlc = "s_hlc", poor_health = "s_rating", bpi = "s_bpi")
res <- list()
add <- function(fit, check, outcome, pattern = "^shift") {
  ct <- coeftable(fit); r <- grep(pattern, rownames(ct))
  res[[length(res) + 1]] <<- tibble(check = check, outcome = outcome, term = rownames(ct)[r],
                                    b = ct[r, 1], se = ct[r, 2], p = ct[r, 4], N = nobs(fit))
}

# Main FE (reference point)
for (y in names(OUT)) add(feols(as.formula(paste(y, "~ shift | childID + child_age")),
                                d[d[[SMP[y]]], ], cluster = CLUSTER), "0. Main FE", OUT[y])

# R1 ---------------------------------------------------------------------------
x <- d[d$s_hlc, ] %>% mutate(hlc4 = activities + treatment + medicine + equipment)
add(feols(hlc4 ~ shift | childID + child_age, x, cluster = CLUSTER), "R1. 4-item index (0-4)", OUT["hlc"])
add(feols(any_condition ~ shift | childID + child_age, x, cluster = CLUSTER), "R1. Any condition (0/1)", OUT["hlc"])

# R2 ---------------------------------------------------------------------------
lagged <- mp %>% transmute(motherID, year = year + 2, shift_lag = shift)
led    <- mp %>% transmute(motherID, year = year - 2, shift_lead = shift)
dl <- d %>% left_join(lagged, by = c("motherID", "year")) %>% left_join(led, by = c("motherID", "year"))
for (y in names(OUT)) {
  x <- dl[dl[[SMP[y]]], ]
  add(feols(as.formula(paste(y, "~ shift_lag | childID + child_age")), x, cluster = CLUSTER),
      "R2. Schedule at t-2 (lag)", OUT[y], "^shift_lag")
  add(feols(as.formula(paste(y, "~ shift_lead | childID + child_age")), x, cluster = CLUSTER),
      "R2. Schedule at t+2 (lead)", OUT[y], "^shift_lead")
  joint <- feols(as.formula(paste(y, "~ shift + shift_lag + shift_lead | childID + child_age")), x, cluster = CLUSTER)
  add(joint, "R2. Joint model: schedule at t",   OUT[y], "^shift[A-Z]")
  add(joint, "R2. Joint model: schedule at t-2", OUT[y], "^shift_lag")
  add(joint, "R2. Joint model: schedule at t+2", OUT[y], "^shift_lead")
}

# R3 ---------------------------------------------------------------------------
for (y in names(OUT)) {
  x <- d[d[[SMP[y]]], ]
  add(feols(as.formula(paste(y, "~ shift + multi_job | childID + child_age")), x, cluster = CLUSTER),
      "R3. + multiple job holding", OUT[y])
  xh <- x %>% filter(year <= 2012, !is.na(hours_cat))
  add(feols(as.formula(paste(y, "~ shift | childID + child_age")), xh, cluster = CLUSTER),
      "R3. Hours sample (1990-2012), no hours control", OUT[y])
  add(feols(as.formula(paste(y, "~ shift + hours_cat | childID + child_age")), xh, cluster = CLUSTER),
      "R3. Hours sample, + weekly hours category", OUT[y])
}

# R4 ---------------------------------------------------------------------------
for (y in names(OUT)) {
  x <- d[d[[SMP[y]]], ]
  add(feols(as.formula(paste(y, "~ nonstandard + i(child_age) + child_female + race_child + educ + lone_mother + family_size + i(year)")),
            x, cluster = CLUSTER), "R4. Any nonstandard vs day: OLS", OUT[y], "^nonstandard")
  add(feols(as.formula(paste(y, "~ nonstandard | childID + child_age")), x, cluster = CLUSTER),
      "R4. Any nonstandard vs day: FE", OUT[y], "^nonstandard")
}

# R5 ---------------------------------------------------------------------------
# Propensity of each schedule given characteristics measured two years earlier
# (so they are not affected by the current schedule) plus child age and wave.
prev <- mp %>% transmute(motherID, year = year + 2, lone_prev = lone_mother, fam_prev = family_size,
                         loginc_prev = log_income, occ_prev = occ_group, shift_prev = shift)
dw <- d %>% left_join(prev, by = c("motherID", "year")) %>%
  mutate(loginc_prev_f = ifelse(is.na(loginc_prev), 0, loginc_prev), inc_prev_miss = as.integer(is.na(loginc_prev)),
         occ_prev = forcats::fct_na_value_to_level(occ_prev, "Missing"),
         shift_prev = forcats::fct_na_value_to_level(shift_prev, "Missing")) %>%
  filter(!is.na(lone_prev), !is.na(fam_prev))
ps <- nnet::multinom(shift ~ lone_prev + fam_prev + loginc_prev_f + inc_prev_miss + occ_prev + shift_prev +
                       educ + race_mother + factor(child_age) + factor(year), data = dw, trace = FALSE)
pr <- predict(ps, type = "probs")
p_obs <- pr[cbind(seq_len(nrow(dw)), match(as.character(dw$shift), colnames(pr)))]
marg <- prop.table(table(dw$shift))[as.character(dw$shift)]
dw$sw <- as.numeric(marg / p_obs)
lims <- quantile(dw$sw, c(.01, .99)); dw$sw <- pmin(pmax(dw$sw, lims[1]), lims[2])
say("R5. Stabilized IPW weights (trimmed 1/99)"); print(summary(dw$sw))
for (y in names(OUT)) {
  x <- dw[dw[[SMP[y]]], ]
  add(feols(as.formula(paste(y, "~ shift | childID + child_age")), x, cluster = CLUSTER),
      "R5. IPW sample, unweighted", OUT[y])
  add(feols(as.formula(paste(y, "~ shift | childID + child_age")), x, weights = ~sw, cluster = CLUSTER),
      "R5. IPW sample, weighted", OUT[y])
}

# R6 ---------------------------------------------------------------------------
for (y in names(OUT)) {
  x <- d[d[[SMP[y]]], ] %>% group_by(childID) %>% filter(n() >= 3) %>% ungroup()
  add(feols(as.formula(paste(y, "~ shift | childID + child_age")), x, cluster = CLUSTER),
      "R6. Children observed 3+ times", OUT[y])
}

# R7 ---------------------------------------------------------------------------
ever_phys   <- unique(d$childID[d$occ1990dd %in% 84])
ever_health <- unique(d$childID[d$occ1990dd %in% 84:106])
for (y in names(OUT)) {
  x <- d[d[[SMP[y]]], ]
  add(feols(as.formula(paste(y, "~ shift | childID + child_age")), x[!x$childID %in% ever_phys, ], cluster = CLUSTER),
      "R7. Excluding children of physicians", OUT[y])
  add(feols(as.formula(paste(y, "~ shift | childID + child_age")), x[!x$childID %in% ever_health, ], cluster = CLUSTER),
      "R7. Excluding children of health professionals", OUT[y])
}

# ---- Output ------------------------------------------------------------------
res <- bind_rows(res) %>% mutate(term = sub("^shift_lag|^shift_lead|^shift", "", term),
                                 term = ifelse(term == "nonstandard", "Any nonstandard", term))
write.csv(res, file.path(TAB, "robustness.csv"), row.names = FALSE)
fmt <- function(b, se, p) paste0(sprintf("%.3f", b), " (", sprintf("%.3f", se), ")",
                                 ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
wide <- res %>% mutate(est = fmt(b, se, p), check = paste0(check, " [N=", N, "]")) %>%
  select(outcome, check, term, est) %>% pivot_wider(names_from = term, values_from = est)
say("Robustness checks (child FE + child age FE unless stated; ref. regular day)")
print(as.data.frame(wide), right = FALSE)
save_docx(wide, file.path(TAB, "robustness.docx"), title = "Robustness checks",
          notes = "Child and child-age fixed effects unless stated. SEs clustered by mother. * p<.05, ** p<.01, *** p<.001.")
