###############################################################################
# 09_selection.R
# Is the association between maternal schedules and child outcomes driven by
# SELECTION (who works these schedules) rather than by the schedules?
#
#  A. Sequential OLS + Gelbach decomposition: which observed characteristics
#     explain the change in the schedule coefficient as controls are added?
#  B. Mundlak (correlated random effects): formal test of whether the
#     BETWEEN-child and WITHIN-child associations are equal.
#  C. Were children already different BEFORE their mother changed schedule?
#     C1: pre-switch levels; C2: event study around the switch.
#  D. Reverse causality: does a child's health/behavior predict that the
#     mother moves from a day schedule into a nonstandard one two years later?
#  E. Power and equivalence: are the FE estimates precise enough to rule out
#     effects as large as the OLS associations?
#
# Input : data/analysis.rds, data/children_panel.rds, data/mothers_panel.rds
# Output: results/tables/sel_*.csv (+ .docx), results/figures/fig3_event_study.*
###############################################################################
source("code/00_setup.R")
library(ggplot2)
set.seed(20261109)
setFixest_notes(FALSE)
N_BOOT <- 200   # cluster-bootstrap replications for the Gelbach SEs
d <- readRDS(file.path(DATA, "analysis.rds")) %>%
  mutate(occ_group_f = forcats::fct_na_value_to_level(occ_group, "Missing"),
         d_even = as.integer(shift == "Regular evening"),
         d_night = as.integer(shift == "Regular night"),
         d_irreg = as.integer(shift == "Irregular"))

OUT <- c(hlc = "Health-limiting conditions", poor_health = "Mother-rated poor health",
         bpi = "Behavior Problems Index")
SMP <- c(hlc = "s_hlc", poor_health = "s_rating", bpi = "s_bpi")
SCHED <- c("Regular evening", "Regular night", "Irregular")
get_shift <- function(fit) {           # schedule coefficients from a fixest model
  ct <- coeftable(fit); r <- grep("^shift", rownames(ct))
  tibble(term = sub("^shift", "", rownames(ct)[r]), b = ct[r, 1], se = ct[r, 2], p = ct[r, 4])
}
fmt <- function(b, se, p) paste0(sprintf("%.3f", b), " (", sprintf("%.3f", se), ")",
                                 ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))

# =============================================================================
# A. Sequential OLS and Gelbach decomposition
# =============================================================================
# All models: same sample, child age and wave fixed effects. Controls are
# added in groups. Gelbach (2016) splits the change in the schedule
# coefficient (base -> full OLS) exactly into the part due to each group:
#   contribution of group g = sum over covariates x in g of
#      [coef of schedule in a regression of x on schedule] * [coef of x in full model]
groups <- list(
  "Child sex and race"     = c("child_female", "race_child"),
  "Mother's education"     = c("educ"),
  "Family structure/income"= c("lone_mother", "family_size", "log_income_f", "income_missing"),
  "Mother's occupation"    = c("occ_group_f"))

gelbach <- function(x, y) {
  full_rhs <- paste(unlist(groups), collapse = " + ")
  # fixef.rm = "none": keep every observation so all models use identical rows
  full <- feols(as.formula(paste(y, "~ shift +", full_rhs, "| child_age + year")), x, fixef.rm = "none")
  base <- feols(as.formula(paste(y, "~ shift | child_age + year")), x, fixef.rm = "none")
  X <- model.matrix(full, type = "rhs"); X <- X[, !grepl("^shift", colnames(X)), drop = FALSE]
  gam <- coef(full)[colnames(X)]
  # coefficient of each schedule dummy in the auxiliary regressions
  aux <- sapply(colnames(X), function(v) {
    xx <- x; xx$.xv <- X[, v]
    coef(feols(.xv ~ shift | child_age + year, xx, fixef.rm = "none"))[paste0("shift", SCHED)]
  })                                     # rows = schedules, cols = covariates
  owner <- sapply(colnames(X), function(v) names(groups)[sapply(groups, function(g) any(startsWith(v, g)))][1])
  contrib <- sapply(names(groups), function(g) aux[, owner == g, drop = FALSE] %*% gam[owner == g])
  rownames(contrib) <- SCHED
  list(base = coef(base)[paste0("shift", SCHED)], full = coef(full)[paste0("shift", SCHED)], contrib = contrib)
}

seqA <- list(); gelA <- list()
for (y in names(OUT)) {
  x <- d[d[[SMP[y]]], ]
  steps <- list(
    "1. Age + wave FE" = "shift | child_age + year",
    "2. + child sex, race" = "shift + child_female + race_child | child_age + year",
    "3. + mother's education" = "shift + child_female + race_child + educ | child_age + year",
    "4. + family structure, income" = "shift + child_female + race_child + educ + lone_mother + family_size + log_income_f + income_missing | child_age + year",
    "5. + occupation" = "shift + child_female + race_child + educ + lone_mother + family_size + log_income_f + income_missing + occ_group_f | child_age + year",
    "6. Child fixed effects" = "shift | childID + child_age")
  seqA[[y]] <- lapply(names(steps), function(s)
    get_shift(feols(as.formula(paste(y, "~", steps[[s]])), x, cluster = CLUSTER)) %>% mutate(step = s)) %>%
    bind_rows() %>% mutate(outcome = OUT[y])

  g <- gelbach(x, y)
  # cluster bootstrap (by mother) for the Gelbach components
  by_mom <- split(x, x$motherID)
  boots <- replicate(N_BOOT, {
    bm <- sample(names(by_mom), replace = TRUE)
    xb <- bind_rows(by_mom[bm], .id = "draw")
    gb <- tryCatch(gelbach(xb, y), error = function(e) NULL)
    if (is.null(gb)) rep(NA, length(SCHED) * length(groups)) else as.vector(gb$contrib)
  })
  se <- matrix(apply(boots, 1, sd, na.rm = TRUE), nrow = length(SCHED))
  gelA[[y]] <- tibble(outcome = OUT[y], schedule = rep(SCHED, length(groups)),
                      group = rep(names(groups), each = length(SCHED)),
                      contribution = as.vector(g$contrib), se = as.vector(se)) %>%
    left_join(tibble(schedule = SCHED, b_base = g$base, b_full = g$full), by = "schedule") %>%
    mutate(share_of_change = contribution / (b_base - b_full))
}
seqA <- bind_rows(seqA); gelA <- bind_rows(gelA)
write.csv(seqA, file.path(TAB, "sel_A_sequential.csv"), row.names = FALSE)
write.csv(gelA, file.path(TAB, "sel_A_gelbach.csv"), row.names = FALSE)

say("A. Sequential models (schedule coefficients, ref. regular day)")
seq_wide <- seqA %>% mutate(est = fmt(b, se, p)) %>% select(outcome, term, step, est) %>%
  pivot_wider(names_from = step, values_from = est)
print(as.data.frame(seq_wide), right = FALSE)
save_docx(seq_wide, file.path(TAB, "sel_A_sequential.docx"),
          title = "Sequential models: schedule coefficients as controls are added",
          notes = "Same sample in all columns. SEs clustered by mother. * p<.05, ** p<.01, *** p<.001.")
say("A. Gelbach decomposition of (step 1 - step 5)")
print(gelA %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())

# =============================================================================
# B. Mundlak / correlated random effects
# =============================================================================
# y = b_W * schedule + c * (child's mean of schedule dummies) + age FE + controls
# b_W = within-child association (identical to the child FE estimate)
# b_B = b_W + c = between-child association
# H0: c = 0  <=>  between = within. Rejecting H0 is direct evidence that
# children whose mothers work a schedule differ in stable ways (selection).
mundB <- lapply(names(OUT), function(y) {
  x <- d[d[[SMP[y]]], ] %>% group_by(childID) %>%
    mutate(m_even = mean(d_even), m_night = mean(d_night), m_irreg = mean(d_irreg)) %>% ungroup()
  fit <- feols(as.formula(paste(y, "~ d_even + d_night + d_irreg + m_even + m_night + m_irreg | child_age")),
               x, cluster = CLUSTER)
  b <- coef(fit); V <- vcov(fit)
  lapply(list(c("d_even", "m_even"), c("d_night", "m_night"), c("d_irreg", "m_irreg")), function(v) {
    within <- b[v[1]]; c_ <- b[v[2]]
    between <- within + c_
    se_between <- sqrt(V[v[1], v[1]] + V[v[2], v[2]] + 2 * V[v[1], v[2]])
    tibble(outcome = OUT[y], schedule = c(d_even = "Regular evening", d_night = "Regular night", d_irreg = "Irregular")[v[1]],
           within = within, se_within = sqrt(V[v[1], v[1]]), between = between, se_between = se_between,
           difference = c_, se_difference = sqrt(V[v[2], v[2]]), p_equal = 2 * pnorm(-abs(c_ / sqrt(V[v[2], v[2]]))))
  }) %>% bind_rows() %>%
    mutate(p_joint_all_equal = wald(fit, keep = "^m_", print = FALSE)$p)
}) %>% bind_rows()
write.csv(mundB, file.path(TAB, "sel_B_mundlak.csv"), row.names = FALSE)
say("B. Mundlak: between vs within associations")
print(mundB %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
save_docx(mundB %>% mutate(across(where(is.numeric), ~ round(.x, 3))),
          file.path(TAB, "sel_B_mundlak.docx"),
          title = "Between-child vs within-child associations (Mundlak model)",
          notes = "Within = child fixed-effects estimate; between = within + coefficient on the child's mean of the schedule dummy. p_equal tests between = within. Child age FE; SEs clustered by mother.")

# =============================================================================
# C. Were children different BEFORE the mother changed schedule?
# =============================================================================
# Children observed with the mother on a regular day schedule at their first
# observation. "Switchers into X": mother later moves directly from day to X.
# "Stayers": mother on day schedule at every observation.
build_events <- function(x, target) {
  x %>% arrange(childID, year) %>% group_by(childID) %>%
    mutate(prev = lag(shift),
           switch_here = shift == target & prev %in% "Regular day",
           first_day = first(shift) == "Regular day",
           always_day = all(shift == "Regular day"),
           switch_year = if (any(switch_here, na.rm = TRUE)) min(year[switch_here %in% TRUE]) else NA_real_,
           pre_switch_all_day = if (!is.na(first(switch_year))) all(shift[year < first(switch_year)] == "Regular day") else FALSE) %>%
    ungroup() %>%
    filter(first_day, always_day | (!is.na(switch_year) & pre_switch_all_day)) %>%
    mutate(switcher = as.integer(!is.na(switch_year)),
           event_time = ifelse(switcher == 1, (year - switch_year) / 2, NA))
}

# C1: pre-switch levels (observations BEFORE the switch, mother on day schedule)
preC <- lapply(names(OUT), function(y) lapply(SCHED, function(s) {
  ev <- build_events(d[d[[SMP[y]]], ], s) %>% filter(switcher == 0 | year < switch_year)
  fit <- feols(as.formula(paste(y, "~ switcher + child_female + race_child + educ + lone_mother + family_size | child_age + year")),
               ev, cluster = CLUSTER)
  ct <- coeftable(fit)["switcher", ]
  tibble(outcome = OUT[y], schedule = s, n_switch_children = n_distinct(ev$childID[ev$switcher == 1]),
         n_stayer_children = n_distinct(ev$childID[ev$switcher == 0]),
         pre_switch_difference = ct[1], se = ct[2], p = ct[4],
         raw_mean_switchers = mean(ev[[y]][ev$switcher == 1]), raw_mean_stayers = mean(ev[[y]][ev$switcher == 0]))
}) %>% bind_rows()) %>% bind_rows()
write.csv(preC, file.path(TAB, "sel_C1_preswitch_levels.csv"), row.names = FALSE)
say("C1. Differences BEFORE the switch (switchers-to-be vs stayers on day schedule)")
print(preC %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())

# C2: event study with child FE (switch into irregular; HLC and BPI)
esC <- lapply(c("hlc", "bpi"), function(y) lapply(c("Irregular", "Regular evening"), function(s) {
  ev <- build_events(d[d[[SMP[y]]], ], s) %>%
    mutate(et = case_when(switcher == 0 ~ "-1",
                          event_time <= -3 ~ "-3+", event_time >= 2 ~ "2+",
                          TRUE ~ as.character(event_time)),
           et = factor(et, levels = c("-3+", "-2", "-1", "0", "1", "2+")))
  fit <- feols(as.formula(paste(y, "~ i(et, ref = '-1') | childID + child_age")), ev, cluster = CLUSTER)
  ct <- coeftable(fit)
  tibble(outcome = OUT[y], schedule = s, et = sub("^et::", "", rownames(ct)), b = ct[, 1], se = ct[, 2], p = ct[, 4]) %>%
    bind_rows(tibble(outcome = OUT[y], schedule = s, et = "-1", b = 0, se = 0, p = NA))
}) %>% bind_rows()) %>% bind_rows() %>%
  mutate(et = factor(et, levels = c("-3+", "-2", "-1", "0", "1", "2+")))
write.csv(esC, file.path(TAB, "sel_C2_event_study.csv"), row.names = FALSE)
say("C2. Event study (ref = observation just before the switch)")
print(esC %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% arrange(outcome, schedule, et) %>% as.data.frame())

p3 <- ggplot(esC, aes(x = et, y = b, group = 1)) +
  geom_hline(yintercept = 0, colour = "grey30", linetype = "dashed") +
  geom_vline(xintercept = 3.5, colour = "grey75", linetype = "dotted") +
  geom_errorbar(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), width = 0, colour = "grey50") +
  geom_point(size = 2, colour = "black") + geom_line(colour = "black") +
  facet_wrap(outcome ~ schedule, scales = "free_y", nrow = 2) +
  labs(x = "Observations (2-year waves) relative to the mother's move from a day schedule",
       y = "Difference relative to the observation before the move") +
  theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
ggsave(file.path(FIG, "fig3_event_study.png"), p3, width = 9, height = 6, dpi = 300, bg = "white")
ggsave(file.path(FIG, "fig3_event_study.pdf"), p3, width = 9, height = 6)

# =============================================================================
# D. Reverse causality: does the child's outcome predict a schedule change?
# =============================================================================
# Pairs of consecutive waves (t, t+2) in which the mother works a regular day
# schedule at t and is employed with a valid schedule at t+2. Outcome: she
# works evening / night / irregular at t+2. Predictor: child outcome at t,
# standardized (1 SD). Coefficients = change in the probability (pp) of moving.
mp <- readRDS(file.path(DATA, "mothers_panel.rds")) %>% select(motherID, year, next_shift = shift, next_employed = employed) %>%
  mutate(year = year - 2)
revD <- lapply(names(OUT), function(y) {
  x <- d[d[[SMP[y]]] & d$shift == "Regular day", ] %>% left_join(mp, by = c("motherID", "year")) %>%
    filter(next_employed %in% 1, !is.na(next_shift)) %>%
    mutate(z = (.data[[y]] - mean(.data[[y]])) / sd(.data[[y]]))
  lapply(SCHED, function(s) {
    x$move <- 100 * as.integer(x$next_shift == s)
    ols <- feols(move ~ z + child_female + race_child + educ + lone_mother + family_size | child_age + year, x, cluster = CLUSTER)
    fe  <- feols(move ~ z + lone_mother + family_size | childID + child_age, x, cluster = CLUSTER)
    tibble(outcome = OUT[y], moves_to = s, n_pairs = nobs(ols), base_rate_pct = mean(x$move),
           ols_b = coef(ols)["z"], ols_se = se(ols)["z"], ols_p = pvalue(ols)["z"],
           fe_b = coef(fe)["z"], fe_se = se(fe)["z"], fe_p = pvalue(fe)["z"])
  }) %>% bind_rows()
}) %>% bind_rows()
write.csv(revD, file.path(TAB, "sel_D_reverse_causality.csv"), row.names = FALSE)
say("D. Does a 1-SD higher child outcome at t predict the mother moving from day to X at t+2? (pp)")
print(revD %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())

# =============================================================================
# E. Power and equivalence of the FE estimates
# =============================================================================
# MDE (80% power, 5% two-sided) = 2.8 x SE. Equivalence (TOST) against a
# bound of +-0.10 SD of the outcome: if both one-sided tests reject, the FE
# estimate is "practically zero". We also check whether the FE 95% CI
# excludes the OLS point estimate.
eqE <- lapply(names(OUT), function(y) {
  x <- d[d[[SMP[y]]], ]
  sdy <- sd(x[[y]])
  ols <- get_shift(feols(as.formula(paste(y, "~ shift + i(child_age) + child_female + race_child + educ + lone_mother + family_size + i(year)")), x, cluster = CLUSTER))
  fe  <- get_shift(feols(as.formula(paste(y, "~ shift | childID + child_age")), x, cluster = CLUSTER))
  fe %>% rename(fe_b = b, fe_se = se, fe_p = p) %>% left_join(ols %>% select(term, ols_b = b), by = "term") %>%
    mutate(outcome = OUT[y], sd_outcome = sdy,
           fe_ci_low = fe_b - 1.96 * fe_se, fe_ci_high = fe_b + 1.96 * fe_se,
           mde80 = 2.8 * fe_se, mde80_in_sd = mde80 / sdy,
           ols_outside_fe_ci = ols_b < fe_ci_low | ols_b > fe_ci_high,
           bound = 0.10 * sdy,
           p_tost = pmax(pnorm((fe_b - (-bound)) / fe_se, lower.tail = FALSE),
                         pnorm((fe_b - bound) / fe_se)),
           equivalent_at_0.1sd = p_tost < .05)
}) %>% bind_rows() %>% select(outcome, term, everything())
write.csv(eqE, file.path(TAB, "sel_E_equivalence.csv"), row.names = FALSE)
say("E. Power and equivalence of FE estimates")
print(eqE %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\nDone: results/tables/sel_*.csv, results/figures/fig3_event_study.*\n")
