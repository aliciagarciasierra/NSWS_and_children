###############################################################################
# 07_heterogeneity.R  (secondary analysis)
# Do the within-child associations differ across groups? Child FE + child age
# FE models with schedule x moderator interactions, one moderator at a time.
#
# Moderators (all fixed over time, so the model only adds interactions):
#   - mother's education (high school or less / some college / college degree)
#   - mother's race/ethnicity
#   - child sex
#   - child age group (3-5, 6-10, 11-14)  [time-varying, enters as main effect too]
#   - family income tercile (mother's average real income over the panel)
#
# Because many interaction terms are tested, we report (i) a joint Wald test of
# all interactions in each model and (ii) p-values adjusted for multiple
# comparisons (Benjamini-Hochberg) across all interaction terms.
# Input : data/analysis.rds
# Output: results/tables/het_interactions.csv, het_wald_tests.(csv|docx),
#         het_group_effects.csv
###############################################################################
source("code/00_setup.R")
setFixest_notes(FALSE)
d <- readRDS(file.path(DATA, "analysis.rds"))

inc_mother <- d %>% group_by(motherID) %>% summarise(mean_inc = mean(income, na.rm = TRUE)) %>%
  mutate(inc_tercile = factor(ntile(ifelse(is.nan(mean_inc), NA, mean_inc), 3),
                              labels = c("Low income", "Middle income", "High income")))
d <- d %>% left_join(inc_mother %>% select(motherID, inc_tercile), by = "motherID") %>%
  mutate(educ3 = factor(case_when(educ %in% c("Less than high school", "High school") ~ "High school or less",
                                  educ == "Some college" ~ "Some college",
                                  educ == "College degree" ~ "College degree"),
                        levels = c("High school or less", "Some college", "College degree")),
         child_sex = factor(ifelse(child_female == 1, "Girl", "Boy"), levels = c("Boy", "Girl")),
         age_group = cut(child_age, c(2, 5, 10, 14), labels = c("Age 3-5", "Age 6-10", "Age 11-14")))

OUT <- c(hlc = "Health-limiting conditions", poor_health = "Mother-rated poor health",
         bpi = "Behavior Problems Index")
SMP <- c(hlc = "s_hlc", poor_health = "s_rating", bpi = "s_bpi")
MOD <- c(educ3 = "Mother's education", race_mother = "Mother's race/ethnicity",
         child_sex = "Child sex", age_group = "Child age group", inc_tercile = "Family income tercile")

inter <- list(); wald_t <- list(); groupfx <- list()
for (y in names(OUT)) for (m in names(MOD)) {
  x <- d[d[[SMP[y]]] & !is.na(d[[m]]), ]
  extra <- if (m == "age_group") " + age_group" else ""
  fit <- feols(as.formula(paste0(y, " ~ shift * ", m, extra, " | childID + child_age")), x, cluster = CLUSTER)
  ct <- coeftable(fit)
  ii <- grep(":", rownames(ct))
  inter[[length(inter) + 1]] <- tibble(outcome = OUT[y], moderator = MOD[m], term = rownames(ct)[ii],
                                       b = ct[ii, 1], se = ct[ii, 2], p = ct[ii, 4])
  w <- wald(fit, keep = ":", print = FALSE)
  wald_t[[length(wald_t) + 1]] <- tibble(outcome = OUT[y], moderator = MOD[m],
                                         n_interactions = length(ii), F = w$stat, p_joint = w$p)
  # schedule effect within each group = main effect + interaction
  lv <- levels(factor(x[[m]]))
  for (g in lv) for (s in c("Regular evening", "Regular night", "Irregular")) {
    main <- paste0("shift", s); int <- paste0("shift", s, ":", m, g)
    L <- setNames(rep(0, length(coef(fit))), names(coef(fit))); L[main] <- 1
    if (int %in% names(L)) L[int] <- 1
    est <- sum(L * coef(fit)); se_ <- sqrt(as.numeric(t(L) %*% vcov(fit) %*% L))
    groupfx[[length(groupfx) + 1]] <- tibble(outcome = OUT[y], moderator = MOD[m], group = g, schedule = s,
                                             b = est, se = se_, p = 2 * pnorm(-abs(est / se_)))
  }
}
inter <- bind_rows(inter) %>% mutate(p_bh = p.adjust(p, method = "BH"))
wald_t <- bind_rows(wald_t)
groupfx <- bind_rows(groupfx)
write.csv(inter, file.path(TAB, "het_interactions.csv"), row.names = FALSE)
write.csv(wald_t, file.path(TAB, "het_wald_tests.csv"), row.names = FALSE)
write.csv(groupfx, file.path(TAB, "het_group_effects.csv"), row.names = FALSE)

say("Joint Wald tests: do the schedule associations differ across groups?")
print(wald_t %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
say("Interaction terms with p < .05 (unadjusted), and BH-adjusted p")
print(inter %>% filter(p < .05) %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\nInteraction terms estimated:", nrow(inter), " significant at .05:", sum(inter$p < .05),
    " expected by chance:", round(.05 * nrow(inter), 1), " significant after BH:", sum(inter$p_bh < .05), "\n")

save_docx(wald_t %>% mutate(F = round(F, 2), p_joint = sprintf("%.3f", p_joint)),
          file.path(TAB, "het_wald_tests.docx"),
          title = "Heterogeneity: joint tests of schedule x group interactions (child fixed-effects models)",
          notes = paste("Each row is a separate model with child and child-age fixed effects; F tests that all",
                        "schedule x group interactions are zero. SEs clustered by mother."))
