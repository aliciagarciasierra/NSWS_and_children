###############################################################################
# 06_main_models.R
# Main results (Table 2): pooled OLS vs child fixed-effects models, estimated
# on EXACTLY the same sample for each outcome. SEs clustered by mother.
#
#   OLS : outcome ~ schedule + child age dummies + child sex + child race
#         + mother's education + lone mother + family size + wave dummies
#   FE1 : outcome ~ schedule | child FE + child age FE
#   FE2 : FE1 + time-varying family controls (lone mother, family size,
#         log family income with a missing-income indicator)
#   FE3 : FE2 + mother's broad occupation group
#
# The gap between OLS and FE tells us how much of the between-family
# association is explained by stable child/family characteristics (selection).
# Input : data/analysis.rds
# Output: results/tables/table2_main_models.(csv|docx), main_coefficients.csv
###############################################################################
source("code/00_setup.R")
d <- readRDS(file.path(DATA, "analysis.rds")) %>%
  mutate(occ_group_f = forcats::fct_na_value_to_level(occ_group, "Missing"))

outcomes <- c(hlc = "Health-limiting conditions", poor_health = "Mother-rated poor health",
              bpi = "Behavior Problems Index")
samples  <- c(hlc = "s_hlc", poor_health = "s_rating", bpi = "s_bpi")

# Build a formula from a (possibly multi-line) string
F <- function(...) as.formula(gsub("\\s+", " ", paste(...)))

fit_all <- function(y) {
  x <- d[d[[samples[y]]], ]
  list(
    OLS = feols(F(y, "~ shift + i(child_age) + child_female + race_child + educ +
                                   lone_mother + family_size + i(year)"), x, cluster = CLUSTER),
    FE1 = feols(F(y, "~ shift | childID + child_age"), x, cluster = CLUSTER),
    FE2 = feols(F(y, "~ shift + lone_mother + family_size + log_income_f + income_missing
                                   | childID + child_age"), x, cluster = CLUSTER),
    FE3 = feols(F(y, "~ shift + lone_mother + family_size + log_income_f + income_missing
                                   + occ_group_f | childID + child_age"), x, cluster = CLUSTER)
  )
}
models <- lapply(names(outcomes), fit_all); names(models) <- names(outcomes)

# ---- Coefficients in long format --------------------------------------------
coefs <- lapply(names(models), function(y) lapply(names(models[[y]]), function(sp) {
  ct <- coeftable(models[[y]][[sp]])
  rows <- grep("^shift", rownames(ct))
  tibble(outcome = y, model = sp, term = sub("^shift", "", rownames(ct)[rows]),
         b = ct[rows, 1], se = ct[rows, 2], p = ct[rows, 4], N = nobs(models[[y]][[sp]]))
}) %>% bind_rows()) %>% bind_rows()
write.csv(coefs, file.path(TAB, "main_coefficients.csv"), row.names = FALSE)

# ---- Readable summary in the console / log ------------------------------------
fmt <- function(b, se, p) paste0(sprintf("%.3f", b), " (", sprintf("%.3f", se), ")",
                                 ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
wide <- coefs %>% mutate(est = fmt(b, se, p)) %>%
  select(outcome, term, model, est) %>% pivot_wider(names_from = model, values_from = est)
say("Schedule coefficients (ref. = regular day), SEs clustered by mother")
print(as.data.frame(wide), right = FALSE)

say("How much of the OLS coefficient disappears in FE1? (only meaningful when OLS is clearly non-zero)")
red <- coefs %>% filter(model %in% c("OLS", "FE1")) %>% select(outcome, term, model, b) %>%
  pivot_wider(names_from = model, values_from = b) %>%
  mutate(reduction_pct = round(100 * (1 - FE1 / OLS)))
print(as.data.frame(red))

# ---- Word table ----------------------------------------------------------------
all_models <- unlist(models, recursive = FALSE)
names(all_models) <- paste(rep(outcomes, each = 4), rep(c("OLS", "FE", "FE + family", "FE + family + occ."), 3))
tab <- modelsummary(all_models, output = "data.frame", coef_omit = "^(?!shift)",
                    coef_rename = function(x) sub("^shift", "", x),
                    stars = STARS, gof_map = c("nobs"), fmt = 3)
tab <- tab %>% select(-part, -statistic) %>% rename(` ` = term)
write.csv(tab, file.path(TAB, "table2_main_models.csv"), row.names = FALSE)
save_docx(tab, file.path(TAB, "table2_main_models.docx"),
          title = "Table 2. Maternal work schedule and child outcomes: OLS vs child fixed effects",
          notes = paste("Reference: regular day schedule. Standard errors clustered by mother in parentheses.",
                        "OLS and FE models use the same sample for each outcome.",
                        "* p<.05, ** p<.01, *** p<.001."))
cat("\nSaved results/tables/table2_main_models.docx\n")
