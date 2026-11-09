###############################################################################
# 04_analytic_sample.R
# Merges children with their mothers' wave-specific information and builds
# the analytic samples. Every restriction is counted in a "sample flow" table
# so that the N reported in the paper can be traced back.
# Input : data/children_panel.rds, data/mothers_panel.rds
# Output: data/analysis.rds, results/tables/sample_flow.csv
###############################################################################
source("code/00_setup.R")
kids    <- readRDS(file.path(DATA, "children_panel.rds"))
mothers <- readRDS(file.path(DATA, "mothers_panel.rds"))

d <- kids %>% left_join(mothers, by = c("motherID", "year")) %>%
  mutate(race_child = factor(race_child, levels = c(3, 1, 2),
                             labels = c("Non-Black non-Hispanic", "Hispanic", "Black")))

flow <- list()
count_step <- function(df, label) {
  flow[[length(flow) + 1]] <<- tibble(step = label, child_years = nrow(df),
                                       children = n_distinct(df$childID),
                                       mothers = n_distinct(df$motherID))
  df
}

d0 <- count_step(d, "Child-years aged 3-14, waves 1990-2018")
d1 <- count_step(d0 %>% filter(employed %in% 1), "Mother employed at interview")
d2 <- count_step(d1 %>% filter(!is.na(shift)), "Mother reports a valid work schedule")

# Covariates used in the models (see 06_main_models.R).
# Family income is NOT required here: it is missing for ~15% of child-years and
# it may itself be affected by the mother's schedule (a mediator). It enters
# the models only as an extra control, with a missing-value indicator.
OLS_COVARS <- c("child_age", "child_female", "race_child", "educ", "lone_mother",
                "family_size", "year")
d2 <- d2 %>% mutate(covars_ok = complete.cases(across(all_of(OLS_COVARS))))

# Outcome-specific estimation samples: outcome and covariates observed, and the
# child observed at least twice (single observations add nothing to a model
# with child fixed effects). OLS and FE use EXACTLY the same sample, so the
# change in the coefficient between them is not due to a change in sample.
make_sample <- function(df, y) {
  ok <- !is.na(df[[y]]) & df$covars_ok
  n_obs <- ave(as.integer(ok), df$childID, FUN = sum)
  ok & n_obs >= 2
}
d2 <- d2 %>% mutate(s_hlc = make_sample(d2, "hlc"),
                    s_rating = make_sample(d2, "poor_health"),
                    s_bpi = make_sample(d2, "bpi"))

for (s in c("hlc", "rating", "bpi")) {
  lab <- c(hlc = "health-limiting conditions", rating = "mother-rated poor health", bpi = "BPI")[s]
  count_step(d2 %>% filter(!is.na(.data[[c(hlc = "hlc", rating = "poor_health", bpi = "bpi")[s]]])),
             paste0("  ", lab, ": outcome observed"))
  count_step(d2 %>% filter(.data[[paste0("s_", s)]]),
             paste0("  ", lab, ": + covariates observed, child observed 2+ times (estimation sample)"))
}

flow <- bind_rows(flow)
say("Sample flow")
print(as.data.frame(flow), right = FALSE)
write.csv(flow, file.path(TAB, "sample_flow.csv"), row.names = FALSE)

analysis <- d2 %>% filter(s_hlc | s_rating | s_bpi) %>%
  mutate(income_missing = as.integer(is.na(log_income)),
         log_income_f   = ifelse(is.na(log_income), 0, log_income))   # used with income_missing
saveRDS(analysis, file.path(DATA, "analysis.rds"))
cat("\nSaved data/analysis.rds:", nrow(analysis), "child-years in at least one estimation sample\n")
