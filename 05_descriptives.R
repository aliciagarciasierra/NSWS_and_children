###############################################################################
# 05_descriptives.R
# Table 1: sample characteristics by mother's work schedule, and the amount of
# within-child variation in schedules that identifies the fixed-effects models.
# Input : data/analysis.rds
# Output: results/tables/table1_descriptives.(csv|docx),
#         results/tables/within_child_variation.csv,
#         results/tables/transitions.csv
###############################################################################
source("code/00_setup.R")
d <- readRDS(file.path(DATA, "analysis.rds"))

# Indicator columns for categorical variables
d <- d %>% mutate(
  race_hispanic = as.integer(race_child == "Hispanic"),
  race_black    = as.integer(race_child == "Black"),
  educ_lths     = as.integer(educ == "Less than high school"),
  educ_hs       = as.integer(educ == "High school"),
  educ_somecol  = as.integer(educ == "Some college"),
  educ_college  = as.integer(educ == "College degree"),
  income_k      = income / 1000,
  occ_service   = as.integer(occ_group == "Service"),
  occ_prof      = as.integer(occ_group == "Managerial/professional/technical"))

vars <- c(
  "Health-limiting conditions (0-6)"          = "hlc",
  "Any health-limiting condition"             = "any_condition",
  "Mother-rated poor health (1 excellent - 4 poor)" = "poor_health",
  "Behavior Problems Index (standard score)"  = "bpi",
  "Child age"                                 = "child_age",
  "Child is female"                           = "child_female",
  "Child Hispanic"                            = "race_hispanic",
  "Child Black"                               = "race_black",
  "Mother: less than high school"             = "educ_lths",
  "Mother: high school"                       = "educ_hs",
  "Mother: some college"                      = "educ_somecol",
  "Mother: college degree"                    = "educ_college",
  "Lone mother"                               = "lone_mother",
  "Family size"                               = "family_size",
  "Family income (thousands, 2018 $)"         = "income_k",
  "Usual weekly hours (1990-2012)"            = "hours_week",
  "Occupation: managerial/professional"       = "occ_prof",
  "Occupation: service"                       = "occ_service")

# Mean (SD) by schedule. For evening, night and irregular, stars mark a
# significant difference from REGULAR DAY (the reference category), from a
# regression of the variable on schedule dummies with SEs clustered by mother.
star <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
one_row <- function(v) {
  m <- tapply(d[[v]], d$shift, mean, na.rm = TRUE)
  s <- tapply(d[[v]], d$shift, sd,   na.rm = TRUE)
  fit <- feols(as.formula(paste(v, "~ shift")), data = d, cluster = CLUSTER, notes = FALSE)
  p <- coeftable(fit)[-1, 4]                      # p-values vs regular day
  dec <- ifelse(v %in% c("bpi", "income_k", "hours_week"), "%.1f", "%.2f")
  cells <- paste0(sprintf(dec, m), " (", sprintf(dec, s), ")")
  cells[2:4] <- paste0(cells[2:4], star(p))
  cells
}
tab <- t(sapply(vars, one_row))
colnames(tab) <- levels(d$shift)
tab <- rbind(tab, "Child-years" = as.character(table(d$shift)))
tab <- data.frame(Variable = rownames(tab), tab, check.names = FALSE, row.names = NULL)

say("Table 1")
print(tab, right = FALSE)
write.csv(tab, file.path(TAB, "table1_descriptives.csv"), row.names = FALSE)
save_docx(tab, file.path(TAB, "table1_descriptives.docx"),
          title = "Table 1. Sample characteristics by mother's work schedule (means, SD in parentheses)",
          notes = paste("Child-years in at least one estimation sample.",
                        "Stars indicate a significant difference from regular day schedules",
                        "(regression on schedule dummies, SEs clustered by mother): * p<.05, ** p<.01, *** p<.001."))

# ---------------------------------------------------------------------------
# Within-child variation in schedules (what identifies the FE estimates)
# ---------------------------------------------------------------------------
var_tab <- lapply(c(hlc = "s_hlc", poor_health = "s_rating", bpi = "s_bpi"), function(s) {
  x <- d[d[[s]], ]
  per_child <- x %>% group_by(childID) %>%
    summarise(n_types = n_distinct(shift), ever_irreg = any(shift == "Irregular"),
              ever_even = any(shift == "Regular evening"), ever_night = any(shift == "Regular night"),
              .groups = "drop")
  tibble(children = nrow(per_child),
         pct_children_with_schedule_change = round(100 * mean(per_child$n_types > 1), 1),
         n_switch_children_irregular = sum(per_child$n_types > 1 & per_child$ever_irreg),
         n_switch_children_evening   = sum(per_child$n_types > 1 & per_child$ever_even),
         n_switch_children_night     = sum(per_child$n_types > 1 & per_child$ever_night))
}) %>% bind_rows(.id = "outcome")
say("Within-child variation in schedules, by estimation sample")
print(as.data.frame(var_tab))
write.csv(var_tab, file.path(TAB, "within_child_variation.csv"), row.names = FALSE)

# Transitions between consecutive observed waves (HLC sample)
tr <- d %>% filter(s_hlc) %>% arrange(childID, year) %>% group_by(childID) %>%
  mutate(prev = lag(shift)) %>% ungroup() %>% filter(!is.na(prev))
tr_tab <- as.data.frame.matrix(table(previous = tr$prev, current = tr$shift))
say("Transitions between consecutive observations (rows = previous, cols = current)")
print(tr_tab)
write.csv(tr_tab, file.path(TAB, "transitions.csv"))
