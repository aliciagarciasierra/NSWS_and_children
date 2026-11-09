###############################################################################
# 05b_figures.R
# Figure 1. Who works which schedule? Linear probability models of working a
#           regular evening, regular night or irregular schedule (vs. any
#           other schedule) on mothers' characteristics, all entered jointly.
#           Unit: mother-wave (employed mothers of children in the sample).
# Figure 2. Schedule changes between consecutive observations of the same
#           child: where do mothers go from each schedule? (row percentages)
#           These changes are what identifies the fixed-effects estimates.
# Input : data/analysis.rds, data/mothers_panel.rds
# Output: results/figures/fig1_who_works_what.(png|pdf), fig2_transitions.(png|pdf)
#         results/tables/fig1_who_works_what.csv
###############################################################################
source("code/00_setup.R")
library(ggplot2)
theme_set(theme_minimal(base_size = 11, base_family = "sans") +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "grey92"), strip.text = element_text(face = "bold"),
                  plot.title.position = "plot"))

d  <- readRDS(file.path(DATA, "analysis.rds"))
mp <- readRDS(file.path(DATA, "mothers_panel.rds"))

# ---------------------------------------------------------------------------
# Figure 1
# ---------------------------------------------------------------------------
# NOTE: these are POOLED (between-family) models: they describe which mothers
# work each schedule. They are not fixed-effects models; education and race do
# not change over time, so a model with mother fixed effects could not show them.
# Mother-waves: employed, valid schedule, mother of at least one sample child,
# wave in which at least one of her children is in the analysis sample.
keep <- distinct(d, motherID, year)
mw <- mp %>% semi_join(keep, by = c("motherID", "year")) %>%
  filter(employed %in% 1, !is.na(shift)) %>%
  mutate(income_tercile = factor(ntile(income, 3), labels = c("Low", "Middle", "High")),
         income_tercile = forcats::fct_na_value_to_level(income_tercile, "Missing"),
         occ_group = forcats::fct_na_value_to_level(occ_group, "Missing"),
         evening = as.integer(shift == "Regular evening"),
         night = as.integer(shift == "Regular night"),
         irregular = as.integer(shift == "Irregular"))
cat("Mother-waves used in Figure 1:", nrow(mw), " mothers:", n_distinct(mw$motherID), "\n")

rhs <- "educ + race_mother + lone_mother + family_size + income_tercile + occ_group | year"
lpm <- lapply(c(evening = "evening", night = "night", irregular = "irregular"), function(y)
  feols(as.formula(paste(y, "~", rhs)), mw, cluster = CLUSTER))

labels <- c(
  "educHigh school" = "Education: high school", "educSome college" = "Education: some college",
  "educCollege degree" = "Education: college degree",
  "race_motherHispanic" = "Hispanic", "race_motherBlack" = "Black",
  "lone_mother" = "Lone mother", "family_size" = "Family size (+1)",
  "income_tercileMiddle" = "Income: middle tercile", "income_tercileHigh" = "Income: top tercile",
  "income_tercileMissing" = "Income: missing",
  "occ_groupSales" = "Occupation: sales", "occ_groupClerical/administrative" = "Occupation: clerical",
  "occ_groupService" = "Occupation: service", "occ_groupProduction/transport/farm" = "Occupation: production/transport",
  "occ_groupMissing" = "Occupation: missing")

f1 <- lapply(names(lpm), function(y) {
  ct <- coeftable(lpm[[y]])
  tibble(schedule = y, term = rownames(ct), b = ct[, 1], se = ct[, 2], p = ct[, 4])
}) %>% bind_rows() %>%
  filter(term %in% names(labels), !grepl("Missing", term)) %>%
  mutate(label = factor(labels[term], levels = rev(labels)),
         schedule = factor(schedule, levels = c("evening", "night", "irregular"),
                           labels = c("Regular evening", "Regular night", "Irregular")),
         lo = b - 1.96 * se, hi = b + 1.96 * se)
write.csv(f1, file.path(TAB, "fig1_who_works_what.csv"), row.names = FALSE)

base_rates <- sapply(c("evening", "night", "irregular"), function(v) mean(mw[[v]]))
say("Share of mother-waves in each schedule")
print(round(base_rates, 3))

p1 <- ggplot(f1, aes(x = b * 100, y = label)) +
  geom_vline(xintercept = 0, colour = "grey30", linetype = "dashed") +
  geom_errorbar(aes(xmin = lo * 100, xmax = hi * 100), width = 0, colour = "grey50", orientation = "y") +
  geom_point(size = 2, colour = "black") +
  facet_wrap(~schedule, nrow = 1) +
  labs(x = "Difference in the probability of working this schedule (percentage points)", y = NULL,
       title = "Figure 1. Who works nonstandard schedules?",
       subtitle = paste0("Linear probability models, all characteristics entered jointly, wave fixed effects; 95% CI clustered by mother.\n",
                         "Reference: less than high school, non-Black non-Hispanic, partnered, lowest income tercile, managerial/professional occupation."))
ggsave(file.path(FIG, "fig1_who_works_what.png"), p1, width = 10, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(FIG, "fig1_who_works_what.pdf"), p1, width = 10, height = 5.5)

# ---------------------------------------------------------------------------
# Figure 2
# ---------------------------------------------------------------------------
tr <- d %>% filter(s_hlc) %>% arrange(childID, year) %>% group_by(childID) %>%
  mutate(prev = lag(shift)) %>% ungroup() %>% filter(!is.na(prev)) %>%
  count(prev, shift) %>% group_by(prev) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()
say("Transitions (row %)")
print(tr %>% select(-n) %>% pivot_wider(names_from = shift, values_from = pct) %>% as.data.frame(), digits = 3)

p2 <- ggplot(tr, aes(x = shift, y = forcats::fct_rev(prev), fill = pct)) +
  geom_tile(colour = "grey80") +
  geom_text(aes(label = paste0(round(pct), "%\n(n=", n, ")")), size = 3.2,
            colour = ifelse(tr$pct > 50, "white", "black")) +
  scale_fill_gradient(low = "white", high = "grey25", guide = "none") +
  labs(x = "Schedule at current observation", y = "Schedule at previous observation",
       title = "Figure 2. Changes in mothers' work schedules between observations",
       subtitle = "Consecutive observations of the same child (health-limiting conditions sample). Row percentages.")
ggsave(file.path(FIG, "fig2_transitions.png"), p2, width = 8, height = 5, dpi = 300, bg = "white")
ggsave(file.path(FIG, "fig2_transitions.pdf"), p2, width = 8, height = 5)
cat("\nSaved figures in results/figures\n")
