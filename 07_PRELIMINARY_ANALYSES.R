
##################################################################
#########  PRELIMINARY ANALYSES ######################
###################################################################


#------------------------------------------------------------- #
#---------------- OPEN DATASET and set up ---------------- #
#------------------------------------------------------------- #

load("data.Rda")
library(fixest)
library(dplyr)
library(modelsummary)



#------------------------------------------------------------- #
#----------------  OLS MODELS  ---------------- #
#------------------------------------------------------------- #

# Relevel factor to set "regular_day" as reference
data$shift_type <- relevel(factor(data$shift_type), ref = "regular_day")

# Create filtered dataset without "not_working"
data_filtered <- data %>% filter(shift_type != "not_working")

# Fit models
models <- list(
  "OLS: Physical Health (all)"      = feols(physical_health ~ shift_type, data),
  "OLS: Behavioural Problems (all)"                 = feols(bpiz ~ shift_type, data),
  "OLS: Physical Health (working)" = feols(physical_health ~ shift_type, data_filtered),
  "OLS: Behavioural Problems (working)"            = feols(bpiz ~ shift_type, data_filtered),
  "FE: Physical Health (all)"      = feols(physical_health ~ shift_type | childID, data),
  "FE: Behavioural Problems (all)"                 = feols(bpiz ~ shift_type | childID, data),
  "FE: Physical Health (working)" = feols(physical_health ~ shift_type | childID, data_filtered),
  "FE: Behavioural Problems (working)"            = feols(bpiz ~ shift_type | childID, data_filtered)
)

# Define clustered standard errors where appropriate
vcov_list <- list(
  "OLS: Physical Health (all)"      = ~motherID,
  "OLS: Behavioural Problems (all)"                 = ~motherID,
  "OLS: Physical Health (working)" = ~motherID,
  "OLS: Behavioural Problems (working)"            = ~motherID,
  "FE: Physical Health (all)"      = ~motherID,
  "FE: Behavioural Problems (all)"                 = ~motherID,
  "FE: Physical Health (working)" = ~motherID,
  "FE: Behavioural Problems (working)"            = ~motherID
)

# Export to Word
modelsummary(models,
             vcov = vcov_list,
             statistic = "({std.error})",
             stars = TRUE,
             gof_omit = "Adj|AIC|BIC|Log|RMSE",
             output = "results/preliminary_models.docx",
             title = "Preliminary Results: Effects of Parental Work Shifts"
)

# Export to HTML
modelsummary(models,
             vcov = vcov_list,
             statistic = "({std.error})",
             stars = TRUE,
             gof_omit = "Adj|AIC|BIC|Log|RMSE",
             output = "results/preliminary_models.html",
             title = "Preliminary Results: Effects of Parental Work Shifts"
)
