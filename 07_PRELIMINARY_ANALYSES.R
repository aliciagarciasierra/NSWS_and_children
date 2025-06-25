
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
library(purrr)
library(tibble)


#------------------------------------------------------------- #
#----------------  OLS MODELS FOR HEALTH LIMITATIONS  ---------------- #
#------------------------------------------------------------- #
# Relevel shift_type reference
data$shift_type <- relevel(factor(data$shift_type), ref = "regular_day")

#Create lagged variable
data_lagged <- data %>%
  # Ensure proper ordering
  arrange(motherID, childID, year) %>%
  # Create lag within each mother-child pair
  group_by(motherID, childID) %>%
  mutate(lag_shift_type = dplyr::lag(shift_type, order_by = year)) %>%
  ungroup() 

# Ensure factors are clearly defined
data$age_anchor <- factor(data$age_anchor, levels = c("5-6", "7-8", "9-10", "11-12", "13-14"))
data$race_child<-factor(data$race_child)
data$health_insurance<-factor(data$health_insurance)
data$marital_status<-factor(data$marital_status)

# Fit pooled model
pooled_model <- feols(health_limiting_conditions ~ shift_type+
                        #Child level controls
                        age_anchor + sex_child + race_child + health_insurance + 
                        #Family level controls
                        marital_status + num_children + numberjobs + education + totalincome + hourlywage + healthlimit
                      , data = data)

# Fit stratified models by age_anchor
run_ols <- function(sub_data) {
  feols(health_limiting_conditions ~ shift_type +
          #Child level controls
         sex_child + race_child + health_insurance + 
          #Family level controls
          marital_status + num_children + numberjobs + education + totalincome + hourlywage + healthlimit
          , data = sub_data)
}

ols_models <- data %>%
  split(.$age_anchor) %>%
  imap(~run_ols(.x))

# Name models
names(ols_models) <- paste0("Age ", names(ols_models))

# Combine with pooled model
ols_models <- c(list("All Ages (Pooled)" = pooled_model), ols_models)

# Update vcov list
vcov_list <- rep(list(~motherID), length(ols_models))
names(vcov_list) <- names(ols_models)

# Custom "Clustered SE" row
cluster_row <- tibble(
  row = "Clustered SE: motherID",
  !!!setNames(rep("Yes", length(ols_models)), names(ols_models))
)

# Export table
modelsummary(
  ols_models,
  vcov = vcov_list,
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "Adj|AIC|BIC|Log|RMSE|Std.Errors", 
  add_rows = cluster_row,
  notes = "Standard errors in parentheses. * p<0.1, ** p<0.05, *** p<0.01",
  output = "results/OLS_Health_Limitations.docx",
  title = "OLS Models: Health-Limiting Conditions by Age Group"
)


#------------------------------------------------------------- #
#----------------  OLS MODELS FOR HEALTH RATING  ---------------- #
#------------------------------------------------------------- #

# Fit pooled model
pooled_model <- feols(healthrating_dichotomized ~ shift_type+
                        #Child level controls
                        age_anchor + sex_child + race_child + health_insurance + 
                        #Family level controls
                        marital_status + num_children + numberjobs + education + totalincome + hourlywage + healthlimit
                      , data = data)

# Fit stratified models by age_anchor
run_ols <- function(sub_data) {
  feols(healthrating_dichotomized ~ shift_type +
          #Child level controls
          sex_child + race_child + health_insurance + 
          #Family level controls
          marital_status + num_children + numberjobs + education + totalincome + hourlywage + healthlimit
        , data = sub_data)
}

ols_models <- data %>%
  split(.$age_anchor) %>%
  imap(~run_ols(.x))

# Name models
names(ols_models) <- paste0("Age ", names(ols_models))

# Combine with pooled model
ols_models <- c(list("All Ages (Pooled)" = pooled_model), ols_models)

# Update vcov list
vcov_list <- rep(list(~motherID), length(ols_models))
names(vcov_list) <- names(ols_models)

# Custom "Clustered SE" row
cluster_row <- tibble(
  row = "Clustered SE: motherID",
  !!!setNames(rep("Yes", length(ols_models)), names(ols_models))
)

# Export table
modelsummary(
  ols_models,
  vcov = vcov_list,
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "Adj|AIC|BIC|Log|RMSE|Std.Errors", 
  add_rows = cluster_row,
  notes = "Standard errors in parentheses. * p<0.1, ** p<0.05, *** p<0.01",
  output = "results/OLS_MothersRating.docx",
  title = "OLS Models: Mothers' Health-Rating by Age Group"
)

#------------------------------------------------------------- #
#----------------  OLS MODELS FOR BEHAVIOURAL PROBLEMS  ---------------- #
#------------------------------------------------------------- #

# Fit pooled model
pooled_model <- feols(bpiz~ shift_type+
                        #Child level controls
                        age_anchor + sex_child + race_child + health_insurance + 
                        #Family level controls
                        marital_status + num_children + numberjobs + education + totalincome + hourlywage + healthlimit
                      , data = data)

# Fit stratified models by age_anchor
run_ols <- function(sub_data) {
  feols(bpiz ~ shift_type +
          #Child level controls
          sex_child + race_child + health_insurance + 
          #Family level controls
          marital_status + num_children + numberjobs + education + totalincome + hourlywage + healthlimit
        , data = sub_data)
}

ols_models <- data %>%
  split(.$age_anchor) %>%
  imap(~run_ols(.x))

# Name models
names(ols_models) <- paste0("Age ", names(ols_models))

# Combine with pooled model
ols_models <- c(list("All Ages (Pooled)" = pooled_model), ols_models)

# Update vcov list
vcov_list <- rep(list(~motherID), length(ols_models))
names(vcov_list) <- names(ols_models)

# Custom "Clustered SE" row
cluster_row <- tibble(
  row = "Clustered SE: motherID",
  !!!setNames(rep("Yes", length(ols_models)), names(ols_models))
)

# Export table
modelsummary(
  ols_models,
  vcov = vcov_list,
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "Adj|AIC|BIC|Log|RMSE|Std.Errors", 
  add_rows = cluster_row,
  notes = "Standard errors in parentheses. * p<0.1, ** p<0.05, *** p<0.01",
  output = "results/OLS_BehaviouralProblems.docx",
  title = "OLS Models: Behavioural Problems by Age Group"
)



#------------------------------------------------------------- #
#----------------  FE MODELS  ---------------- #
#------------------------------------------------------------- #
#------------------ FE MODELS FOR HEALTH LIMITATIONS (POOLED) ------------------#

# Fit FE models for each outcome
fe_models <- list(
  "FE: Health Limiting Conditions" = feols(health_limiting_conditions ~ shift_type + age_anchor | childID, data = data),
  "FE: General Health Rating"      = feols(healthrating ~ shift_type + age_anchor | childID, data = data),
  "FE: Behavioural Problems (BPI)" = feols(bpiz ~ shift_type + age_anchor | childID, data = data)
)

# Define clustered SEs
vcov_list <- rep(list(~motherID), length(fe_models))
names(vcov_list) <- names(fe_models)

# Add "Clustered SE" custom row
cluster_row <- tibble(
  row = "Clustered SE: motherID",
  !!!setNames(rep("Yes", length(fe_models)), names(fe_models))
)

# Export to Word
modelsummary(
  fe_models,
  vcov = vcov_list,
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "Adj|AIC|BIC|Log|RMSE|Std.Errors",
  add_rows = cluster_row,
  notes = "Standard errors in parentheses. * p<0.1, ** p<0.05, *** p<0.01",
  output = "results/FE_models.docx",
  title = "Fixed Effects Models (Pooled): Effects of Parental Work Shifts"
)

