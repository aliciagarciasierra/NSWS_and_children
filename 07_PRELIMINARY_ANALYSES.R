
##################################################################
#########  PRELIMINARY ANALYSES ######################
###################################################################

#------------------------------------------------------------- #
#---------------- OPEN DATASET and set up ---------------- #
#------------------------------------------------------------- #

# Load data
load("data_analyses.Rda")

#Libraries
library(fixest)
library(dplyr)
library(modelsummary)
library(purrr)
library(tibble)
library(tidyr)
library(officer)
library(forcats)
library(scales)
library(stringr)

#Theme for graphs
set_pilot_family("Avenir Next Medium", title_family = "Avenir Next Demi Bold")

#------------------------------------------------------------- #
#---------------- Descriptives ---------------- #
#------------------------------------------------------------- #

#--------------------------------------- #
#------1 ) Proportion over time of women in each shift type
#--------------------------------------- #

# Calculate proportions by year and shift type
data_prop <- data %>%
  dplyr::group_by(year, shift_type) %>%
  dplyr::summarise(n = dplyr::n()) %>%    # <- dplyr::summarise so n() works
  dplyr::group_by(year) %>%
  dplyr::mutate(proportion = n / sum(n)) %>%
  dplyr::ungroup()

# Plot the area graph with proportions (stacked)
ggplot(data_prop, aes(x = year, y = proportion, fill = shift_type)) +
  geom_area(stat = "identity", position = "stack") +  # Use "identity" instead of "count"
  scale_fill_manual(values = c("irregular" = "coral", 
                               "regular_day" = "olivedrab3", 
                               "regular_evening" = "lightblue1", 
                               "regular_night" = "purple", 
                               "not_working" = "orange")) +
  labs(title = "Proportion of the sample in each working schedule by year ",
       x = "Year", y = "Proportion", fill = "Working schedules") +
  scale_x_continuous(breaks = seq(min(data_prop$year), max(data_prop$year), by = 2)) +  
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position = "right",
              title_size = 13) 


#--------------------------------------- #
#--------2) Creating transitions variable
#--------------------------------------- #

# Create transition flag per individual
data_long <- data %>%
  group_by(motherID) %>%
  mutate(shift_prev = lag(shift_type), 
         transition_flag = if_else(shift_type != shift_prev, 1, 0)) %>%
  ungroup()

# Compute total transitions and total individuals per year
transition_counts <- data_long %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    total_transitions = sum(transition_flag, na.rm = TRUE),
    total_individuals = dplyr::n_distinct(motherID),
    .groups = "drop"
  ) %>%
  dplyr::mutate(proportion_transitions = total_transitions / total_individuals)

# Plot both lines: total transitions and proportion of transitions
ggplot(transition_counts, aes(x = year)) +
  geom_line(aes(y = total_transitions, color = "Total Transitions")) +
  geom_point(aes(y = total_transitions, color = "Total Transitions")) +
  geom_line(aes(y = proportion_transitions * max(total_transitions), color = "Proportion of Transitions")) +  # Scale proportion for better visibility
  geom_point(aes(y = proportion_transitions * max(total_transitions), color = "Proportion of Transitions")) +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(transition_counts$total_transitions), name = "Proportion of Transitions")) +
  labs(title = "Proportion and number of transitions between work shifts per year",
       x = "Year",
       y = "Number of Transitions",
       color = "Metric") +
  scale_x_continuous(breaks = seq(min(data_long$year), max(data_long$year), by = 2)) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position = "bottom",
              title_size = 13)


#--------------------------------------- #
#--------3) Understanding the transitions
#--------------------------------------- #


transition_matrix <- data_long%>%
  filter(transition_flag == 1) %>%
  group_by(shift_prev, shift_type) %>%
  dplyr::summarise(count = n()) %>%
  ungroup()

ggplot(transition_matrix, aes(x = shift_prev, y = shift_type, fill = count)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("white", "olivedrab1", "olivedrab2", "olivedrab3", "olivedrab4")) + 
  labs(title = " ", x = "Previous Shift Type", y = "Current Shift Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position = "right",
              title_size = 13)



#------------------------------------------------------------- #
#---------------- FE models ---------------- #
#------------------------------------------------------------- #
#First, make sure to establish the regular_day as the reference category
data$shift_type <- relevel(factor(data$shift_type), ref = "regular_day")
data$dich_shift_type <- relevel(factor(data$dich_shift_type), ref = "standard")

#---------- Simple models without controls and the full range variable
model1 <- feols(healthlimit ~ shift_type | childID + childage, data = data)
etable(model1)
model2 <- feols(bpiz ~ shift_type | childID + childage, data)
etable(model2)
model3 <- feols(healthrating ~ shift_type | childID + childage, data)
etable(model3)

models <- list(
  "Model 1 (Health Limitations)"    = model1,
  "Model 2 (Behavioural Problems)"    = model2,
  "Model 3 (Health Rated by the Mothers)"    = model3)

# Export to Word
modelsummary(models,
             stars=TRUE,
             output = "results/TWFE_simple.docx",
             title = "TWFE without controls")

#------------With the reduced variable 
model1 <- feols(healthlimit ~ dich_shift_type | childID + childage, data = data)
etable(model1)
model2 <- feols(bpiz ~ dich_shift_type | childID + childage, data)
etable(model2)
model3 <- feols(healthrating ~ dich_shift_type | childID + childage, data)
etable(model3)

models <- list(
  "Model 1 (Health Limitations)"    = model1,
  "Model 2 (Behavioural Problems)"    = model2,
  "Model 3 (Health Rated by the Mothers)"    = model3)

# Export to Word
modelsummary(models,
             stars=TRUE,
             output = "results/TWFE_reduced_variable.docx",
             title = "TWFE reduced variable")



#-----------With control for occupational changes
model1 <- feols(healthlimit ~ shift_type + occ | childID + childage, data = data)
etable(model1)
model2 <- feols(bpiz ~ shift_type+ occ | childID + childage, data)
etable(model2)
model3 <- feols(healthrating~ shift_type + occ | childID + childage, data)
etable(model3)

models <- list(
  "Model 1 (Health Limitations)"    = model1,
  "Model 2 (Behavioural Problems)"    = model2,
  "Model 3 (Health Rated by the Mothers)"    = model3)

# Export to Word
modelsummary(models,
             stars=TRUE,
             output = "results/TWFE_occ_changes.docx",
             title = "TWFE with control for occupational changes")


#-----------With control for occupational changes
model1 <- feols(healthlimit ~ shift_type + childage + family_size + race_child + sex_child +education , data = data)
etable(model1)
model2 <- feols(bpiz ~ shift_type+ + childage + family_size + race_child + sex_child +education, data)
etable(model2)
model3 <- feols(healthrating~ shift_type + childage + family_size + race_child + sex_child +education, data)
etable(model3)

models <- list(
  "Model 1 (Health Limitations)"    = model1,
  "Model 2 (Behavioural Problems)"    = model2,
  "Model 3 (Health Rated by the Mothers)"    = model3)

# Export to Word
modelsummary(models,
             stars=TRUE,
             output = "results/OLS.docx",
             title = "OLS with sociodemographic controls")
