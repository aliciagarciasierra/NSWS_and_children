

##################################################################
######################## DESCRIPTIVES MOTHERS ######################
###################################################################


#--------------------------------------- #
#---------------- PLOTS ---------------- #
#--------------------------------------- #


#--------------------------------------- #
#------1 ) Proportion over time of women changing work schedules over time
#--------------------------------------- #

set_pilot_family("Avenir Next Medium", title_family = "Avenir Next Demi Bold")

# Calculate proportions by year and shift type
data_prop <- data_long%>%
  group_by(year, shift_type) %>%
  tally() %>%
  group_by(year) %>%
  mutate(proportion = n / sum(n))

# Plot the area graph with proportions (stacked)
ggplot(data_prop, aes(x = year, y = proportion, fill = shift_type)) +
  geom_area(stat = "identity", position = "stack") +  # Use "identity" instead of "count"
  scale_fill_manual(values = c("irregular" = "coral", 
                               "regular_day" = "olivedrab3", 
                               "regular_evening" = "lightblue1", 
                               "regular_night" = "purple", 
                               "not_working" = "orange")) +
  labs(title = " ",
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
data_long <- data_long %>%
  group_by(ID) %>%
  mutate(shift_prev = lag(shift_type), 
         transition_flag = if_else(shift_type != shift_prev, 1, 0)) %>%
  ungroup()

# Compute total transitions and total individuals per year
transition_counts <- data_long %>%
  group_by(year) %>%
  summarise(total_transitions = sum(transition_flag, na.rm = TRUE),
            total_individuals = n_distinct(ID)) %>%
  mutate(proportion_transitions = total_transitions / total_individuals)

# Plot both lines: total transitions and proportion of transitions
ggplot(transition_counts, aes(x = year)) +
  geom_line(aes(y = total_transitions, color = "Total Transitions")) +
  geom_point(aes(y = total_transitions, color = "Total Transitions")) +
  geom_line(aes(y = proportion_transitions * max(total_transitions), color = "Proportion of Transitions")) +  # Scale proportion for better visibility
  geom_point(aes(y = proportion_transitions * max(total_transitions), color = "Proportion of Transitions")) +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(transition_counts$total_transitions), name = "Proportion of Transitions")) +
  labs(title = " ",
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


transition_matrix <- data_long %>%
  filter(transition_flag == 1) %>%
  group_by(shift_prev, shift_type) %>%
  summarise(count = n()) %>%
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

