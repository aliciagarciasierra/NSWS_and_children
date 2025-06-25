
##################################################################
######### MERGE CHILDREN AND MOTHER ######################
###################################################################
#------------- Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(pilot)
#------------------------------------------------------------- #
#---------------- OPEN DATASETS ---------------- #
#------------------------------------------------------------- #

load("data_mothers.Rda")
mothers <- data_long %>%
  rename(motherID = ID)%>%
  rename(sex_mother = sex)%>%
  rename(race_mother = race)

load("data_children.Rda")
children<- data_long %>%
  rename(sex_child = sex)%>%
  rename(race_child = race)%>%
  rename(birthyear_child = birthyear)

#------------------------------------------------------------- #
#---------------- MERGE ---------------- #
#------------------------------------------------------------- #

# Merge
merged_data<-merge(mothers, children, by.x=c("motherID", "year"), by.y=c("motherID", "year"),all.x=TRUE)

#Select only women with children
data<- merged_data[ which(merged_data$childID!="NA"), ]

#Check number of observations

n_distinct(data$childID) #10234 children
n_distinct(data$motherID) #4297 mothers

# Number of children per family
children %>%
  distinct(motherID, childID) %>%
  count(motherID, name = "n_children") %>%
  count(n_children, name = "n_families")
      

#------------------------------------------------------------- #
#---------------- FURTHER CLEANING IN THE VARIABLES ---------------- #
#------------------------------------------------------------- #

# Relabel rece
table(data$race_child)
data <- data %>%
  mutate(race_child = case_when(
    race_child %in% c(1, 2) ~ 1, # non-white
    race_child == 3         ~ 0, # white
    TRUE                    ~ NA_real_  # handles other/missing cases safely
  ))

# Dichotomous health rating
table(data$healthrating)
data$healthrating_dichotomized <- ifelse(data$healthrating %in% c(1, 2), 1, 0)
table(data$healthrating_dichotomized)

# Create age anchor variable
data <- data %>%
  mutate(age_anchor = case_when(
    childage >= 5 & childage <= 6  ~ "5-6",
    childage >= 7 & childage <= 8  ~ "7-8",
    childage >= 9 & childage <= 10 ~ "9-10",
    childage >= 11 & childage <= 12 ~ "11-12",
    childage >= 13 & childage <= 14 ~ "13-14",
    TRUE ~ NA_character_
  ))


#------------------------------------------------------------- #
#---------------- SAMPLE RESTRICTIONS ---------------- #
#------------------------------------------------------------- #

#------------------------------------------------------------- #
#---------------- SAVE DATA ---------------- #
#------------------------------------------------------------- #

save(data, file="data.Rda")
