
##################################################################
######### MERGE CHILDREN AND MOTHER ######################
###################################################################
#------------- Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(pilot)
library(haven)
#------------------------------------------------------------- #
#---------------- OPEN DATASETS ---------------- #
#------------------------------------------------------------- #

load("data_mothers.Rda")
mothers <- data_mothers %>%
  dplyr::rename(motherID = ID)%>%
  dplyr::rename(sex_mother = sex)%>%
  dplyr::rename(race_mother = race)

load("data_children.Rda")
children<- data_long %>%
  dplyr::rename(sex_child = sex)%>%
  dplyr::rename(race_child = race)%>%
  dplyr::rename(birthyear_child = birthyear)

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
data %>%
  distinct(motherID, childID) %>%
  dplyr::count(motherID, name = "n_children") %>%
  dplyr::count(n_children, name = "n_families")

#------------------------------------------------------------- #
#---------------- ARRANGING THE STRUCTURE AND CLEAN ---------------- #
#------------------------------------------------------------- #

#-------------------------------Filter those with real ages
data <- data %>%
  filter(childage >= 0, childage <= 14)


#-------------------------------Reorder and arrange

data <- data %>%
  relocate(childID, year, motherID) %>%  # put these three first, in this order
  arrange(childID, year)

#------------------------------- Create dichotomous variable

data <- data%>%
  mutate(
    dich_shift_type = case_when(
      shift_type %in% c("irregular", "regular_evening", "regular_night",
                        "rotates_shift", "split_shift") ~ "nonstandard",
      shift_type == "regular_day"   ~ "standard",
      shift_type == "not_working"   ~ "not_working",
      TRUE                          ~ NA_character_
    )
  )
#------------------------------- Inspect the variables of interest

table(data$shift_type)
table(data$dich_shift_type)

#-------------------------------Save
save(data, file="data_analyses.Rda")
