
##################################################################
######### DATA CLEANING AND VARIABLE PREPARATION CHILDREN ######################
###################################################################

#------------------------------------------------------------- #
#---------------- CLEAN  VARIABLES ---------------- #
#------------------------------------------------------------- #

#------------------------------------------------------------- #
#------- TIME INVARIANT 
#------------------------------------------------------------- #

data <- data %>%
  mutate(
    childID = CPUBID_XRND,
    motherID=MPUBID_XRND,
    sex = CSEX_XRND,
    race=CRACE_XRND,
    birthyear= CYRB_XRND
  )

#------------------------------------------------------------- #
#------- TIME VARIANT 
#------------------------------------------------------------- #

# Define all relevant prefixes and years
prefixes <- c("HOMEZ", "MOTOZ", "BPIZ")
years <- seq(1986, 2014, by = 2)

# Loop through prefixes and years to create new variables
for (prefix in prefixes) {
  for (year in years) {
    from <- paste0(prefix, year, "_", year)
    to   <- paste0(tolower(prefix), "_", year)
    
    if (from %in% names(data)) {
      data[[to]] <- data[[from]]
    }
  }
}


#------------------------------------------------------------- #
#---------Physical Health variables ---------#
#------------------------------------------------------------- #

years <- c(1990,1992, 1994, 1996, 1998,
           2000, 2002, 2004,
           2006, 2008, 2010,
           2012, 2014, 2016, 2018)

#----- Condition that limits school attendance
qnames <- c("CS901631_1990", "CS921758_1992", "CS94H-2A_1994", "CS96H-2A_1996", "CS98H-2A_1998",
            "HLTH-2A_2000", "HLTH-2A_2002", "HLTH-2A_2004",
            "MS-HLTH-2A_2006", "MS-HLTH-2A_2008", "MS-HLTH-2A_2010", 
            "MS-HLTH-2A_2012", "MS-HLTH-2A_2014", "MS-HLTH-2A_2016", "MS-HLTH-2A_2018")


# New variable names
new_names <- paste0("conditionattendance_", years)
# Create named vector
rename_map <- setNames(new_names, qnames)
# Rename
names(data)[names(data) %in% names(rename_map)] <- rename_map[names(data)[names(data) %in% names(rename_map)]]

#----- Condition that limits school attendance
qnames <- c("CS901635_1990", "CS921762_1992", "CS94H-2C_1994", "CS96H-2C_1996", "CS98H-2C_1998",
            "HLTH-2C_2000", "HLTH-2C_2002", "HLTH-2C_2004",
            "MS-HLTH-2C_2006", "MS-HLTH-2C_2008", "MS-HLTH-2C_2010", 
            "MS-HLTH-2C_2012", "MS-HLTH-2C_2014", "MS-HLTH-2C_2016", "MS-HLTH-2C_2018")

# New variable names
new_names <- paste0("conditionactivities_", years)
# Create named vector
rename_map <- setNames(new_names, qnames)
# Rename
names(data)[names(data) %in% names(rename_map)] <- rename_map[names(data)[names(data) %in% names(rename_map)]]


#----- Condition that limits school work
qnames <- c("CS901633_1990", "CS921760_1992", "CS94H-2B_1994", "CS96H-2B_1996", "CS98H-2B_1998",
            "HLTH-2B_2000", "HLTH-2B_2002", "HLTH-2B_2004",
            "MS-HLTH-2B_2006", "MS-HLTH-2B_2008", "MS-HLTH-2B_2010", 
            "MS-HLTH-2B_2012", "MS-HLTH-2B_2014", "MS-HLTH-2B_2016", "MS-HLTH-2B_2018")

# New variable names
new_names <- paste0("conditionwork_", years)
# Create named vector
rename_map <- setNames(new_names, qnames)
# Rename
names(data)[names(data) %in% names(rename_map)] <- rename_map[names(data)[names(data) %in% names(rename_map)]]


#----- Condition that requires medicines
qnames <- c("CS901639_1990", "CS921766_1992", "CS94H-3B_1994", "CS96H-3B_1996", "CS98H-3B_1998",
            "HLTH-3B_2000", "HLTH-3B_2002", "HLTH-3B_2004",
            "MS-HLTH-3B_2006", "MS-HLTH-3B_2008", "MS-HLTH-3B_2010", 
            "MS-HLTH-3B_2012", "MS-HLTH-3B_2014", "MS-HLTH-3B_2016", "MS-HLTH-3B_2018")

# New variable names
new_names <- paste0("conditionmedicines_", years)
# Create named vector
rename_map <- setNames(new_names, qnames)
# Rename
names(data)[names(data) %in% names(rename_map)] <- rename_map[names(data)[names(data) %in% names(rename_map)]]


#----- Condition that requires special equipment
qnames <- c("CS901641_1990", "CS921768_1992", "CS94H-3C_1994", "CS96H-3C_1996", "CS98H-3C_1998",
            "HLTH-3C_2000", "HLTH-3C_2002", "HLTH-3C_2004",
            "MS-HLTH-3C_2006", "MS-HLTH-3C_2008", "MS-HLTH-3C_2010", 
            "MS-HLTH-3C_2012", "MS-HLTH-3C_2014", "MS-HLTH-3C_2016", "MS-HLTH-3C_2018")

# New variable names
new_names <- paste0("conditionequipment_", years)
# Create named vector
rename_map <- setNames(new_names, qnames)
# Rename
names(data)[names(data) %in% names(rename_map)] <- rename_map[names(data)[names(data) %in% names(rename_map)]]


#----- Condition that requires medical treatment
qnames <- c("CS901637_1990", "CS921764_1992", "CS94H-3A_1994", "CS96H-3A_1996", "CS98H-3A_1998",
            "HLTH-3A_2000", "HLTH-3A_2002", "HLTH-3A_2004",
            "MS-HLTH-3A_2006", "MS-HLTH-3A_2008", "MS-HLTH-3A_2010", 
            "MS-HLTH-3A_2012", "MS-HLTH-3A_2014", "MS-HLTH-3A_2016", "MS-HLTH-3A_2018")

# New variable names
new_names <- paste0("conditiontreatment_", years)
# Create named vector
rename_map <- setNames(new_names, qnames)
# Rename
names(data)[names(data) %in% names(rename_map)] <- rename_map[names(data)[names(data) %in% names(rename_map)]]

#------------------------------------------------------------- #
#---------  Health rating by the mothers ---------#
#------------------------------------------------------------- #

#This variable was not collected for 1990, so let's create one with NAs.
data$healthrating_1990<-NA

# Now normal processing
qnames <- c("healthrating_1990", "MS921515_1992", "MS941711_1994", "MS961711_1996", "MS985019A_1998",
            "BKGN-46A_2000", "0	BKGN-46A_2002", "BKGN-46A_2004",
            "MS-BKGN-46A_2006", "MS-BKGN-46A_2008", "MS-BKGN-46A_2010", 
            "MS-BKGN-46A_2012", "MS-BKGN-46A_2014", "MS-BKGN-46A_2016", "MS-BKGN-46A_2018")

# New variable names
new_names <- paste0("healthrating_", years)
# Create named vector
rename_map <- setNames(new_names, qnames)
# Rename
names(data)[names(data) %in% names(rename_map)] <- rename_map[names(data)[names(data) %in% names(rename_map)]]


#------------------------------------------------------------- #
#--------- Mental Health variables ---------#
#------------------------------------------------------------- #

#------------- Mother felt or was told child needed help for mental/behavioural/emotional problems
qnames <- c("CS902137_1990", "CS922237_1992", "CS94H-17_1994", "CS96H-17_1996", "CS98H-17_1998",
            "	HLTH-19_2000", "HLTH-19_2002", "HLTH-19_2004",
            "MS-HLTH-19_2006", "MS-HLTH-19_2008", "MS-HLTH-19_2010", 
            "MS-HLTH-19_2012", "MS-HLTH-19_2014", "MS-HLTH-19_2016", "MS-HLTH-19_2018")

# New variable names
new_names <- paste0("childmentalhelp_", years)
# Create named vector
rename_map <- setNames(new_names, qnames)
# Rename
names(data)[names(data) %in% names(rename_map)] <- rename_map[names(data)[names(data) %in% names(rename_map)]]


#------------------Child saw a psychiatrist in the last year
qnames <- c("CS902111_1990", "CS922211_1992", "CS94H-16_1994", "CS96H-16_1996", "CS96H-16_1998",
            "HLTH-17_2000", "HLTH-17_2002", "HLTH-17_2004",
            "MS-HLTH-17_2006", "MS-HLTH-17_2008", "MS-HLTH-17_2010", 
            "MS-HLTH-17_2012", "MS-HLTH-17_2014", "MS-HLTH-17_2016", "MS-HLTH-17_2018")

# New variable names
new_names <- paste0("childpsychiatrist_", years)
# Create named vector
rename_map <- setNames(new_names, qnames)
# Rename
names(data)[names(data) %in% names(rename_map)] <- rename_map[names(data)[names(data) %in% names(rename_map)]]

#------------------------------------------------------------- #
#--------- Other variables ---------#
#------------------------------------------------------------- #

#----- Private health insurance
qnames <- c("CS902055_1990", "CS922155_1992", "CS94H-13_1994", "CS96H-13_1996", "CS98H-13_1998",
            "HLTH-14_2000", "HLTH-14_2002", "HLTH-14_2004",
            "MS-HLTH-14_2006", "MS-HLTH-14_2008", "MS-HLTH-14_2010", 
            "MS-HLTH-14_2012", "MS-HLTH-14_2014", "MS-HLTH-14_2016", "MS-HLTH-14_2018")


# New variable names
new_names <- paste0("privateinsurance_", years)
# Create named vector
rename_map <- setNames(new_names, qnames)
# Rename
names(data)[names(data) %in% names(rename_map)] <- rename_map[names(data)[names(data) %in% names(rename_map)]]

#----- Medicaid 
qnames <- c("CS902057_1990", "CS922157_1992", "CS94H-14_1994", "CS96H-14_1996", "CS98H-14_1998",
            "HLTH-15_2000", "HLTH-15_2002", "HLTH-15_2004",
            "MS-HLTH-15_2006", "MS-HLTH-15_2008", "MS-HLTH-15_2010", 
            "MS-HLTH-15_2012", "MS-HLTH-15_2014", "MS-HLTH-15_2016", "MS-HLTH-15_2018")


# New variable names
new_names <- paste0("medicaidinsurance_", years)
# Create named vector
rename_map <- setNames(new_names, qnames)
# Rename
names(data)[names(data) %in% names(rename_map)] <- rename_map[names(data)[names(data) %in% names(rename_map)]]


#------------------------------------------------------------- #
#---------------- SELECT VARIABLES ---------------- #
#------------------------------------------------------------- #

custom_prefixes <- c(
  "homez_", "motoz_", "bpiz_",
  "conditionattendance_", "conditionactivities_", "conditionwork_",
  "conditionmedicines_", "conditionequipment_", "conditiontreatment_",
  "childmentalhelp_", "childpsychiatrist_", "medicaidinsurance_", "privateinsurance_", "healthrating"
)

# Plus time-invariant variables
time_invariant_variables <- c("childID", "motherID", "sex", "race", "birthyear")

# Combine all variables you've created
created_vars <- c(
  time_invariant_variables,
  names(data)[sapply(names(data), function(col) any(startsWith(col, custom_prefixes)))]
)

data_selected <- data[, created_vars]

#------------------------------------------------------------- #
#---------------- RESHAPE DATA ---------------- #
#------------------------------------------------------------- #

data_long <- data_selected %>%
  pivot_longer(
    cols = -all_of(time_invariant_variables),
    names_to = c(".value", "year"),
    names_sep = "_"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(childID, year)


#------------------------------------------------------------- #
#---------------- CLEAN VARIABLES ---------------- #
#------------------------------------------------------------- #

#------- PHYSICAL HEALTH

condition_vars <- names(data_long)[grepl("^condition", names(data_long))]
data_long <- data_long %>%
  mutate(across(all_of(condition_vars), ~ case_when(
    .x %in% 0:1 ~ .x,       # Keep 0 and 1
    TRUE ~ NA_real_         # Convert anything else to NA
  )))

data_long <- data_long %>% # sum the conditions to try to capture worse health
  rowwise() %>%
  mutate(health_limiting_conditions = sum(c_across(all_of(condition_vars)) %>% as.numeric(), na.rm = TRUE)) %>%
  ungroup()
table(data_long$health_limiting_conditions)

#-------- MENTAL HEALTH

data_long <- data_long %>%
  mutate(mental_health = case_when(
    childmentalhelp == 1 | childpsychiatrist == 1 ~ 1,
    is.na(childmentalhelp) & is.na(childpsychiatrist) ~ NA_real_,
    TRUE ~ 0
  ))
table(data_long$mental_health)

#-------- HEALTH INSURANCE

data_long <- data_long %>% 
  mutate(
  health_insurance = case_when(
    privateinsurance == 1 ~ 1,
    medicaidinsurance == 1 ~ 2,
    TRUE ~ 0
  )
)
table(data_long$health_insurance)


#------------- CHILD AGE

# Plot the distribution of birthyear
data_long %>%
  distinct(childID, birthyear) %>%
  count(birthyear) %>%
  ggplot(aes(x = birthyear, y = n)) +
  geom_col() +
  labs(title = "Number of children per birthyear",
       x = "Birthyear",
       y = "Number of children") +
  theme_minimal()
table(data$childage)
data_long %>% filter(year - birthyear < 0) %>% select(childID, year, birthyear, childage)


data_long$childage<-data_long$year-data_long$birthyear
table(data_long$childage)

# Remove outliers in age
data <- data %>%
  filter(childage >= 0 & childage <= 15)

#------------------------------------------------------------- #
#---------------- SAVE DATA ---------------- #
#------------------------------------------------------------- #
save(data_long, file = "data_children.Rda")
