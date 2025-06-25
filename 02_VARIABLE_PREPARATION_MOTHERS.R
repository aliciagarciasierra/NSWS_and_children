
##################################################################
######### DATA CLEANING AND VARIABLE PREPARATION MOTHERS ######################
###################################################################

#------------------------------------------------------------- #
#---------------- CLEAN WORK SCHEDULES VARIABLE ---------------- #
#------------------------------------------------------------- #

#------- From 1979 to 1985

data <- data %>%
  mutate(across(c(`CPS-QES-86A.01_1979`, `CPS-QES-86A.01_1980`, `CPS-QES-86A.01_1981`, 
                  `CPS-QES-86A.01_1983`, `CPS-QES-86A.01_1984`, `CPS-QES-86A.01_1985`),
                ~ recode(.,
                         `1` = "regular_day",
                         `2` = "regular_evening",
                         `3` = "regular_night",
                         `4` = "split_shift",
                         `5` = "irregular",
                         `6` = NA_character_),
                .names = "{.col}_new")) %>%
  rename_with(~ sub("CPS-QES-86A.01_", "shift_", .x), ends_with("_new")) %>%
  rename_with(~ sub("_new", "", .x), ends_with("_new"))  # Removes unnecessary suffix


#------- 1986 and 1987

data <- data %>%
  mutate(shift_1986 = case_when(
    `CPS-QES-86C.01_1986` == 0 ~ "irregular",
    `CPS-QES-86C.01_1986` >= 1 & `CPS-QES-86C.01_1986` <= 600 ~ "regular_night",
    `CPS-QES-86C.01_1986` >= 601 & `CPS-QES-86C.01_1986` <= 1500 ~ "regular_day",
    `CPS-QES-86C.01_1986` >= 1501 & `CPS-QES-86C.01_1986` <= 2100 ~ "regular_evening",
    `CPS-QES-86C.01_1986` >= 2101 & `CPS-QES-86C.01_1986` <= 2400 ~ "regular_night",
    TRUE ~ NA_character_
  ),
  shift_1987 = case_when(
    `CPS-QES-86C.01_1987` == 0 ~ "irregular",
    `CPS-QES-86C.01_1987` >= 1 & `CPS-QES-86C.01_1987` <= 600 ~ "regular_night",
    `CPS-QES-86C.01_1987` >= 601 & `CPS-QES-86C.01_1987` <= 1500 ~ "regular_day",
    `CPS-QES-86C.01_1987` >= 1501 & `CPS-QES-86C.01_1987` <= 2100 ~ "regular_evening",
    `CPS-QES-86C.01_1987` >= 2101 & `CPS-QES-86C.01_1987` <= 2400 ~ "regular_night",
    TRUE ~ NA_character_
  ))


#------- 1988 and 1989

if("CPS-QES-86A.01_1988" %in% colnames(data) && "CPS-QES-86C.01_1988" %in% colnames(data) &&
   "CPS-QES-86A.01_1989" %in% colnames(data) && "CPS-QES-86C.01_1989" %in% colnames(data)) {
  
  data <- data %>%
    mutate(
      shift_1988 = case_when(
        `CPS-QES-86A.01_1988` == 2 ~ "irregular",
        `CPS-QES-86C.01_1988` >= 0 & `CPS-QES-86C.01_1988` <= 600 ~ "regular_night",
        `CPS-QES-86C.01_1988` >= 601 & `CPS-QES-86C.01_1988` <= 1500 ~ "regular_day",
        `CPS-QES-86C.01_1988` >= 1501 & `CPS-QES-86C.01_1988` <= 2100 ~ "regular_evening",
        `CPS-QES-86C.01_1988` >= 2101 & `CPS-QES-86C.01_1988` <= 2500 ~ "regular_night",
        TRUE ~ NA_character_
      ),
      shift_1989 = case_when(
        `CPS-QES-86A.01_1989` == 2 ~ "irregular",
        `CPS-QES-86C.01_1989` >= 0 & `CPS-QES-86C.01_1989` <= 600 ~ "regular_night",
        `CPS-QES-86C.01_1989` >= 601 & `CPS-QES-86C.01_1989` <= 1500 ~ "regular_day",
        `CPS-QES-86C.01_1989` >= 1501 & `CPS-QES-86C.01_1989` <= 2100 ~ "regular_evening",
        `CPS-QES-86C.01_1989` >= 2101 & `CPS-QES-86C.01_1989` <= 2500 ~ "regular_night",
        TRUE ~ NA_character_
      )
    )
} 


#------- 1990 to 2000

# List of columns to transform
columns_to_process <- c(
  "CPS-QES-86A.01_1979", "CPS-QES-86A.01_1980", "CPS-QES-86A.01_1981", 
  "CPS-QES-86A.01_1983", "CPS-QES-86A.01_1984", "CPS-QES-86A.01_1985",
  "CPS-QES-86A.01_1990", "CPS-QES-86A.01_1991", "CPS-QES-86A.01_1992", 
  "CPS-QES-86A.01_1993", "QES-86A.01_1994", "QES-86A.01_1996", 
  "QES-86A.01_1998", "QES-86A.01_2000"
)

# Recode 
for (column in columns_to_process) {
  # Extract the year from the column name
  year <- sub(".*_(\\d{4})$", "\\1", column)
  # Recode varaible
  data <- data %>%
    mutate(!!paste0("shift_", year) := recode(
      .data[[column]], 
      `1` = "regular_day", 
      `2` = "regular_evening", 
      `3` = "regular_night", 
      `4` = "rotates_shift", 
      `5` = "split_shift", 
      `6` = "irregular", 
      `7` = NA_character_
    ))
}


#------- from 2002 to 2022

years <- c(2002, 2004, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022)

# Loop through years and create the corresponding shift_ variables for each year
for (year in years) {
  
  # Define the two column names for the current year
  col_0_01 <- paste0("QES-86A_0.01_", year)
  col_01 <- paste0("QES-86A.01_", year)
  
  # Apply the mutate and case_when for each year
  data <- data %>%
    mutate(!!paste0("shift_", year) := case_when(
      # Checking first variable QES-86A_0.01_YYYY
      !!sym(col_0_01) == 1 ~ "regular_day",
      
      # Checking second variable QES-86A.01_YYYY
      !!sym(col_01) == 2 ~ "regular_evening",
      !!sym(col_01) == 3 ~ "regular_night",
      !!sym(col_01) == 4 ~ "rotates_shift",
      !!sym(col_01) == 5 ~ "split_shift",
      !!sym(col_01) == 6 ~ "irregular",
      
      # Default case if none of the conditions match
      TRUE ~ NA_character_
    ))
}

#------- ID and demographic variables

data <- data %>%
  mutate(
    ID = CASEID_1979,
    sex = SAMPLE_SEX_1979,
    race=SAMPLE_RACE_78SCRN,
    education= HGC_EVER_XRND
  )


#-------- TIME VARYING DMEOGRAPHICS

#Rename number of children variable for easy reshaping afterwards
years_children <- seq(1990, 2022, by = 1)
current_names <- paste0("NUMCH", sprintf("%02d", years_children %% 100), "_", years_children)
new_names <- paste0("NUMCH_", years_children)
for (i in seq_along(current_names)) {
  if (current_names[i] %in% names(data)) {
    names(data)[names(data) == current_names[i]] <- new_names[i]
  }
}

#Create empty HRP1_2022 to prevent dropping variable 
data$HRP1_2022<-NA

#---------------------------------------------------- #
#---------------- SAMPLE SELECTION ---------------- #
#---------------------------------------------------- #

# List of all years with shift information
years <- c(1990,  1992, 1994, 1996, 1998, 2000, 2002, 2004, 
           2008, 2010, 2012, 2014, 2016, 2018, 2020) # only from 1990 onwards beecause this is the period with more consistent variables

# Create a vector of the shift variable names (shift_ followed by the year)
shift_vars <- paste0("shift_", years)
doi_vars <- paste0("DOI_EMPLOYED_", years)
marstat_vars <- paste0("MARSTAT-KEY_", years)
famsize_vars <- paste0("FAMSIZE_", years)
hourlyrate_vars<-paste0("HRP1_", years)
numch_vars<-paste0("NUMCH_", years)
healthlimit_vars<-paste0("Q11-4_", years)
numberjobs<-paste0("JOBSNUM_", years)
totalincome<-paste0("TNFI_TRUNC_", years)

# Select the variables ID, SEX, RACE, and all shift variables created in the script
data_selected <- data %>%
  select(ID, sex, race, education, all_of(shift_vars), 
         all_of(doi_vars), 
         all_of(marstat_vars),
         all_of(famsize_vars),
         all_of(hourlyrate_vars),
         all_of(numch_vars),
         all_of(healthlimit_vars),
         all_of(numberjobs),
         all_of(totalincome))

# View the selected data
head(data_selected)

#------------------------------------------------------------- #
#---------------- RESHAPING TO LONGITUDINAL ---------------- #
#------------------------------------------------------------- #

data_long <- data_selected %>%
  pivot_longer(
    cols = matches("^(shift_|DOI_EMPLOYED_|MARSTAT-KEY_|FAMSIZE_|HRP1_|NUMCH_|Q11-4_|JOBSNUM_|TNFI_TRUNC_)"),
    names_to = c(".value", "year"),
    names_pattern = "^(shift|DOI_EMPLOYED|MARSTAT-KEY|FAMSIZE|HRP1|NUMCH|Q11-4|JOBSNUM|TNFI_TRUNC)_(\\d+)"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(ID, year) %>%
  filter(!is.na(shift)) %>%
  filter(sex == 2) %>%
  rename(
    shift_type = shift,
    employed = DOI_EMPLOYED,
    marital_status = `MARSTAT-KEY`,
    family_size = FAMSIZE,
    hourlywage = HRP1,
    num_children = NUMCH,
    healthlimit=`Q11-4`,
    numberjobs=JOBSNUM,
    totalincome=TNFI_TRUNC
    
  )


#------------------------------------------------------------- #
#----- READJUSTING LEVELS FOR THE WORK SHIFTS VARIABLE ---------------- #
#------------------------------------------------------------- #

data_long <- data_long %>%
  mutate(shift_type = case_when(
    employed == 0 ~ "not_working",  # Override if not employed
    shift_type %in% c("irregular", "rotates_shift", "split_shift") ~ "irregular",
    TRUE ~ shift_type
  ))

table(data_long$shift_type)

#------------------------------------------------------------- #
#------------------- SAVE DATA-------------------# 
#------------------------------------------------------------- #

save(data_long, file = "data_mothers.Rda")
