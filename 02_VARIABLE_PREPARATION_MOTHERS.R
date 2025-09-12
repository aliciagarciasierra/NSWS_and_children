
##################################################################
######### DATA CLEANING AND VARIABLE PREPARATION MOTHERS ######################
###################################################################

#------------------------------------------------------------- #
#---------------- CLEAN WORK SCHEDULES VARIABLE ---------------- #
#------------------------------------------------------------- #

#------- 1990 to 2000

# Your original list
columns_to_process <- c(
  "CPS-QES-86A.01_1979", "CPS-QES-86A.01_1980", "CPS-QES-86A.01_1981", 
  "CPS-QES-86A.01_1983", "CPS-QES-86A.01_1984", "CPS-QES-86A.01_1985",
  "CPS-QES-86A.01_1990", "CPS-QES-86A.01_1991", "CPS-QES-86A.01_1992", 
  "CPS-QES-86A.01_1993", "QES-86A.01_1994", "QES-86A.01_1996", 
  "QES-86A.01_1998", "QES-86A.01_2000"
)

# Only keep names that actually exist in your data
cols <- intersect(columns_to_process, names(data))

# Create shift_<year> columns with recoded categories
for (col in cols) {
  year <- stringr::str_extract(col, "\\d{4}$")
  data[[paste0("shift_", year)]] <- dplyr::recode(
    as.character(data[[col]]),
    "1" = "regular_day",
    "2" = "regular_evening",
    "3" = "regular_night",
    "4" = "rotates_shift",
    "5" = "split_shift",
    "6" = "irregular",
    "7" = NA_character_,
    .default = NA_character_,
    .missing = NA_character_
  )
}



#------- from 2002 to 2022

years <- c(2002, 2004, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022)

for (year in years) {
  col_0_01 <- paste0("QES-86A_0.01_", year)  # indicator for regular day (1)
  col_01   <- paste0("QES-86A.01_",  year)   # 2..6 categories
  
  # Pull columns safely (or NAs if not present)
  x0 <- if (col_0_01 %in% names(data)) data[[col_0_01]] else rep(NA, nrow(data))
  x1 <- if (col_01   %in% names(data)) data[[col_01]]   else rep(NA, nrow(data))
  
  # Coerce to numeric safely (handles labelled/factors/characters)
  x0 <- suppressWarnings(as.numeric(as.character(x0)))
  x1 <- suppressWarnings(as.numeric(as.character(x1)))
  
  # Build the shift_<year> vector
  data[[paste0("shift_", year)]] <- dplyr::case_when(
    x0 == 1 ~ "regular_day",        # from the _0.01_ var
    x1 == 2 ~ "regular_evening",    # from the .01 var
    x1 == 3 ~ "regular_night",
    x1 == 4 ~ "rotates_shift",
    x1 == 5 ~ "split_shift",
    x1 == 6 ~ "irregular",
    x1 == 7 ~ NA_character_,        # explicit NA code if present
    TRUE ~ NA_character_
  )
}


#------- ID and demographic variables

data <- data %>%
  mutate(
    ID = CASEID_1979,
    sex = SAMPLE_SEX_1979,
    race=SAMPLE_RACE_78SCRN,
    education= HGC_EVER_XRND
  )


#-------- TIME VARYING DEMOGRAPHICS

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
  tidyr::pivot_longer(
    cols = tidyselect::matches("^(shift_|DOI_EMPLOYED_|MARSTAT-KEY_|FAMSIZE_|HRP1_|NUMCH_|Q11-4_|JOBSNUM_|TNFI_TRUNC_)"),
    names_to = c(".value", "year"),
    names_pattern = "^(shift|DOI_EMPLOYED|MARSTAT-KEY|FAMSIZE|HRP1|NUMCH|Q11-4|JOBSNUM|TNFI_TRUNC)_(\\d{4})"
  ) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::arrange(ID, year) %>%
  dplyr::filter(!is.na(shift), sex == 2) %>%
  dplyr::rename(
    shift_type     = shift,
    employed       = DOI_EMPLOYED,
    marital_status = `MARSTAT-KEY`,
    family_size    = FAMSIZE,
    hourlywage     = HRP1,
    num_children   = NUMCH,
    healthlimit    = `Q11-4`,
    numberjobs     = JOBSNUM,
    totalincome    = TNFI_TRUNC
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


