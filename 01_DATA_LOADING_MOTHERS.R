
##################################################################
######### DATA LOADING AND PREPARATION MOTHERS ######################
###################################################################

#--------------- SET WORKING DIRECTORY
setwd("~/Library/CloudStorage/OneDrive-UniversitédeLausanne/PAPER PABLO WEN/NSWS")

#------------- Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(pilot)

#------------- LOAD DATA FROM NLS

# Set working directory
# setwd()


# Set working directory
# setwd()


new_data <- read.table('NSWS.dat', sep=' ')
names(new_data) <- c('R0000100',
                     'R0047400',
                     'R0173600',
                     'R0214700',
                     'R0214800',
                     'R0264100',
                     'R0449100',
                     'R0946400',
                     'R1256800',
                     'R1651900',
                     'R1924800',
                     'R2319600',
                     'R2531600',
                     'R2532200',
                     'R2929800',
                     'R2930400',
                     'R3135200',
                     'R3135400',
                     'R3270500',
                     'R3341500',
                     'R3400600',
                     'R3401400',
                     'R3401701',
                     'R3403500',
                     'R3407700',
                     'R3525600',
                     'R3525800',
                     'R3558000',
                     'R3605800',
                     'R3656000',
                     'R3656800',
                     'R3657101',
                     'R3658900',
                     'R3659047',
                     'R3730700',
                     'R3730900',
                     'R3885600',
                     'R3956000',
                     'R4006500',
                     'R4007300',
                     'R4007601',
                     'R4009400',
                     'R4009447',
                     'R4186600',
                     'R4186800',
                     'R4284000',
                     'R4416900',
                     'R4417600',
                     'R4418400',
                     'R4418701',
                     'R4420500',
                     'R4444700',
                     'R4615100',
                     'R4961200',
                     'R5079900',
                     'R5080600',
                     'R5081400',
                     'R5081701',
                     'R5083500',
                     'R5087500',
                     'R5165200',
                     'R5165900',
                     'R5166700',
                     'R5167001',
                     'R5168800',
                     'R5172800',
                     'R5297200',
                     'R5616700',
                     'R6126900',
                     'R6343700',
                     'R6478000',
                     'R6478600',
                     'R6479300',
                     'R6479801',
                     'R6481600',
                     'R6486400',
                     'R6685900',
                     'R6887300',
                     'R7005700',
                     'R7006400',
                     'R7007000',
                     'R7007501',
                     'R7009300',
                     'R7014200',
                     'R7347200',
                     'R7347700',
                     'R7597700',
                     'R7702900',
                     'R7703600',
                     'R7704300',
                     'R7704801',
                     'R7706600',
                     'R7711800',
                     'R8040200',
                     'R8040700',
                     'R8297500',
                     'R8495200',
                     'R8496000',
                     'R8496700',
                     'R8497201',
                     'R8499000',
                     'R8504300',
                     'T0273700',
                     'T0274200',
                     'T0895800',
                     'T0986800',
                     'T0987600',
                     'T0988500',
                     'T0989001',
                     'T0990700',
                     'T0996000',
                     'T1424000',
                     'T1424500',
                     'T2051600',
                     'T2209100',
                     'T2209900',
                     'T2210500',
                     'T2210801',
                     'T2212500',
                     'T2217800',
                     'T2447300',
                     'T2447800',
                     'T3022900',
                     'T3106900',
                     'T3107700',
                     'T3108400',
                     'T3108701',
                     'T3110400',
                     'T3115800',
                     'T3436500',
                     'T3437000',
                     'T3953100',
                     'T4110900',
                     'T4112200',
                     'T4112900',
                     'T4113201',
                     'T4114900',
                     'T4120300',
                     'T4410300',
                     'T4410800',
                     'T4890800',
                     'T5019700',
                     'T5022500',
                     'T5023300',
                     'T5023700',
                     'T5026200',
                     'T5031500',
                     'T5394800',
                     'T5395300',
                     'T5593900',
                     'T5768600',
                     'T5770700',
                     'T5771200',
                     'T5771600',
                     'T5774300',
                     'T5779700',
                     'T7952600',
                     'T7953100',
                     'T8088200',
                     'T8215400',
                     'T8218600',
                     'T8219300',
                     'T8219500',
                     'T8221500',
                     'T8226800',
                     'T8555000',
                     'T8555500',
                     'T8620300',
                     'T8785100',
                     'T8787800',
                     'T8788500',
                     'T8788700',
                     'T8790700',
                     'T8796100',
                     'T9103300',
                     'T9103800',
                     'T9178100',
                     'T9299600',
                     'T9300300',
                     'T9300500',
                     'T9302500',
                     'T9307900',
                     'T9900000')


# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -4] = NA  # Valid missing
new_data[new_data == -5] = NA  # Non-interview


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$R0047400 <- factor(data$R0047400,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0),
                          labels=c("REGULAR DAY SHIFT",
                                   "REGULAR EVENING SHIFT",
                                   "REGULAR NIGHT SHIFT",
                                   "SPLIT SHIFT",
                                   "HOURS VARY",
                                   "OTHER"))
  data$R0173600 <- factor(data$R0173600,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0),
                          labels=c("CROSS MALE WHITE",
                                   "CROSS MALE WH. POOR",
                                   "CROSS MALE BLACK",
                                   "CROSS MALE HISPANIC",
                                   "CROSS FEMALE WHITE",
                                   "CROSS FEMALE WH POOR",
                                   "CROSS FEMALE BLACK",
                                   "CROSS FEMALE HISPANIC",
                                   "SUP MALE WH POOR",
                                   "SUP MALE BLACK",
                                   "SUP MALE HISPANIC",
                                   "SUP FEM WH POOR",
                                   "SUP FEMALE BLACK",
                                   "SUP FEMALE HISPANIC",
                                   "MIL MALE WHITE",
                                   "MIL MALE BLACK",
                                   "MIL MALE HISPANIC",
                                   "MIL FEMALE WHITE",
                                   "MIL FEMALE BLACK",
                                   "MIL FEMALE HISPANIC"))
  data$R0214700 <- factor(data$R0214700,
                          levels=c(1.0,2.0,3.0),
                          labels=c("HISPANIC",
                                   "BLACK",
                                   "NON-BLACK, NON-HISPANIC"))
  data$R0214800 <- factor(data$R0214800,
                          levels=c(1.0,2.0),
                          labels=c("MALE",
                                   "FEMALE"))
  data$R0264100 <- factor(data$R0264100,
                          levels=c(1.0,2.0,3.0,4.0,5.0),
                          labels=c("DAY SHIFT",
                                   "EVENING SHIFT",
                                   "NIGHT SHIFT",
                                   "A SPLIT SHIFT",
                                   "HOURS VARY"))
  data$R0449100 <- factor(data$R0449100,
                          levels=c(1.0,2.0,3.0,4.0,5.0),
                          labels=c("DAY SHIFT",
                                   "EVENING SHIFT",
                                   "NIGHT SHIFT",
                                   "A SPLIT SHIFT",
                                   "HOURS VARY"))
  data$R0946400 <- factor(data$R0946400,
                          levels=c(1.0,2.0,3.0,4.0,5.0),
                          labels=c("DAY SHIFT",
                                   "EVENING SHIFT",
                                   "NIGHT SHIFT",
                                   "A SPLIT SHIFT",
                                   "HOURS VARY"))
  data$R1256800 <- factor(data$R1256800,
                          levels=c(1.0,2.0,3.0,4.0,5.0),
                          labels=c("DAY SHIFT",
                                   "EVENING SHIFT",
                                   "NIGHT SHIFT",
                                   "A SPLIT SHIFT",
                                   "HOURS VARY"))
  data$R1651900 <- factor(data$R1651900,
                          levels=c(1.0,2.0,3.0,4.0,5.0),
                          labels=c("DAY SHIFT",
                                   "EVENING SHIFT",
                                   "NIGHT SHIFT",
                                   "A SPLIT SHIFT",
                                   "HOURS VARY"))
  data$R2532200 <- factor(data$R2532200,
                          levels=c(1.0,2.0),
                          labels=c("SAME OR FIXED SHIFT",
                                   "SHIFT ROTATES"))
  data$R2930400 <- factor(data$R2930400,
                          levels=c(1.0,2.0),
                          labels=c("SAME OR FIXED SHIFT",
                                   "SHIFT ROTATES"))
  data$R3135200 <- factor(data$R3135200,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0),
                          labels=c("REGULAR DAY SHIFT",
                                   "REGULAR EVENING SHIFT",
                                   "REGULAR NIGHT SHIFT",
                                   "SHIFT ROTATES",
                                   "SPLIT SHIFT",
                                   "IRREGULAR HOURS",
                                   "OTHER"))
  data$R3270500 <- factor(data$R3270500,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$R3401400 <- factor(data$R3401400,
                          levels=c(0.0,1.0,2.0,3.0,5.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "REMARRIED",
                                   "WIDOWED"))
  data$R3401701 <- factor(data$R3401701,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$R3525600 <- factor(data$R3525600,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0),
                          labels=c("REGULAR DAY SHIFT",
                                   "REGULAR EVENING SHIFT",
                                   "REGULAR NIGHT SHIFT",
                                   "SHIFT ROTATES",
                                   "SPLIT SHIFT",
                                   "IRREGULAR HOURS",
                                   "OTHER"))
  data$R3558000 <- factor(data$R3558000,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$R3656800 <- factor(data$R3656800,
                          levels=c(0.0,1.0,2.0,3.0,5.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "REMARRIED",
                                   "WIDOWED"))
  data$R3657101 <- factor(data$R3657101,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$R3730700 <- factor(data$R3730700,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0),
                          labels=c("REGULAR DAY SHIFT",
                                   "REGULAR EVENING SHIFT",
                                   "REGULAR NIGHT SHIFT",
                                   "SHIFT ROTATES",
                                   "SPLIT SHIFT",
                                   "IRREGULAR HOURS",
                                   "OTHER"))
  data$R3885600 <- factor(data$R3885600,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$R4007300 <- factor(data$R4007300,
                          levels=c(0.0,1.0,2.0,3.0,5.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "REMARRIED",
                                   "WIDOWED"))
  data$R4007601 <- factor(data$R4007601,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$R4186600 <- factor(data$R4186600,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0),
                          labels=c("REGULAR DAY SHIFT",
                                   "REGULAR EVENING SHIFT",
                                   "REGULAR NIGHT SHIFT",
                                   "SHIFT ROTATES",
                                   "SPLIT SHIFT",
                                   "IRREGULAR HOURS",
                                   "OTHER"))
  data$R4284000 <- factor(data$R4284000,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$R4418400 <- factor(data$R4418400,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "WIDOWED"))
  data$R4418701 <- factor(data$R4418701,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$R4615100 <- factor(data$R4615100,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0),
                          labels=c("Regular day shift",
                                   "Regular evening shift",
                                   "Regular night shift",
                                   "Shift rotates (changes periodically from days to evenings or nights)",
                                   "Split shift (consists of two distinct periods each day)",
                                   "Irregular schedule or hours",
                                   "Other (SPECIFY)"))
  data$R4961200 <- factor(data$R4961200,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$R5081400 <- factor(data$R5081400,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$R5081701 <- factor(data$R5081701,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$R5166700 <- factor(data$R5166700,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: NEVER MARRIED",
                                   "1: MARRIED",
                                   "2: SEPARATED",
                                   "3: DIVORCED",
                                   "6: WIDOWED"))
  data$R5167001 <- factor(data$R5167001,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$R5297200 <- factor(data$R5297200,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0),
                          labels=c("Regular day shift",
                                   "Regular evening shift",
                                   "Regular night shift",
                                   "Shift rotates (changes periodically from days to evenings or nights)",
                                   "Split shift (consists of two distinct periods each day)",
                                   "Irregular schedule or hours",
                                   "Other (SPECIFY)"))
  data$R5616700 <- factor(data$R5616700,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$R6126900 <- factor(data$R6126900,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0),
                          labels=c("Regular day shift",
                                   "Regular evening shift",
                                   "Regular night shift",
                                   "Shift rotates (changes periodically from days to evenings)",
                                   "Split shift (consists of two distinct periods each day)",
                                   "Irregular schedule or hours",
                                   "Other (SPECIFY)"))
  data$R6343700 <- factor(data$R6343700,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$R6479300 <- factor(data$R6479300,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("Never Married",
                                   "Married",
                                   "Separated",
                                   "Divorced",
                                   "Widowed"))
  data$R6479801 <- factor(data$R6479801,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$R6685900 <- factor(data$R6685900,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("A regular day schedule/morning",
                                   "A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)",
                                   "irregular schedule (arranged by employee)"))
  data$R6887300 <- factor(data$R6887300,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$R7007000 <- factor(data$R7007000,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("Never Married",
                                   "Married",
                                   "Separated",
                                   "Divorced",
                                   "Widowed"))
  data$R7007501 <- factor(data$R7007501,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$R7347200 <- factor(data$R7347200,
                          levels=c(1.0,2.0),
                          labels=c("A regular day schedule",
                                   "Some other schedule"))
  data$R7347700 <- factor(data$R7347700,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("A regular day schedule/morning",
                                   "A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)",
                                   "irregular schedule (arranged by employee)"))
  data$R7597700 <- factor(data$R7597700,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$R7704300 <- factor(data$R7704300,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$R7704801 <- factor(data$R7704801,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$R8040200 <- factor(data$R8040200,
                          levels=c(1.0,2.0),
                          labels=c("A regular day schedule",
                                   "Some other schedule"))
  data$R8040700 <- factor(data$R8040700,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("A regular day schedule/morning",
                                   "A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)",
                                   "irregular schedule (arranged by employee)"))
  data$R8297500 <- factor(data$R8297500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$R8496700 <- factor(data$R8496700,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$R8497201 <- factor(data$R8497201,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$T0273700 <- factor(data$T0273700,
                          levels=c(1.0,2.0),
                          labels=c("A regular day schedule",
                                   "Some other schedule"))
  data$T0274200 <- factor(data$T0274200,
                          levels=c(2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)",
                                   "irregular schedule (arranged by employee)"))
  data$T0895800 <- factor(data$T0895800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$T0988500 <- factor(data$T0988500,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T0989001 <- factor(data$T0989001,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$T1424000 <- factor(data$T1424000,
                          levels=c(1.0,2.0),
                          labels=c("A regular day schedule",
                                   "Some other schedule"))
  data$T1424500 <- factor(data$T1424500,
                          levels=c(2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0),
                          labels=c("A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)",
                                   "Irregular schedule (arranged by employee)",
                                   "A regular day shift"))
  data$T2051600 <- factor(data$T2051600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$T2210500 <- factor(data$T2210500,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T2210801 <- factor(data$T2210801,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$T2447300 <- factor(data$T2447300,
                          levels=c(1.0,2.0),
                          labels=c("A regular day schedule",
                                   "Some other schedule"))
  data$T2447800 <- factor(data$T2447800,
                          levels=c(2.0,3.0,4.0,5.0,6.0,7.0),
                          labels=c("A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)"))
  data$T3022900 <- factor(data$T3022900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$T3108400 <- factor(data$T3108400,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T3108701 <- factor(data$T3108701,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$T3436500 <- factor(data$T3436500,
                          levels=c(1.0,2.0),
                          labels=c("A regular day schedule",
                                   "Some other schedule"))
  data$T3437000 <- factor(data$T3437000,
                          levels=c(2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,99.0),
                          labels=c("A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)",
                                   "R SETS OWN SCHEDULE",
                                   "R WORKS MULTIPLE CONSECUTIVE SHIFTS",
                                   "R WORKS PARTIAL SHIFTS OR WEEKEND ONLY SHIFTS",
                                   "UNCODABLE"))
  data$T3953100 <- factor(data$T3953100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$T4112900 <- factor(data$T4112900,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T4113201 <- factor(data$T4113201,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$T4410300 <- factor(data$T4410300,
                          levels=c(1.0,2.0),
                          labels=c("A regular day schedule",
                                   "Some other schedule"))
  data$T4410800 <- factor(data$T4410800,
                          levels=c(2.0,3.0,4.0,5.0,6.0,7.0),
                          labels=c("A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)"))
  data$T4890800 <- factor(data$T4890800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$T5023300 <- factor(data$T5023300,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T5023700 <- factor(data$T5023700,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$T5394800 <- factor(data$T5394800,
                          levels=c(1.0,2.0),
                          labels=c("A regular day schedule",
                                   "Some other schedule"))
  data$T5395300 <- factor(data$T5395300,
                          levels=c(2.0,3.0,4.0,5.0,6.0,7.0),
                          labels=c("A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)"))
  data$T5593900 <- factor(data$T5593900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$T5771200 <- factor(data$T5771200,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T5771600 <- factor(data$T5771600,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$T7952600 <- factor(data$T7952600,
                          levels=c(1.0,2.0),
                          labels=c("A regular day schedule",
                                   "Some other schedule"))
  data$T7953100 <- factor(data$T7953100,
                          levels=c(2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0),
                          labels=c("A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)",
                                   "Irregular schedule (arranged by employee)",
                                   "A regular day shift"))
  data$T8088200 <- factor(data$T8088200,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$T8219300 <- factor(data$T8219300,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T8219500 <- factor(data$T8219500,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$T8555000 <- factor(data$T8555000,
                          levels=c(1.0,2.0),
                          labels=c("A regular day schedule",
                                   "Some other schedule"))
  data$T8555500 <- factor(data$T8555500,
                          levels=c(2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0),
                          labels=c("A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)",
                                   "Irregular schedule (arranged by employee)",
                                   "A regular day shift"))
  data$T8620300 <- factor(data$T8620300,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$T8788500 <- factor(data$T8788500,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T8788700 <- factor(data$T8788700,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$T9103300 <- factor(data$T9103300,
                          levels=c(1.0,2.0),
                          labels=c("A regular day schedule",
                                   "Some other schedule"))
  data$T9103800 <- factor(data$T9103800,
                          levels=c(2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0),
                          labels=c("A regular evening shift",
                                   "A regular night shift",
                                   "A rotating shift (changes periodically from days to evenings or nights)",
                                   "A split shift (consists of two distinct periods each day)",
                                   "Irregular schedule (arranged by employer)",
                                   "Other (SPECIFY)",
                                   "Irregular schedule (arranged by employee)",
                                   "A regular day shift"))
  data$T9178100 <- factor(data$T9178100,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$T9300300 <- factor(data$T9300300,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T9300500 <- factor(data$T9300500,
                          levels=c(0.0,1.0),
                          labels=c("0: 0  NO",
                                   "1: 1  YES"))
  data$T9900000 <- factor(data$T9900000,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,93.0,94.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YEAR COLLEGE",
                                   "2ND YEAR COLLEGE",
                                   "3RD YEAR COLLEGE",
                                   "4TH YEAR COLLEGE",
                                   "5TH YEAR COLLEGE",
                                   "6TH YEAR COLLEGE",
                                   "7TH YEAR COLLEGE",
                                   "8TH YEAR COLLEGE OR MORE",
                                   "PRE-KINDERGARTEN",
                                   "KINDERGARTEN",
                                   "UNGRADED"))
  return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
  data$R1924800[0.0 <= data$R1924800 & data$R1924800 <= 300.0] <- 0.0
  data$R1924800[301.0 <= data$R1924800 & data$R1924800 <= 600.0] <- 301.0
  data$R1924800[601.0 <= data$R1924800 & data$R1924800 <= 900.0] <- 601.0
  data$R1924800[901.0 <= data$R1924800 & data$R1924800 <= 1200.0] <- 901.0
  data$R1924800[1201.0 <= data$R1924800 & data$R1924800 <= 1500.0] <- 1201.0
  data$R1924800[1501.0 <= data$R1924800 & data$R1924800 <= 1800.0] <- 1501.0
  data$R1924800[1801.0 <= data$R1924800 & data$R1924800 <= 2100.0] <- 1801.0
  data$R1924800[2101.0 <= data$R1924800 & data$R1924800 <= 2400.0] <- 2101.0
  data$R1924800 <- factor(data$R1924800,
                          levels=c(0.0,301.0,601.0,901.0,1201.0,1501.0,1801.0,2101.0),
                          labels=c("0 TO 300: 0- 300   :00 TO  3:00",
                                   "301 TO 600: 301- 600  3:01 TO  6:00",
                                   "601 TO 900: 601- 900  6:01 TO  9:00",
                                   "901 TO 1200: 901-1200  9:01 TO 12:00",
                                   "1201 TO 1500: 1201-1500 12:01 TO 15:00",
                                   "1501 TO 1800: 1501-1800 15:01 TO 18:00",
                                   "1801 TO 2100: 1801-2100 18:01 TO 21:00",
                                   "2101 TO 2400: 2101-2400 21:01 TO 24:00"))
  data$R2319600[0.0 <= data$R2319600 & data$R2319600 <= 300.0] <- 0.0
  data$R2319600[301.0 <= data$R2319600 & data$R2319600 <= 600.0] <- 301.0
  data$R2319600[601.0 <= data$R2319600 & data$R2319600 <= 900.0] <- 601.0
  data$R2319600[901.0 <= data$R2319600 & data$R2319600 <= 1200.0] <- 901.0
  data$R2319600[1201.0 <= data$R2319600 & data$R2319600 <= 1500.0] <- 1201.0
  data$R2319600[1501.0 <= data$R2319600 & data$R2319600 <= 1800.0] <- 1501.0
  data$R2319600[1801.0 <= data$R2319600 & data$R2319600 <= 2100.0] <- 1801.0
  data$R2319600[2101.0 <= data$R2319600 & data$R2319600 <= 2400.0] <- 2101.0
  data$R2319600 <- factor(data$R2319600,
                          levels=c(0.0,301.0,601.0,901.0,1201.0,1501.0,1801.0,2101.0),
                          labels=c("0 TO 300: 0- 300   :00 TO  3:00",
                                   "301 TO 600: 301- 600  3:01 TO  6:00",
                                   "601 TO 900: 601- 900  6:01 TO  9:00",
                                   "901 TO 1200: 901-1200  9:01 TO 12:00",
                                   "1201 TO 1500: 1201-1500 12:01 TO 15:00",
                                   "1501 TO 1800: 1501-1800 15:01 TO 18:00",
                                   "1801 TO 2100: 1801-2100 18:01 TO 21:00",
                                   "2101 TO 2400: 2101-2400 21:01 TO 24:00"))
  data$R2531600[0.0 <= data$R2531600 & data$R2531600 <= 300.0] <- 0.0
  data$R2531600[301.0 <= data$R2531600 & data$R2531600 <= 600.0] <- 301.0
  data$R2531600[601.0 <= data$R2531600 & data$R2531600 <= 900.0] <- 601.0
  data$R2531600[901.0 <= data$R2531600 & data$R2531600 <= 1200.0] <- 901.0
  data$R2531600[1201.0 <= data$R2531600 & data$R2531600 <= 1500.0] <- 1201.0
  data$R2531600[1501.0 <= data$R2531600 & data$R2531600 <= 1800.0] <- 1501.0
  data$R2531600[1801.0 <= data$R2531600 & data$R2531600 <= 2100.0] <- 1801.0
  data$R2531600[2101.0 <= data$R2531600 & data$R2531600 <= 2400.0] <- 2101.0
  data$R2531600[2401.0 <= data$R2531600 & data$R2531600 <= 2500.0] <- 2401.0
  data$R2531600 <- factor(data$R2531600,
                          levels=c(0.0,301.0,601.0,901.0,1201.0,1501.0,1801.0,2101.0,2401.0),
                          labels=c("0 TO 300: 0- 300   :00 TO  3:00",
                                   "301 TO 600: 301- 600  3:01 TO  6:00",
                                   "601 TO 900: 601- 900  6:01 TO  9:00",
                                   "901 TO 1200: 901-1200  9:01 TO 12:00",
                                   "1201 TO 1500: 1201-1500 12:01 TO 15:00",
                                   "1501 TO 1800: 1501-1800 15:01 TO 18:00",
                                   "1801 TO 2100: 1801-2100 18:01 TO 21:00",
                                   "2101 TO 2400: 2101-2400 21:01 TO 24:00",
                                   "2401 TO 2500: 24:00+"))
  data$R2929800[0.0 <= data$R2929800 & data$R2929800 <= 300.0] <- 0.0
  data$R2929800[301.0 <= data$R2929800 & data$R2929800 <= 600.0] <- 301.0
  data$R2929800[601.0 <= data$R2929800 & data$R2929800 <= 900.0] <- 601.0
  data$R2929800[901.0 <= data$R2929800 & data$R2929800 <= 1200.0] <- 901.0
  data$R2929800[1201.0 <= data$R2929800 & data$R2929800 <= 1500.0] <- 1201.0
  data$R2929800[1501.0 <= data$R2929800 & data$R2929800 <= 1800.0] <- 1501.0
  data$R2929800[1801.0 <= data$R2929800 & data$R2929800 <= 2100.0] <- 1801.0
  data$R2929800[2101.0 <= data$R2929800 & data$R2929800 <= 2400.0] <- 2101.0
  data$R2929800 <- factor(data$R2929800,
                          levels=c(0.0,301.0,601.0,901.0,1201.0,1501.0,1801.0,2101.0),
                          labels=c("0 TO 300: 0- 300   :00 TO  3:00",
                                   "301 TO 600: 301- 600  3:01 TO  6:00",
                                   "601 TO 900: 601- 900  6:01 TO  9:00",
                                   "901 TO 1200: 901-1200  9:01 TO 12:00",
                                   "1201 TO 1500: 1201-1500 12:01 TO 15:00",
                                   "1501 TO 1800: 1501-1800 15:01 TO 18:00",
                                   "1801 TO 2100: 1801-2100 18:01 TO 21:00",
                                   "2101 TO 2400: 2101-2400 21:01 TO 24:00"))
  data$R3135400[0.0 <= data$R3135400 & data$R3135400 <= 300.0] <- 0.0
  data$R3135400[301.0 <= data$R3135400 & data$R3135400 <= 600.0] <- 301.0
  data$R3135400[601.0 <= data$R3135400 & data$R3135400 <= 900.0] <- 601.0
  data$R3135400[901.0 <= data$R3135400 & data$R3135400 <= 1200.0] <- 901.0
  data$R3135400[1201.0 <= data$R3135400 & data$R3135400 <= 1500.0] <- 1201.0
  data$R3135400[1501.0 <= data$R3135400 & data$R3135400 <= 1800.0] <- 1501.0
  data$R3135400[1801.0 <= data$R3135400 & data$R3135400 <= 2100.0] <- 1801.0
  data$R3135400[2101.0 <= data$R3135400 & data$R3135400 <= 2400.0] <- 2101.0
  data$R3135400 <- factor(data$R3135400,
                          levels=c(0.0,301.0,601.0,901.0,1201.0,1501.0,1801.0,2101.0),
                          labels=c("0 TO 300: 0- 300   :00 TO  3:00",
                                   "301 TO 600: 301- 600  3:01 TO  6:00",
                                   "601 TO 900: 601- 900  6:01 TO  9:00",
                                   "901 TO 1200: 901-1200  9:01 TO 12:00",
                                   "1201 TO 1500: 1201-1500 12:01 TO 15:00",
                                   "1501 TO 1800: 1501-1800 15:01 TO 18:00",
                                   "1801 TO 2100: 1801-2100 18:01 TO 21:00",
                                   "2101 TO 2400: 2101-2400 21:01 TO 24:00"))
  data$R3341500[1.0 <= data$R3341500 & data$R3341500 <= 99.0] <- 1.0
  data$R3341500[100.0 <= data$R3341500 & data$R3341500 <= 199.0] <- 100.0
  data$R3341500[200.0 <= data$R3341500 & data$R3341500 <= 299.0] <- 200.0
  data$R3341500[300.0 <= data$R3341500 & data$R3341500 <= 399.0] <- 300.0
  data$R3341500[400.0 <= data$R3341500 & data$R3341500 <= 499.0] <- 400.0
  data$R3341500[500.0 <= data$R3341500 & data$R3341500 <= 599.0] <- 500.0
  data$R3341500[600.0 <= data$R3341500 & data$R3341500 <= 699.0] <- 600.0
  data$R3341500[700.0 <= data$R3341500 & data$R3341500 <= 799.0] <- 700.0
  data$R3341500[800.0 <= data$R3341500 & data$R3341500 <= 899.0] <- 800.0
  data$R3341500[900.0 <= data$R3341500 & data$R3341500 <= 999.0] <- 900.0
  data$R3341500[1000.0 <= data$R3341500 & data$R3341500 <= 9999999.0] <- 1000.0
  data$R3341500 <- factor(data$R3341500,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 9999999: 1000+"))
  data$R3400600[17.0 <= data$R3400600 & data$R3400600 <= 99999.0] <- 17.0
  data$R3400600 <- factor(data$R3400600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$R3403500[16.0 <= data$R3403500 & data$R3403500 <= 99999.0] <- 16.0
  data$R3403500 <- factor(data$R3403500,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$R3407700[16.0 <= data$R3407700 & data$R3407700 <= 99999.0] <- 16.0
  data$R3407700 <- factor(data$R3407700,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$R3525800[0.0 <= data$R3525800 & data$R3525800 <= 300.0] <- 0.0
  data$R3525800[301.0 <= data$R3525800 & data$R3525800 <= 600.0] <- 301.0
  data$R3525800[601.0 <= data$R3525800 & data$R3525800 <= 900.0] <- 601.0
  data$R3525800[901.0 <= data$R3525800 & data$R3525800 <= 1200.0] <- 901.0
  data$R3525800[1201.0 <= data$R3525800 & data$R3525800 <= 1500.0] <- 1201.0
  data$R3525800[1501.0 <= data$R3525800 & data$R3525800 <= 1800.0] <- 1501.0
  data$R3525800[1801.0 <= data$R3525800 & data$R3525800 <= 2100.0] <- 1801.0
  data$R3525800[2101.0 <= data$R3525800 & data$R3525800 <= 2400.0] <- 2101.0
  data$R3525800 <- factor(data$R3525800,
                          levels=c(0.0,301.0,601.0,901.0,1201.0,1501.0,1801.0,2101.0),
                          labels=c("0 TO 300: 0- 300   :00 TO  3:00",
                                   "301 TO 600: 301- 600  3:01 TO  6:00",
                                   "601 TO 900: 601- 900  6:01 TO  9:00",
                                   "901 TO 1200: 901-1200  9:01 TO 12:00",
                                   "1201 TO 1500: 1201-1500 12:01 TO 15:00",
                                   "1501 TO 1800: 1501-1800 15:01 TO 18:00",
                                   "1801 TO 2100: 1801-2100 18:01 TO 21:00",
                                   "2101 TO 2400: 2101-2400 21:01 TO 24:00"))
  data$R3605800[1.0 <= data$R3605800 & data$R3605800 <= 99.0] <- 1.0
  data$R3605800[100.0 <= data$R3605800 & data$R3605800 <= 199.0] <- 100.0
  data$R3605800[200.0 <= data$R3605800 & data$R3605800 <= 299.0] <- 200.0
  data$R3605800[300.0 <= data$R3605800 & data$R3605800 <= 399.0] <- 300.0
  data$R3605800[400.0 <= data$R3605800 & data$R3605800 <= 499.0] <- 400.0
  data$R3605800[500.0 <= data$R3605800 & data$R3605800 <= 599.0] <- 500.0
  data$R3605800[600.0 <= data$R3605800 & data$R3605800 <= 699.0] <- 600.0
  data$R3605800[700.0 <= data$R3605800 & data$R3605800 <= 799.0] <- 700.0
  data$R3605800[800.0 <= data$R3605800 & data$R3605800 <= 899.0] <- 800.0
  data$R3605800[900.0 <= data$R3605800 & data$R3605800 <= 999.0] <- 900.0
  data$R3605800[1000.0 <= data$R3605800 & data$R3605800 <= 9999999.0] <- 1000.0
  data$R3605800 <- factor(data$R3605800,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 9999999: 1000+"))
  data$R3656000[17.0 <= data$R3656000 & data$R3656000 <= 99999.0] <- 17.0
  data$R3656000 <- factor(data$R3656000,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$R3658900[16.0 <= data$R3658900 & data$R3658900 <= 99999.0] <- 16.0
  data$R3658900 <- factor(data$R3658900,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$R3659047[16.0 <= data$R3659047 & data$R3659047 <= 99999.0] <- 16.0
  data$R3659047 <- factor(data$R3659047,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$R3730900[0.0 <= data$R3730900 & data$R3730900 <= 300.0] <- 0.0
  data$R3730900[301.0 <= data$R3730900 & data$R3730900 <= 600.0] <- 301.0
  data$R3730900[601.0 <= data$R3730900 & data$R3730900 <= 900.0] <- 601.0
  data$R3730900[901.0 <= data$R3730900 & data$R3730900 <= 1200.0] <- 901.0
  data$R3730900[1201.0 <= data$R3730900 & data$R3730900 <= 1500.0] <- 1201.0
  data$R3730900[1501.0 <= data$R3730900 & data$R3730900 <= 1800.0] <- 1501.0
  data$R3730900[1801.0 <= data$R3730900 & data$R3730900 <= 2100.0] <- 1801.0
  data$R3730900[2101.0 <= data$R3730900 & data$R3730900 <= 2400.0] <- 2101.0
  data$R3730900 <- factor(data$R3730900,
                          levels=c(0.0,301.0,601.0,901.0,1201.0,1501.0,1801.0,2101.0),
                          labels=c("0 TO 300: 0- 300   :00 TO  3:00",
                                   "301 TO 600: 301- 600  3:01 TO  6:00",
                                   "601 TO 900: 601- 900  6:01 TO  9:00",
                                   "901 TO 1200: 901-1200  9:01 TO 12:00",
                                   "1201 TO 1500: 1201-1500 12:01 TO 15:00",
                                   "1501 TO 1800: 1501-1800 15:01 TO 18:00",
                                   "1801 TO 2100: 1801-2100 18:01 TO 21:00",
                                   "2101 TO 2400: 2101-2400 21:01 TO 24:00"))
  data$R3956000[1.0 <= data$R3956000 & data$R3956000 <= 99.0] <- 1.0
  data$R3956000[100.0 <= data$R3956000 & data$R3956000 <= 199.0] <- 100.0
  data$R3956000[200.0 <= data$R3956000 & data$R3956000 <= 299.0] <- 200.0
  data$R3956000[300.0 <= data$R3956000 & data$R3956000 <= 399.0] <- 300.0
  data$R3956000[400.0 <= data$R3956000 & data$R3956000 <= 499.0] <- 400.0
  data$R3956000[500.0 <= data$R3956000 & data$R3956000 <= 599.0] <- 500.0
  data$R3956000[600.0 <= data$R3956000 & data$R3956000 <= 699.0] <- 600.0
  data$R3956000[700.0 <= data$R3956000 & data$R3956000 <= 799.0] <- 700.0
  data$R3956000[800.0 <= data$R3956000 & data$R3956000 <= 899.0] <- 800.0
  data$R3956000[900.0 <= data$R3956000 & data$R3956000 <= 999.0] <- 900.0
  data$R3956000[1000.0 <= data$R3956000 & data$R3956000 <= 9999999.0] <- 1000.0
  data$R3956000 <- factor(data$R3956000,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 9999999: 1000+"))
  data$R4006500[17.0 <= data$R4006500 & data$R4006500 <= 99999.0] <- 17.0
  data$R4006500 <- factor(data$R4006500,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$R4009400[16.0 <= data$R4009400 & data$R4009400 <= 99999.0] <- 16.0
  data$R4009400 <- factor(data$R4009400,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$R4009447[16.0 <= data$R4009447 & data$R4009447 <= 99999.0] <- 16.0
  data$R4009447 <- factor(data$R4009447,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$R4186800[0.0 <= data$R4186800 & data$R4186800 <= 300.0] <- 0.0
  data$R4186800[301.0 <= data$R4186800 & data$R4186800 <= 600.0] <- 301.0
  data$R4186800[601.0 <= data$R4186800 & data$R4186800 <= 900.0] <- 601.0
  data$R4186800[901.0 <= data$R4186800 & data$R4186800 <= 1200.0] <- 901.0
  data$R4186800[1201.0 <= data$R4186800 & data$R4186800 <= 1500.0] <- 1201.0
  data$R4186800[1501.0 <= data$R4186800 & data$R4186800 <= 1800.0] <- 1501.0
  data$R4186800[1801.0 <= data$R4186800 & data$R4186800 <= 2100.0] <- 1801.0
  data$R4186800[2101.0 <= data$R4186800 & data$R4186800 <= 2400.0] <- 2101.0
  data$R4186800 <- factor(data$R4186800,
                          levels=c(0.0,301.0,601.0,901.0,1201.0,1501.0,1801.0,2101.0),
                          labels=c("0 TO 300: 0- 300   :00 TO  3:00",
                                   "301 TO 600: 301- 600  3:01 TO  6:00",
                                   "601 TO 900: 601- 900  6:01 TO  9:00",
                                   "901 TO 1200: 901-1200  9:01 TO 12:00",
                                   "1201 TO 1500: 1201-1500 12:01 TO 15:00",
                                   "1501 TO 1800: 1501-1800 15:01 TO 18:00",
                                   "1801 TO 2100: 1801-2100 18:01 TO 21:00",
                                   "2101 TO 2400: 2101-2400 21:01 TO 24:00"))
  data$R4416900[1.0 <= data$R4416900 & data$R4416900 <= 99.0] <- 1.0
  data$R4416900[100.0 <= data$R4416900 & data$R4416900 <= 199.0] <- 100.0
  data$R4416900[200.0 <= data$R4416900 & data$R4416900 <= 299.0] <- 200.0
  data$R4416900[300.0 <= data$R4416900 & data$R4416900 <= 399.0] <- 300.0
  data$R4416900[400.0 <= data$R4416900 & data$R4416900 <= 499.0] <- 400.0
  data$R4416900[500.0 <= data$R4416900 & data$R4416900 <= 599.0] <- 500.0
  data$R4416900[600.0 <= data$R4416900 & data$R4416900 <= 699.0] <- 600.0
  data$R4416900[700.0 <= data$R4416900 & data$R4416900 <= 799.0] <- 700.0
  data$R4416900[800.0 <= data$R4416900 & data$R4416900 <= 899.0] <- 800.0
  data$R4416900[900.0 <= data$R4416900 & data$R4416900 <= 999.0] <- 900.0
  data$R4416900[1000.0 <= data$R4416900 & data$R4416900 <= 9999999.0] <- 1000.0
  data$R4416900 <- factor(data$R4416900,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 9999999: 1000+"))
  data$R4417600[16.0 <= data$R4417600 & data$R4417600 <= 99999.0] <- 16.0
  data$R4417600 <- factor(data$R4417600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$R4420500[16.0 <= data$R4420500 & data$R4420500 <= 99999.0] <- 16.0
  data$R4420500 <- factor(data$R4420500,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$R4444700[16.0 <= data$R4444700 & data$R4444700 <= 99999.0] <- 16.0
  data$R4444700 <- factor(data$R4444700,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$R5079900[1.0 <= data$R5079900 & data$R5079900 <= 499.0] <- 1.0
  data$R5079900[500.0 <= data$R5079900 & data$R5079900 <= 999.0] <- 500.0
  data$R5079900[1000.0 <= data$R5079900 & data$R5079900 <= 1499.0] <- 1000.0
  data$R5079900[1500.0 <= data$R5079900 & data$R5079900 <= 1999.0] <- 1500.0
  data$R5079900[2000.0 <= data$R5079900 & data$R5079900 <= 2499.0] <- 2000.0
  data$R5079900[2500.0 <= data$R5079900 & data$R5079900 <= 2999.0] <- 2500.0
  data$R5079900[3000.0 <= data$R5079900 & data$R5079900 <= 3499.0] <- 3000.0
  data$R5079900[3500.0 <= data$R5079900 & data$R5079900 <= 3999.0] <- 3500.0
  data$R5079900[4000.0 <= data$R5079900 & data$R5079900 <= 4499.0] <- 4000.0
  data$R5079900[4500.0 <= data$R5079900 & data$R5079900 <= 4999.0] <- 4500.0
  data$R5079900[5000.0 <= data$R5079900 & data$R5079900 <= 9.9999999E7] <- 5000.0
  data$R5079900 <- factor(data$R5079900,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499",
                                   "500 TO 999",
                                   "1000 TO 1499",
                                   "1500 TO 1999",
                                   "2000 TO 2499",
                                   "2500 TO 2999",
                                   "3000 TO 3499",
                                   "3500 TO 3999",
                                   "4000 TO 4499",
                                   "4500 TO 4999",
                                   "5000 TO 99999999: 5000+"))
  data$R5080600[10.0 <= data$R5080600 & data$R5080600 <= 999.0] <- 10.0
  data$R5080600 <- factor(data$R5080600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R5083500[1.0 <= data$R5083500 & data$R5083500 <= 4.0] <- 1.0
  data$R5083500[5.0 <= data$R5083500 & data$R5083500 <= 9.0] <- 5.0
  data$R5083500[10.0 <= data$R5083500 & data$R5083500 <= 14.0] <- 10.0
  data$R5083500[15.0 <= data$R5083500 & data$R5083500 <= 19.0] <- 15.0
  data$R5083500[20.0 <= data$R5083500 & data$R5083500 <= 24.0] <- 20.0
  data$R5083500[25.0 <= data$R5083500 & data$R5083500 <= 29.0] <- 25.0
  data$R5083500[30.0 <= data$R5083500 & data$R5083500 <= 34.0] <- 30.0
  data$R5083500[35.0 <= data$R5083500 & data$R5083500 <= 39.0] <- 35.0
  data$R5083500[40.0 <= data$R5083500 & data$R5083500 <= 44.0] <- 40.0
  data$R5083500[45.0 <= data$R5083500 & data$R5083500 <= 49.0] <- 45.0
  data$R5083500[50.0 <= data$R5083500 & data$R5083500 <= 9.9999999E7] <- 50.0
  data$R5083500 <- factor(data$R5083500,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$R5087500[10.0 <= data$R5087500 & data$R5087500 <= 999.0] <- 10.0
  data$R5087500 <- factor(data$R5087500,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R5165200[1.0 <= data$R5165200 & data$R5165200 <= 499.0] <- 1.0
  data$R5165200[500.0 <= data$R5165200 & data$R5165200 <= 999.0] <- 500.0
  data$R5165200[1000.0 <= data$R5165200 & data$R5165200 <= 1499.0] <- 1000.0
  data$R5165200[1500.0 <= data$R5165200 & data$R5165200 <= 1999.0] <- 1500.0
  data$R5165200[2000.0 <= data$R5165200 & data$R5165200 <= 2499.0] <- 2000.0
  data$R5165200[2500.0 <= data$R5165200 & data$R5165200 <= 2999.0] <- 2500.0
  data$R5165200[3000.0 <= data$R5165200 & data$R5165200 <= 3499.0] <- 3000.0
  data$R5165200[3500.0 <= data$R5165200 & data$R5165200 <= 3999.0] <- 3500.0
  data$R5165200[4000.0 <= data$R5165200 & data$R5165200 <= 4499.0] <- 4000.0
  data$R5165200[4500.0 <= data$R5165200 & data$R5165200 <= 4999.0] <- 4500.0
  data$R5165200[5000.0 <= data$R5165200 & data$R5165200 <= 9.9999999E7] <- 5000.0
  data$R5165200 <- factor(data$R5165200,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499",
                                   "500 TO 999",
                                   "1000 TO 1499",
                                   "1500 TO 1999",
                                   "2000 TO 2499",
                                   "2500 TO 2999",
                                   "3000 TO 3499",
                                   "3500 TO 3999",
                                   "4000 TO 4499",
                                   "4500 TO 4999",
                                   "5000 TO 99999999: 5000+"))
  data$R5165900[10.0 <= data$R5165900 & data$R5165900 <= 999.0] <- 10.0
  data$R5165900 <- factor(data$R5165900,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R5168800[1.0 <= data$R5168800 & data$R5168800 <= 4.0] <- 1.0
  data$R5168800[5.0 <= data$R5168800 & data$R5168800 <= 9.0] <- 5.0
  data$R5168800[10.0 <= data$R5168800 & data$R5168800 <= 14.0] <- 10.0
  data$R5168800[15.0 <= data$R5168800 & data$R5168800 <= 19.0] <- 15.0
  data$R5168800[20.0 <= data$R5168800 & data$R5168800 <= 24.0] <- 20.0
  data$R5168800[25.0 <= data$R5168800 & data$R5168800 <= 29.0] <- 25.0
  data$R5168800[30.0 <= data$R5168800 & data$R5168800 <= 34.0] <- 30.0
  data$R5168800[35.0 <= data$R5168800 & data$R5168800 <= 39.0] <- 35.0
  data$R5168800[40.0 <= data$R5168800 & data$R5168800 <= 44.0] <- 40.0
  data$R5168800[45.0 <= data$R5168800 & data$R5168800 <= 49.0] <- 45.0
  data$R5168800[50.0 <= data$R5168800 & data$R5168800 <= 9.9999999E7] <- 50.0
  data$R5168800 <- factor(data$R5168800,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$R5172800[10.0 <= data$R5172800 & data$R5172800 <= 999.0] <- 10.0
  data$R5172800 <- factor(data$R5172800,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R6478000[1.0 <= data$R6478000 & data$R6478000 <= 499.0] <- 1.0
  data$R6478000[500.0 <= data$R6478000 & data$R6478000 <= 999.0] <- 500.0
  data$R6478000[1000.0 <= data$R6478000 & data$R6478000 <= 1499.0] <- 1000.0
  data$R6478000[1500.0 <= data$R6478000 & data$R6478000 <= 1999.0] <- 1500.0
  data$R6478000[2000.0 <= data$R6478000 & data$R6478000 <= 2499.0] <- 2000.0
  data$R6478000[2500.0 <= data$R6478000 & data$R6478000 <= 2999.0] <- 2500.0
  data$R6478000[3000.0 <= data$R6478000 & data$R6478000 <= 3499.0] <- 3000.0
  data$R6478000[3500.0 <= data$R6478000 & data$R6478000 <= 3999.0] <- 3500.0
  data$R6478000[4000.0 <= data$R6478000 & data$R6478000 <= 4499.0] <- 4000.0
  data$R6478000[4500.0 <= data$R6478000 & data$R6478000 <= 4999.0] <- 4500.0
  data$R6478000[5000.0 <= data$R6478000 & data$R6478000 <= 9.9999999E7] <- 5000.0
  data$R6478000 <- factor(data$R6478000,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499",
                                   "500 TO 999",
                                   "1000 TO 1499",
                                   "1500 TO 1999",
                                   "2000 TO 2499",
                                   "2500 TO 2999",
                                   "3000 TO 3499",
                                   "3500 TO 3999",
                                   "4000 TO 4499",
                                   "4500 TO 4999",
                                   "5000 TO 99999999: 5000+"))
  data$R6478600[10.0 <= data$R6478600 & data$R6478600 <= 999.0] <- 10.0
  data$R6478600 <- factor(data$R6478600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R6481600[1.0 <= data$R6481600 & data$R6481600 <= 4.0] <- 1.0
  data$R6481600[5.0 <= data$R6481600 & data$R6481600 <= 9.0] <- 5.0
  data$R6481600[10.0 <= data$R6481600 & data$R6481600 <= 14.0] <- 10.0
  data$R6481600[15.0 <= data$R6481600 & data$R6481600 <= 19.0] <- 15.0
  data$R6481600[20.0 <= data$R6481600 & data$R6481600 <= 24.0] <- 20.0
  data$R6481600[25.0 <= data$R6481600 & data$R6481600 <= 29.0] <- 25.0
  data$R6481600[30.0 <= data$R6481600 & data$R6481600 <= 34.0] <- 30.0
  data$R6481600[35.0 <= data$R6481600 & data$R6481600 <= 39.0] <- 35.0
  data$R6481600[40.0 <= data$R6481600 & data$R6481600 <= 44.0] <- 40.0
  data$R6481600[45.0 <= data$R6481600 & data$R6481600 <= 49.0] <- 45.0
  data$R6481600[50.0 <= data$R6481600 & data$R6481600 <= 9.9999999E7] <- 50.0
  data$R6481600 <- factor(data$R6481600,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$R6486400[10.0 <= data$R6486400 & data$R6486400 <= 999.0] <- 10.0
  data$R6486400 <- factor(data$R6486400,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R7005700[1.0 <= data$R7005700 & data$R7005700 <= 499.0] <- 1.0
  data$R7005700[500.0 <= data$R7005700 & data$R7005700 <= 999.0] <- 500.0
  data$R7005700[1000.0 <= data$R7005700 & data$R7005700 <= 1499.0] <- 1000.0
  data$R7005700[1500.0 <= data$R7005700 & data$R7005700 <= 1999.0] <- 1500.0
  data$R7005700[2000.0 <= data$R7005700 & data$R7005700 <= 2499.0] <- 2000.0
  data$R7005700[2500.0 <= data$R7005700 & data$R7005700 <= 2999.0] <- 2500.0
  data$R7005700[3000.0 <= data$R7005700 & data$R7005700 <= 3499.0] <- 3000.0
  data$R7005700[3500.0 <= data$R7005700 & data$R7005700 <= 3999.0] <- 3500.0
  data$R7005700[4000.0 <= data$R7005700 & data$R7005700 <= 4499.0] <- 4000.0
  data$R7005700[4500.0 <= data$R7005700 & data$R7005700 <= 4999.0] <- 4500.0
  data$R7005700[5000.0 <= data$R7005700 & data$R7005700 <= 9.9999999E7] <- 5000.0
  data$R7005700 <- factor(data$R7005700,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499",
                                   "500 TO 999",
                                   "1000 TO 1499",
                                   "1500 TO 1999",
                                   "2000 TO 2499",
                                   "2500 TO 2999",
                                   "3000 TO 3499",
                                   "3500 TO 3999",
                                   "4000 TO 4499",
                                   "4500 TO 4999",
                                   "5000 TO 99999999: 5000+"))
  data$R7006400[10.0 <= data$R7006400 & data$R7006400 <= 999.0] <- 10.0
  data$R7006400 <- factor(data$R7006400,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R7009300[1.0 <= data$R7009300 & data$R7009300 <= 4.0] <- 1.0
  data$R7009300[5.0 <= data$R7009300 & data$R7009300 <= 9.0] <- 5.0
  data$R7009300[10.0 <= data$R7009300 & data$R7009300 <= 14.0] <- 10.0
  data$R7009300[15.0 <= data$R7009300 & data$R7009300 <= 19.0] <- 15.0
  data$R7009300[20.0 <= data$R7009300 & data$R7009300 <= 24.0] <- 20.0
  data$R7009300[25.0 <= data$R7009300 & data$R7009300 <= 29.0] <- 25.0
  data$R7009300[30.0 <= data$R7009300 & data$R7009300 <= 34.0] <- 30.0
  data$R7009300[35.0 <= data$R7009300 & data$R7009300 <= 39.0] <- 35.0
  data$R7009300[40.0 <= data$R7009300 & data$R7009300 <= 44.0] <- 40.0
  data$R7009300[45.0 <= data$R7009300 & data$R7009300 <= 49.0] <- 45.0
  data$R7009300[50.0 <= data$R7009300 & data$R7009300 <= 9.9999999E7] <- 50.0
  data$R7009300 <- factor(data$R7009300,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$R7014200[10.0 <= data$R7014200 & data$R7014200 <= 999.0] <- 10.0
  data$R7014200 <- factor(data$R7014200,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R7702900[1.0 <= data$R7702900 & data$R7702900 <= 499.0] <- 1.0
  data$R7702900[500.0 <= data$R7702900 & data$R7702900 <= 999.0] <- 500.0
  data$R7702900[1000.0 <= data$R7702900 & data$R7702900 <= 1499.0] <- 1000.0
  data$R7702900[1500.0 <= data$R7702900 & data$R7702900 <= 1999.0] <- 1500.0
  data$R7702900[2000.0 <= data$R7702900 & data$R7702900 <= 2499.0] <- 2000.0
  data$R7702900[2500.0 <= data$R7702900 & data$R7702900 <= 2999.0] <- 2500.0
  data$R7702900[3000.0 <= data$R7702900 & data$R7702900 <= 3499.0] <- 3000.0
  data$R7702900[3500.0 <= data$R7702900 & data$R7702900 <= 3999.0] <- 3500.0
  data$R7702900[4000.0 <= data$R7702900 & data$R7702900 <= 4499.0] <- 4000.0
  data$R7702900[4500.0 <= data$R7702900 & data$R7702900 <= 4999.0] <- 4500.0
  data$R7702900[5000.0 <= data$R7702900 & data$R7702900 <= 9.9999999E7] <- 5000.0
  data$R7702900 <- factor(data$R7702900,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499",
                                   "500 TO 999",
                                   "1000 TO 1499",
                                   "1500 TO 1999",
                                   "2000 TO 2499",
                                   "2500 TO 2999",
                                   "3000 TO 3499",
                                   "3500 TO 3999",
                                   "4000 TO 4499",
                                   "4500 TO 4999",
                                   "5000 TO 99999999: 5000+"))
  data$R7703600[10.0 <= data$R7703600 & data$R7703600 <= 999.0] <- 10.0
  data$R7703600 <- factor(data$R7703600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R7706600[1.0 <= data$R7706600 & data$R7706600 <= 4.0] <- 1.0
  data$R7706600[5.0 <= data$R7706600 & data$R7706600 <= 9.0] <- 5.0
  data$R7706600[10.0 <= data$R7706600 & data$R7706600 <= 14.0] <- 10.0
  data$R7706600[15.0 <= data$R7706600 & data$R7706600 <= 19.0] <- 15.0
  data$R7706600[20.0 <= data$R7706600 & data$R7706600 <= 24.0] <- 20.0
  data$R7706600[25.0 <= data$R7706600 & data$R7706600 <= 29.0] <- 25.0
  data$R7706600[30.0 <= data$R7706600 & data$R7706600 <= 34.0] <- 30.0
  data$R7706600[35.0 <= data$R7706600 & data$R7706600 <= 39.0] <- 35.0
  data$R7706600[40.0 <= data$R7706600 & data$R7706600 <= 44.0] <- 40.0
  data$R7706600[45.0 <= data$R7706600 & data$R7706600 <= 49.0] <- 45.0
  data$R7706600[50.0 <= data$R7706600 & data$R7706600 <= 9.9999999E7] <- 50.0
  data$R7706600 <- factor(data$R7706600,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$R7711800[10.0 <= data$R7711800 & data$R7711800 <= 999.0] <- 10.0
  data$R7711800 <- factor(data$R7711800,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R8495200[1.0 <= data$R8495200 & data$R8495200 <= 499.0] <- 1.0
  data$R8495200[500.0 <= data$R8495200 & data$R8495200 <= 999.0] <- 500.0
  data$R8495200[1000.0 <= data$R8495200 & data$R8495200 <= 1499.0] <- 1000.0
  data$R8495200[1500.0 <= data$R8495200 & data$R8495200 <= 1999.0] <- 1500.0
  data$R8495200[2000.0 <= data$R8495200 & data$R8495200 <= 2499.0] <- 2000.0
  data$R8495200[2500.0 <= data$R8495200 & data$R8495200 <= 2999.0] <- 2500.0
  data$R8495200[3000.0 <= data$R8495200 & data$R8495200 <= 3499.0] <- 3000.0
  data$R8495200[3500.0 <= data$R8495200 & data$R8495200 <= 3999.0] <- 3500.0
  data$R8495200[4000.0 <= data$R8495200 & data$R8495200 <= 4499.0] <- 4000.0
  data$R8495200[4500.0 <= data$R8495200 & data$R8495200 <= 4999.0] <- 4500.0
  data$R8495200[5000.0 <= data$R8495200 & data$R8495200 <= 9.9999999E7] <- 5000.0
  data$R8495200 <- factor(data$R8495200,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499",
                                   "500 TO 999",
                                   "1000 TO 1499",
                                   "1500 TO 1999",
                                   "2000 TO 2499",
                                   "2500 TO 2999",
                                   "3000 TO 3499",
                                   "3500 TO 3999",
                                   "4000 TO 4499",
                                   "4500 TO 4999",
                                   "5000 TO 99999999: 5000+"))
  data$R8496000[10.0 <= data$R8496000 & data$R8496000 <= 999.0] <- 10.0
  data$R8496000 <- factor(data$R8496000,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R8499000[1.0 <= data$R8499000 & data$R8499000 <= 4.0] <- 1.0
  data$R8499000[5.0 <= data$R8499000 & data$R8499000 <= 9.0] <- 5.0
  data$R8499000[10.0 <= data$R8499000 & data$R8499000 <= 14.0] <- 10.0
  data$R8499000[15.0 <= data$R8499000 & data$R8499000 <= 19.0] <- 15.0
  data$R8499000[20.0 <= data$R8499000 & data$R8499000 <= 24.0] <- 20.0
  data$R8499000[25.0 <= data$R8499000 & data$R8499000 <= 29.0] <- 25.0
  data$R8499000[30.0 <= data$R8499000 & data$R8499000 <= 34.0] <- 30.0
  data$R8499000[35.0 <= data$R8499000 & data$R8499000 <= 39.0] <- 35.0
  data$R8499000[40.0 <= data$R8499000 & data$R8499000 <= 44.0] <- 40.0
  data$R8499000[45.0 <= data$R8499000 & data$R8499000 <= 49.0] <- 45.0
  data$R8499000[50.0 <= data$R8499000 & data$R8499000 <= 9.9999999E7] <- 50.0
  data$R8499000 <- factor(data$R8499000,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$R8504300[10.0 <= data$R8504300 & data$R8504300 <= 999.0] <- 10.0
  data$R8504300 <- factor(data$R8504300,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T0986800[1.0 <= data$T0986800 & data$T0986800 <= 499.0] <- 1.0
  data$T0986800[500.0 <= data$T0986800 & data$T0986800 <= 999.0] <- 500.0
  data$T0986800[1000.0 <= data$T0986800 & data$T0986800 <= 1499.0] <- 1000.0
  data$T0986800[1500.0 <= data$T0986800 & data$T0986800 <= 1999.0] <- 1500.0
  data$T0986800[2000.0 <= data$T0986800 & data$T0986800 <= 2499.0] <- 2000.0
  data$T0986800[2500.0 <= data$T0986800 & data$T0986800 <= 2999.0] <- 2500.0
  data$T0986800[3000.0 <= data$T0986800 & data$T0986800 <= 3499.0] <- 3000.0
  data$T0986800[3500.0 <= data$T0986800 & data$T0986800 <= 3999.0] <- 3500.0
  data$T0986800[4000.0 <= data$T0986800 & data$T0986800 <= 4499.0] <- 4000.0
  data$T0986800[4500.0 <= data$T0986800 & data$T0986800 <= 4999.0] <- 4500.0
  data$T0986800[5000.0 <= data$T0986800 & data$T0986800 <= 9.9999999E7] <- 5000.0
  data$T0986800 <- factor(data$T0986800,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499",
                                   "500 TO 999",
                                   "1000 TO 1499",
                                   "1500 TO 1999",
                                   "2000 TO 2499",
                                   "2500 TO 2999",
                                   "3000 TO 3499",
                                   "3500 TO 3999",
                                   "4000 TO 4499",
                                   "4500 TO 4999",
                                   "5000 TO 99999999: 5000+"))
  data$T0987600[10.0 <= data$T0987600 & data$T0987600 <= 999.0] <- 10.0
  data$T0987600 <- factor(data$T0987600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T0990700[1.0 <= data$T0990700 & data$T0990700 <= 4.0] <- 1.0
  data$T0990700[5.0 <= data$T0990700 & data$T0990700 <= 9.0] <- 5.0
  data$T0990700[10.0 <= data$T0990700 & data$T0990700 <= 14.0] <- 10.0
  data$T0990700[15.0 <= data$T0990700 & data$T0990700 <= 19.0] <- 15.0
  data$T0990700[20.0 <= data$T0990700 & data$T0990700 <= 24.0] <- 20.0
  data$T0990700[25.0 <= data$T0990700 & data$T0990700 <= 29.0] <- 25.0
  data$T0990700[30.0 <= data$T0990700 & data$T0990700 <= 34.0] <- 30.0
  data$T0990700[35.0 <= data$T0990700 & data$T0990700 <= 39.0] <- 35.0
  data$T0990700[40.0 <= data$T0990700 & data$T0990700 <= 44.0] <- 40.0
  data$T0990700[45.0 <= data$T0990700 & data$T0990700 <= 49.0] <- 45.0
  data$T0990700[50.0 <= data$T0990700 & data$T0990700 <= 9.9999999E7] <- 50.0
  data$T0990700 <- factor(data$T0990700,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$T0996000[10.0 <= data$T0996000 & data$T0996000 <= 999.0] <- 10.0
  data$T0996000 <- factor(data$T0996000,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T2209100[1.0 <= data$T2209100 & data$T2209100 <= 499.0] <- 1.0
  data$T2209100[500.0 <= data$T2209100 & data$T2209100 <= 999.0] <- 500.0
  data$T2209100[1000.0 <= data$T2209100 & data$T2209100 <= 1499.0] <- 1000.0
  data$T2209100[1500.0 <= data$T2209100 & data$T2209100 <= 1999.0] <- 1500.0
  data$T2209100[2000.0 <= data$T2209100 & data$T2209100 <= 2499.0] <- 2000.0
  data$T2209100[2500.0 <= data$T2209100 & data$T2209100 <= 2999.0] <- 2500.0
  data$T2209100[3000.0 <= data$T2209100 & data$T2209100 <= 3499.0] <- 3000.0
  data$T2209100[3500.0 <= data$T2209100 & data$T2209100 <= 3999.0] <- 3500.0
  data$T2209100[4000.0 <= data$T2209100 & data$T2209100 <= 4499.0] <- 4000.0
  data$T2209100[4500.0 <= data$T2209100 & data$T2209100 <= 4999.0] <- 4500.0
  data$T2209100[5000.0 <= data$T2209100 & data$T2209100 <= 9.9999999E7] <- 5000.0
  data$T2209100 <- factor(data$T2209100,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499",
                                   "500 TO 999",
                                   "1000 TO 1499",
                                   "1500 TO 1999",
                                   "2000 TO 2499",
                                   "2500 TO 2999",
                                   "3000 TO 3499",
                                   "3500 TO 3999",
                                   "4000 TO 4499",
                                   "4500 TO 4999",
                                   "5000 TO 99999999: 5000+"))
  data$T2209900[10.0 <= data$T2209900 & data$T2209900 <= 999.0] <- 10.0
  data$T2209900 <- factor(data$T2209900,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T2212500[1.0 <= data$T2212500 & data$T2212500 <= 4.0] <- 1.0
  data$T2212500[5.0 <= data$T2212500 & data$T2212500 <= 9.0] <- 5.0
  data$T2212500[10.0 <= data$T2212500 & data$T2212500 <= 14.0] <- 10.0
  data$T2212500[15.0 <= data$T2212500 & data$T2212500 <= 19.0] <- 15.0
  data$T2212500[20.0 <= data$T2212500 & data$T2212500 <= 24.0] <- 20.0
  data$T2212500[25.0 <= data$T2212500 & data$T2212500 <= 29.0] <- 25.0
  data$T2212500[30.0 <= data$T2212500 & data$T2212500 <= 34.0] <- 30.0
  data$T2212500[35.0 <= data$T2212500 & data$T2212500 <= 39.0] <- 35.0
  data$T2212500[40.0 <= data$T2212500 & data$T2212500 <= 44.0] <- 40.0
  data$T2212500[45.0 <= data$T2212500 & data$T2212500 <= 49.0] <- 45.0
  data$T2212500[50.0 <= data$T2212500 & data$T2212500 <= 9.9999999E7] <- 50.0
  data$T2212500 <- factor(data$T2212500,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$T2217800[10.0 <= data$T2217800 & data$T2217800 <= 999.0] <- 10.0
  data$T2217800 <- factor(data$T2217800,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T3106900[1.0 <= data$T3106900 & data$T3106900 <= 499.0] <- 1.0
  data$T3106900[500.0 <= data$T3106900 & data$T3106900 <= 999.0] <- 500.0
  data$T3106900[1000.0 <= data$T3106900 & data$T3106900 <= 1499.0] <- 1000.0
  data$T3106900[1500.0 <= data$T3106900 & data$T3106900 <= 1999.0] <- 1500.0
  data$T3106900[2000.0 <= data$T3106900 & data$T3106900 <= 2499.0] <- 2000.0
  data$T3106900[2500.0 <= data$T3106900 & data$T3106900 <= 2999.0] <- 2500.0
  data$T3106900[3000.0 <= data$T3106900 & data$T3106900 <= 3499.0] <- 3000.0
  data$T3106900[3500.0 <= data$T3106900 & data$T3106900 <= 3999.0] <- 3500.0
  data$T3106900[4000.0 <= data$T3106900 & data$T3106900 <= 4499.0] <- 4000.0
  data$T3106900[4500.0 <= data$T3106900 & data$T3106900 <= 4999.0] <- 4500.0
  data$T3106900[5000.0 <= data$T3106900 & data$T3106900 <= 9.9999999E7] <- 5000.0
  data$T3106900 <- factor(data$T3106900,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499",
                                   "500 TO 999",
                                   "1000 TO 1499",
                                   "1500 TO 1999",
                                   "2000 TO 2499",
                                   "2500 TO 2999",
                                   "3000 TO 3499",
                                   "3500 TO 3999",
                                   "4000 TO 4499",
                                   "4500 TO 4999",
                                   "5000 TO 99999999: 5000+"))
  data$T3107700[10.0 <= data$T3107700 & data$T3107700 <= 999.0] <- 10.0
  data$T3107700 <- factor(data$T3107700,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T3110400[1.0 <= data$T3110400 & data$T3110400 <= 4.0] <- 1.0
  data$T3110400[5.0 <= data$T3110400 & data$T3110400 <= 9.0] <- 5.0
  data$T3110400[10.0 <= data$T3110400 & data$T3110400 <= 14.0] <- 10.0
  data$T3110400[15.0 <= data$T3110400 & data$T3110400 <= 19.0] <- 15.0
  data$T3110400[20.0 <= data$T3110400 & data$T3110400 <= 24.0] <- 20.0
  data$T3110400[25.0 <= data$T3110400 & data$T3110400 <= 29.0] <- 25.0
  data$T3110400[30.0 <= data$T3110400 & data$T3110400 <= 34.0] <- 30.0
  data$T3110400[35.0 <= data$T3110400 & data$T3110400 <= 39.0] <- 35.0
  data$T3110400[40.0 <= data$T3110400 & data$T3110400 <= 44.0] <- 40.0
  data$T3110400[45.0 <= data$T3110400 & data$T3110400 <= 49.0] <- 45.0
  data$T3110400[50.0 <= data$T3110400 & data$T3110400 <= 9.9999999E7] <- 50.0
  data$T3110400 <- factor(data$T3110400,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$T3115800[10.0 <= data$T3115800 & data$T3115800 <= 999.0] <- 10.0
  data$T3115800 <- factor(data$T3115800,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T4110900[1.0 <= data$T4110900 & data$T4110900 <= 499.0] <- 1.0
  data$T4110900[500.0 <= data$T4110900 & data$T4110900 <= 999.0] <- 500.0
  data$T4110900[1000.0 <= data$T4110900 & data$T4110900 <= 1499.0] <- 1000.0
  data$T4110900[1500.0 <= data$T4110900 & data$T4110900 <= 1999.0] <- 1500.0
  data$T4110900[2000.0 <= data$T4110900 & data$T4110900 <= 2499.0] <- 2000.0
  data$T4110900[2500.0 <= data$T4110900 & data$T4110900 <= 2999.0] <- 2500.0
  data$T4110900[3000.0 <= data$T4110900 & data$T4110900 <= 3499.0] <- 3000.0
  data$T4110900[3500.0 <= data$T4110900 & data$T4110900 <= 3999.0] <- 3500.0
  data$T4110900[4000.0 <= data$T4110900 & data$T4110900 <= 4499.0] <- 4000.0
  data$T4110900[4500.0 <= data$T4110900 & data$T4110900 <= 4999.0] <- 4500.0
  data$T4110900[5000.0 <= data$T4110900 & data$T4110900 <= 9.9999999E7] <- 5000.0
  data$T4110900 <- factor(data$T4110900,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499",
                                   "500 TO 999",
                                   "1000 TO 1499",
                                   "1500 TO 1999",
                                   "2000 TO 2499",
                                   "2500 TO 2999",
                                   "3000 TO 3499",
                                   "3500 TO 3999",
                                   "4000 TO 4499",
                                   "4500 TO 4999",
                                   "5000 TO 99999999: 5000+"))
  data$T4112200[10.0 <= data$T4112200 & data$T4112200 <= 999.0] <- 10.0
  data$T4112200 <- factor(data$T4112200,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T4114900[1.0 <= data$T4114900 & data$T4114900 <= 4.0] <- 1.0
  data$T4114900[5.0 <= data$T4114900 & data$T4114900 <= 9.0] <- 5.0
  data$T4114900[10.0 <= data$T4114900 & data$T4114900 <= 14.0] <- 10.0
  data$T4114900[15.0 <= data$T4114900 & data$T4114900 <= 19.0] <- 15.0
  data$T4114900[20.0 <= data$T4114900 & data$T4114900 <= 24.0] <- 20.0
  data$T4114900[25.0 <= data$T4114900 & data$T4114900 <= 29.0] <- 25.0
  data$T4114900[30.0 <= data$T4114900 & data$T4114900 <= 34.0] <- 30.0
  data$T4114900[35.0 <= data$T4114900 & data$T4114900 <= 39.0] <- 35.0
  data$T4114900[40.0 <= data$T4114900 & data$T4114900 <= 44.0] <- 40.0
  data$T4114900[45.0 <= data$T4114900 & data$T4114900 <= 49.0] <- 45.0
  data$T4114900[50.0 <= data$T4114900 & data$T4114900 <= 9.9999999E7] <- 50.0
  data$T4114900 <- factor(data$T4114900,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$T4120300[10.0 <= data$T4120300 & data$T4120300 <= 999.0] <- 10.0
  data$T4120300 <- factor(data$T4120300,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T5019700[1.0 <= data$T5019700 & data$T5019700 <= 499.0] <- 1.0
  data$T5019700[500.0 <= data$T5019700 & data$T5019700 <= 999.0] <- 500.0
  data$T5019700[1000.0 <= data$T5019700 & data$T5019700 <= 1499.0] <- 1000.0
  data$T5019700[1500.0 <= data$T5019700 & data$T5019700 <= 1999.0] <- 1500.0
  data$T5019700[2000.0 <= data$T5019700 & data$T5019700 <= 2499.0] <- 2000.0
  data$T5019700[2500.0 <= data$T5019700 & data$T5019700 <= 2999.0] <- 2500.0
  data$T5019700[3000.0 <= data$T5019700 & data$T5019700 <= 3499.0] <- 3000.0
  data$T5019700[3500.0 <= data$T5019700 & data$T5019700 <= 3999.0] <- 3500.0
  data$T5019700[4000.0 <= data$T5019700 & data$T5019700 <= 4499.0] <- 4000.0
  data$T5019700[4500.0 <= data$T5019700 & data$T5019700 <= 4999.0] <- 4500.0
  data$T5019700[5000.0 <= data$T5019700 & data$T5019700 <= 9.9999999E7] <- 5000.0
  data$T5019700 <- factor(data$T5019700,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499: 0.01-4.99",
                                   "500 TO 999: 5.00-9.99",
                                   "1000 TO 1499: 10.00-14.99",
                                   "1500 TO 1999: 15.00-19.99",
                                   "2000 TO 2499: 20.00-24.99",
                                   "2500 TO 2999: 25.00-29.99",
                                   "3000 TO 3499: 30.00-34.99",
                                   "3500 TO 3999: 35.00-39.99",
                                   "4000 TO 4499: 40.00-44.99",
                                   "4500 TO 4999: 45.00-49.99",
                                   "5000 TO 99999999: 50.00+"))
  data$T5022500[10.0 <= data$T5022500 & data$T5022500 <= 999.0] <- 10.0
  data$T5022500 <- factor(data$T5022500,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T5026200[1.0 <= data$T5026200 & data$T5026200 <= 4.0] <- 1.0
  data$T5026200[5.0 <= data$T5026200 & data$T5026200 <= 9.0] <- 5.0
  data$T5026200[10.0 <= data$T5026200 & data$T5026200 <= 14.0] <- 10.0
  data$T5026200[15.0 <= data$T5026200 & data$T5026200 <= 19.0] <- 15.0
  data$T5026200[20.0 <= data$T5026200 & data$T5026200 <= 24.0] <- 20.0
  data$T5026200[25.0 <= data$T5026200 & data$T5026200 <= 29.0] <- 25.0
  data$T5026200[30.0 <= data$T5026200 & data$T5026200 <= 34.0] <- 30.0
  data$T5026200[35.0 <= data$T5026200 & data$T5026200 <= 39.0] <- 35.0
  data$T5026200[40.0 <= data$T5026200 & data$T5026200 <= 44.0] <- 40.0
  data$T5026200[45.0 <= data$T5026200 & data$T5026200 <= 49.0] <- 45.0
  data$T5026200[50.0 <= data$T5026200 & data$T5026200 <= 9.9999999E7] <- 50.0
  data$T5026200 <- factor(data$T5026200,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$T5031500[10.0 <= data$T5031500 & data$T5031500 <= 999.0] <- 10.0
  data$T5031500 <- factor(data$T5031500,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T5768600[1.0 <= data$T5768600 & data$T5768600 <= 499.0] <- 1.0
  data$T5768600[500.0 <= data$T5768600 & data$T5768600 <= 999.0] <- 500.0
  data$T5768600[1000.0 <= data$T5768600 & data$T5768600 <= 1499.0] <- 1000.0
  data$T5768600[1500.0 <= data$T5768600 & data$T5768600 <= 1999.0] <- 1500.0
  data$T5768600[2000.0 <= data$T5768600 & data$T5768600 <= 2499.0] <- 2000.0
  data$T5768600[2500.0 <= data$T5768600 & data$T5768600 <= 2999.0] <- 2500.0
  data$T5768600[3000.0 <= data$T5768600 & data$T5768600 <= 3499.0] <- 3000.0
  data$T5768600[3500.0 <= data$T5768600 & data$T5768600 <= 3999.0] <- 3500.0
  data$T5768600[4000.0 <= data$T5768600 & data$T5768600 <= 4499.0] <- 4000.0
  data$T5768600[4500.0 <= data$T5768600 & data$T5768600 <= 4999.0] <- 4500.0
  data$T5768600[5000.0 <= data$T5768600 & data$T5768600 <= 9.9999999E7] <- 5000.0
  data$T5768600 <- factor(data$T5768600,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499: .01-4.99",
                                   "500 TO 999: 5.00-9.99",
                                   "1000 TO 1499: 10.00-14.99",
                                   "1500 TO 1999: 15.00-19.99",
                                   "2000 TO 2499: 20.00-24.99",
                                   "2500 TO 2999: 25.00-29.99",
                                   "3000 TO 3499: 30.00-34.99",
                                   "3500 TO 3999: 35.00-39.99",
                                   "4000 TO 4499: 40.00-44.99",
                                   "4500 TO 4999: 45.00-49.99",
                                   "5000 TO 99999999: 50.00+"))
  data$T5770700[10.0 <= data$T5770700 & data$T5770700 <= 999.0] <- 10.0
  data$T5770700 <- factor(data$T5770700,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T5774300[1.0 <= data$T5774300 & data$T5774300 <= 4.0] <- 1.0
  data$T5774300[5.0 <= data$T5774300 & data$T5774300 <= 9.0] <- 5.0
  data$T5774300[10.0 <= data$T5774300 & data$T5774300 <= 14.0] <- 10.0
  data$T5774300[15.0 <= data$T5774300 & data$T5774300 <= 19.0] <- 15.0
  data$T5774300[20.0 <= data$T5774300 & data$T5774300 <= 24.0] <- 20.0
  data$T5774300[25.0 <= data$T5774300 & data$T5774300 <= 29.0] <- 25.0
  data$T5774300[30.0 <= data$T5774300 & data$T5774300 <= 34.0] <- 30.0
  data$T5774300[35.0 <= data$T5774300 & data$T5774300 <= 39.0] <- 35.0
  data$T5774300[40.0 <= data$T5774300 & data$T5774300 <= 44.0] <- 40.0
  data$T5774300[45.0 <= data$T5774300 & data$T5774300 <= 49.0] <- 45.0
  data$T5774300[50.0 <= data$T5774300 & data$T5774300 <= 9.9999999E7] <- 50.0
  data$T5774300 <- factor(data$T5774300,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$T5779700[10.0 <= data$T5779700 & data$T5779700 <= 999.0] <- 10.0
  data$T5779700 <- factor(data$T5779700,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T8215400[1.0 <= data$T8215400 & data$T8215400 <= 499.0] <- 1.0
  data$T8215400[500.0 <= data$T8215400 & data$T8215400 <= 999.0] <- 500.0
  data$T8215400[1000.0 <= data$T8215400 & data$T8215400 <= 1499.0] <- 1000.0
  data$T8215400[1500.0 <= data$T8215400 & data$T8215400 <= 1999.0] <- 1500.0
  data$T8215400[2000.0 <= data$T8215400 & data$T8215400 <= 2499.0] <- 2000.0
  data$T8215400[2500.0 <= data$T8215400 & data$T8215400 <= 2999.0] <- 2500.0
  data$T8215400[3000.0 <= data$T8215400 & data$T8215400 <= 3499.0] <- 3000.0
  data$T8215400[3500.0 <= data$T8215400 & data$T8215400 <= 3999.0] <- 3500.0
  data$T8215400[4000.0 <= data$T8215400 & data$T8215400 <= 4499.0] <- 4000.0
  data$T8215400[4500.0 <= data$T8215400 & data$T8215400 <= 4999.0] <- 4500.0
  data$T8215400[5000.0 <= data$T8215400 & data$T8215400 <= 1.0E8] <- 5000.0
  data$T8215400 <- factor(data$T8215400,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499: .01-4.99",
                                   "500 TO 999: 5.00-9.99",
                                   "1000 TO 1499: 10.00-14.99",
                                   "1500 TO 1999: 15.00-19.99",
                                   "2000 TO 2499: 20.00-24.99",
                                   "2500 TO 2999: 25.00-29.99",
                                   "3000 TO 3499: 30.00-34.99",
                                   "3500 TO 3999: 35.00-39.99",
                                   "4000 TO 4499: 40.00-44.99",
                                   "4500 TO 4999: 45.00-49.99",
                                   "5000 TO 100000000: 50.00+"))
  data$T8218600[10.0 <= data$T8218600 & data$T8218600 <= 999.0] <- 10.0
  data$T8218600 <- factor(data$T8218600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T8221500[1.0 <= data$T8221500 & data$T8221500 <= 4.0] <- 1.0
  data$T8221500[5.0 <= data$T8221500 & data$T8221500 <= 9.0] <- 5.0
  data$T8221500[10.0 <= data$T8221500 & data$T8221500 <= 14.0] <- 10.0
  data$T8221500[15.0 <= data$T8221500 & data$T8221500 <= 19.0] <- 15.0
  data$T8221500[20.0 <= data$T8221500 & data$T8221500 <= 24.0] <- 20.0
  data$T8221500[25.0 <= data$T8221500 & data$T8221500 <= 29.0] <- 25.0
  data$T8221500[30.0 <= data$T8221500 & data$T8221500 <= 34.0] <- 30.0
  data$T8221500[35.0 <= data$T8221500 & data$T8221500 <= 39.0] <- 35.0
  data$T8221500[40.0 <= data$T8221500 & data$T8221500 <= 44.0] <- 40.0
  data$T8221500[45.0 <= data$T8221500 & data$T8221500 <= 49.0] <- 45.0
  data$T8221500[50.0 <= data$T8221500 & data$T8221500 <= 9.9999999E7] <- 50.0
  data$T8221500 <- factor(data$T8221500,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$T8226800[10.0 <= data$T8226800 & data$T8226800 <= 999.0] <- 10.0
  data$T8226800 <- factor(data$T8226800,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T8785100[1.0 <= data$T8785100 & data$T8785100 <= 499.0] <- 1.0
  data$T8785100[500.0 <= data$T8785100 & data$T8785100 <= 999.0] <- 500.0
  data$T8785100[1000.0 <= data$T8785100 & data$T8785100 <= 1499.0] <- 1000.0
  data$T8785100[1500.0 <= data$T8785100 & data$T8785100 <= 1999.0] <- 1500.0
  data$T8785100[2000.0 <= data$T8785100 & data$T8785100 <= 2499.0] <- 2000.0
  data$T8785100[2500.0 <= data$T8785100 & data$T8785100 <= 2999.0] <- 2500.0
  data$T8785100[3000.0 <= data$T8785100 & data$T8785100 <= 3499.0] <- 3000.0
  data$T8785100[3500.0 <= data$T8785100 & data$T8785100 <= 3999.0] <- 3500.0
  data$T8785100[4000.0 <= data$T8785100 & data$T8785100 <= 4499.0] <- 4000.0
  data$T8785100[4500.0 <= data$T8785100 & data$T8785100 <= 4999.0] <- 4500.0
  data$T8785100[5000.0 <= data$T8785100 & data$T8785100 <= 9.9999999E7] <- 5000.0
  data$T8785100 <- factor(data$T8785100,
                          levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
                          labels=c("0",
                                   "1 TO 499: .01-4.99",
                                   "500 TO 999: 5.00-9.99",
                                   "1000 TO 1499: 10.00-14.99",
                                   "1500 TO 1999: 15.00-19.99",
                                   "2000 TO 2499: 20.00-24.99",
                                   "2500 TO 2999: 25.00-29.99",
                                   "3000 TO 3499: 30.00-34.99",
                                   "3500 TO 3999: 35.00-39.99",
                                   "4000 TO 4499: 40.00-44.99",
                                   "4500 TO 4999: 45.00-49.99",
                                   "5000 TO 99999999: 50.00+"))
  data$T8787800[10.0 <= data$T8787800 & data$T8787800 <= 999.0] <- 10.0
  data$T8787800 <- factor(data$T8787800,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T8790700[1.0 <= data$T8790700 & data$T8790700 <= 4.0] <- 1.0
  data$T8790700[5.0 <= data$T8790700 & data$T8790700 <= 9.0] <- 5.0
  data$T8790700[10.0 <= data$T8790700 & data$T8790700 <= 14.0] <- 10.0
  data$T8790700[15.0 <= data$T8790700 & data$T8790700 <= 19.0] <- 15.0
  data$T8790700[20.0 <= data$T8790700 & data$T8790700 <= 24.0] <- 20.0
  data$T8790700[25.0 <= data$T8790700 & data$T8790700 <= 29.0] <- 25.0
  data$T8790700[30.0 <= data$T8790700 & data$T8790700 <= 34.0] <- 30.0
  data$T8790700[35.0 <= data$T8790700 & data$T8790700 <= 39.0] <- 35.0
  data$T8790700[40.0 <= data$T8790700 & data$T8790700 <= 44.0] <- 40.0
  data$T8790700[45.0 <= data$T8790700 & data$T8790700 <= 49.0] <- 45.0
  data$T8790700[50.0 <= data$T8790700 & data$T8790700 <= 9.9999999E7] <- 50.0
  data$T8790700 <- factor(data$T8790700,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$T8796100[10.0 <= data$T8796100 & data$T8796100 <= 999.0] <- 10.0
  data$T8796100 <- factor(data$T8796100,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T9299600[10.0 <= data$T9299600 & data$T9299600 <= 999.0] <- 10.0
  data$T9299600 <- factor(data$T9299600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T9302500[1.0 <= data$T9302500 & data$T9302500 <= 4.0] <- 1.0
  data$T9302500[5.0 <= data$T9302500 & data$T9302500 <= 9.0] <- 5.0
  data$T9302500[10.0 <= data$T9302500 & data$T9302500 <= 14.0] <- 10.0
  data$T9302500[15.0 <= data$T9302500 & data$T9302500 <= 19.0] <- 15.0
  data$T9302500[20.0 <= data$T9302500 & data$T9302500 <= 24.0] <- 20.0
  data$T9302500[25.0 <= data$T9302500 & data$T9302500 <= 29.0] <- 25.0
  data$T9302500[30.0 <= data$T9302500 & data$T9302500 <= 34.0] <- 30.0
  data$T9302500[35.0 <= data$T9302500 & data$T9302500 <= 39.0] <- 35.0
  data$T9302500[40.0 <= data$T9302500 & data$T9302500 <= 44.0] <- 40.0
  data$T9302500[45.0 <= data$T9302500 & data$T9302500 <= 49.0] <- 45.0
  data$T9302500[50.0 <= data$T9302500 & data$T9302500 <= 9.9999999E7] <- 50.0
  data$T9302500 <- factor(data$T9302500,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 99999999: 50+"))
  data$T9307900[10.0 <= data$T9307900 & data$T9307900 <= 999.0] <- 10.0
  data$T9307900 <- factor(data$T9307900,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  return(data)
}

varlabels <- c("ID# (1-12686) 79",
               "USUAL SHIFT @ M-RCNT JOB 79",
               "SAMPLE ID  79 INT",
               "RACL/ETHNIC COHORT /SCRNR 79",
               "SEX OF R 79",
               "USUAL SHIFT @ M-RCNT JOB 80",
               "USUAL SHIFT @ M-RCNT JOB 81",
               "USUAL SHIFT @ M-RCNT JOB 83",
               "USUAL SHIFT @ M-RCNT JOB 84",
               "USUAL SHIFT @ M-RCNT JOB 85",
               "TIME USUALLY BGN CUR/M-RCNT JOB 86",
               "TIME USUALLY BGN CUR/M-RCNT JOB 87",
               "TIME USUALLY BGN CUR/M-RCNT JOB 88",
               "USUAL SHIFT @ M-RCNT JOB 88",
               "TIME USUALLY BGN CUR/M-RCNT JOB 89",
               "USUAL SHIFT @ M-RCNT JOB 89",
               "SHIFT @ CUR/M-RCNT JOB 90",
               "TIME USUALLY BGN CUR/M-RCNT JOB 90",
               "DOES HLTH LIMIT KIND WRK R CAN DO 90",
               "HRP JOB#1 90",
               "FAMILY SIZE 90",
               "MARITAL STATUS 90",
               "DATE OF INTERVIEW STATUS - EMPLOYED",
               "# DIFFERENT JOBS EVER REPORTED 90",
               "# OF BIO/STEP/ADPT CHILDREN IN HH 90",
               "SHIFT @ CUR/M-RCNT JOB 91",
               "TIME USUALLY BGN CUR/M-RCNT JOB 91",
               "DOES HLTH LIMIT KIND WRK R CAN DO 91",
               "HRP JOB#1 91",
               "FAMILY SIZE 91",
               "MARITAL STATUS 91",
               "DATE OF INTERVIEW STATUS - EMPLOYED",
               "# DIFFERENT JOBS EVER REPORTED 91",
               "# OF BIO/STEP/ADPT CHILDREN IN HH 91",
               "SHIFT @ CUR/M-RCNT JOB 92",
               "TIME USUALLY BGN CUR/M-RCNT JOB 92",
               "DOES HLTH LIMIT KIND WRK R CAN DO 92",
               "HRP JOB#1 92",
               "FAMILY SIZE 92",
               "MARITAL STATUS 92",
               "DATE OF INTERVIEW STATUS - EMPLOYED",
               "# DIFFERENT JOBS EVER REPORTED 92",
               "# OF BIO/STEP/ADPT CHILDREN IN HH 92",
               "R'S SHIFT AT CURR JOB/MOST RCNT JOB 93",
               "TIME R BEG WORK LAST WK/MOST RCNT WK 93",
               "HEALTH LIMITS KIND OF WRK R COULD DO 93",
               "HRLY RATE OF PAY JOB#1 93",
               "FAMILY SIZE 93",
               "MARITAL STATUS 93",
               "DATE OF INTERVIEW STATUS - EMPLOYED",
               "NUM JOBS EVER REPORTED 93",
               "# OF BIO/STEP/ADPT CHILDREN IN HH 93",
               "R'S SHIFT 94",
               "HEALTH LIMITS KIND OF WRK R COULD DO 94",
               "HRLY RATE OF PAY JOB# 1 94",
               "FAMILY SIZE 94",
               "MARITAL STATUS 94",
               "DATE OF INTERVIEW STATUS - EMPLOYED",
               "NUM JOBS EVER REPORTED 94",
               "NUMBER OF BIO/STEP/ADPT CHILDRN IN HH 94",
               "HRLY RATE OF PAY JOB #1 96",
               "FAMILY SIZE 96",
               "MARITAL STATUS 96",
               "DATE OF INTERVIEW STATUS - EMPLOYED",
               "# JOBS EVER REPORTED 96",
               "NUMBER OF BIO/STEP/ADPT CHILDRN IN HH 96",
               "R'S SHIFT JOB #1 96",
               "HEALTH LIMITS KIND OF WRK R COULD DO 96",
               "RS SHIFT L1 1998",
               "HEALTH LIMITS KIND OF WRK COULD DO 1998",
               "HRLY RATE OF PAY JOB #01 1998",
               "FAMILY SIZE 1998",
               "MARITAL STATUS 1998",
               "DATE OF INTERVIEW STATUS - EMPLOYED 1998",
               "NUM JOBS EVER REPORTED 1998",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 1998",
               "RS SHIFT L1 2000",
               "HEALTH LIMITS KIND OF WRK COULD DO 2000",
               "HRLY RATE OF PAY JOB #01 2000",
               "FAMILY SIZE 2000",
               "MARITAL STATUS 2000",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2000",
               "NUM JOBS EVER REPORTED 2000",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2000",
               "WORK REG DAYTIME SHIFT OR OTH L1 2002",
               "RS SHIFT L1 2002",
               "HEALTH LIMITS KIND OF WRK COULD DO 2002",
               "HRLY RATE OF PAY JOB# 1 2002",
               "FAMILY SIZE 2002",
               "MARITAL STATUS 2002",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2002",
               "NUM JOBS EVER REPORTED 2002",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2002",
               "WORK REG DAYTIME SHIFT OR OTH L1 2004",
               "RS SHIFT L1 2004",
               "HEALTH LIMITS KIND OF WRK COULD DO 2004",
               "HRLY RATE OF PAY JOB# 1 2004",
               "FAMILY SIZE 2004",
               "MARITAL STATUS 2004",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2004",
               "NUM JOBS EVER REPORTED 2004",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2004",
               "WORK REG DAYTIME SHIFT OR OTH L1 2006",
               "RS SHIFT L1 2006",
               "HEALTH LIMITS KIND OF WRK COULD DO 2006",
               "HRLY RATE OF PAY JOB #01 2006",
               "FAMILY SIZE 2006",
               "MARITAL STATUS 2006",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2006",
               "NUM JOBS EVER REPORTED 2006",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2006",
               "WORK REG DAYTIME SHIFT OR OTH L1 2008",
               "RS SHIFT L1 2008",
               "HEALTH LIMITS KIND OF WRK COULD DO 2008",
               "HRLY RATE OF PAY JOB# 1 2008",
               "FAMILY SIZE 2008",
               "MARITAL STATUS 2008",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2008",
               "NUM JOBS EVER REPORTED 2008",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2008",
               "WORK REG DAYTIME SHIFT OR OTH L1 2010",
               "RS SHIFT L1 2010",
               "HEALTH LIMITS KIND OF WRK COULD DO 2010",
               "HRLY RATE OF PAY JOB# 1 2010",
               "FAMILY SIZE 2010",
               "MARITAL STATUS 2010",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2010",
               "NUM JOBS EVER REPORTED 2010",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2010",
               "WORK REG DAYTIME SHIFT OR OTH L1 2012",
               "RS SHIFT L1 2012",
               "HEALTH LIMITS KIND OF WRK COULD DO 2012",
               "HRLY RATE OF PAY JOB# 1 2012",
               "FAMILY SIZE 2012",
               "MARITAL STATUS 2012",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2012",
               "NUM JOBS EVER REPORTED 2012",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2012",
               "WORK REG DAYTIME SHIFT OR OTH L1 2014",
               "RS SHIFT L1 2014",
               "HEALTH LIMITS KIND OF WRK COULD DO 2014",
               "HRLY RATE OF PAY JOB# 1 2014",
               "FAMILY SIZE 2014",
               "MARITAL STATUS 2014",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2014",
               "NUM JOBS EVER REPORTED 2014",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2014",
               "WORK REG DAYTIME SHIFT OR OTH L1 2016",
               "RS SHIFT L1 2016",
               "HEALTH LIMITS KIND OF WRK COULD DO 2016",
               "HRLY RATE OF PAY JOB# 1 2016",
               "FAMILY SIZE 2016",
               "MARITAL STATUS 2016",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2016",
               "NUM JOBS EVER REPORTED 2016",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2016",
               "WORK REG DAYTIME SHIFT OR OTH L1 2018",
               "RS SHIFT L1 2018",
               "HEALTH LIMITS KIND OF WRK COULD DO 2018",
               "HRLY RATE OF PAY JOB# 1 2018",
               "FAMILY SIZE 2018",
               "MARITAL STATUS 2018",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2018",
               "NUM JOBS EVER REPORTED 2018",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2018",
               "WORK REG DAYTIME SHIFT OR OTH L1 2020",
               "RS SHIFT L1 2020",
               "HEALTH LIMITS KIND OF WRK COULD DO 2020",
               "HRLY RATE OF PAY JOB# 1 2020",
               "FAMILY SIZE 2020",
               "MARITAL STATUS 2020",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2020",
               "NUM JOBS EVER REPORTED 2020",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2020",
               "WORK REG DAYTIME SHIFT OR OTH L1 2022",
               "RS SHIFT L1 2022",
               "HEALTH LIMITS KIND OF WRK COULD DO 2022",
               "FAMILY SIZE 2022",
               "MARITAL STATUS 2022",
               "DATE OF INTERVIEW STATUS - EMPLOYED 2022",
               "NUM JOBS EVER REPORTED 2022",
               "NUM BIO/STEP/ADPT CHILDREN IN HH 2022",
               "HIGHEST GRADE EVER COMPLETED XRND"
)


# Use qnames rather than rnums

qnames = function(data) {
  names(data) <- c("CASEID_1979",
                   "CPS-QES-86A.01_1979",
                   "SAMPLE_ID_1979",
                   "SAMPLE_RACE_78SCRN",
                   "SAMPLE_SEX_1979",
                   "CPS-QES-86A.01_1980",
                   "CPS-QES-86A.01_1981",
                   "CPS-QES-86A.01_1983",
                   "CPS-QES-86A.01_1984",
                   "CPS-QES-86A.01_1985",
                   "CPS-QES-86C.01_1986",
                   "CPS-QES-86C.01_1987",
                   "CPS-QES-86C.01_1988",
                   "CPS-QES-86A.01_1988",
                   "CPS-QES-86C.01_1989",
                   "CPS-QES-86A.01_1989",
                   "CPS-QES-86A.01_1990",
                   "CPS-QES-86C.01_1990",
                   "Q11-4_1990",
                   "HRP1_1990",
                   "FAMSIZE_1990",
                   "MARSTAT-KEY_1990",
                   "DOI_EMPLOYED_1990",
                   "JOBSNUM_1990",
                   "NUMCH90_1990",
                   "CPS-QES-86A.01_1991",
                   "CPS-QES-86C.01_1991",
                   "Q11-4_1991",
                   "HRP1_1991",
                   "FAMSIZE_1991",
                   "MARSTAT-KEY_1991",
                   "DOI_EMPLOYED_1991",
                   "JOBSNUM_1991",
                   "NUMCH91_1991",
                   "CPS-QES-86A.01_1992",
                   "CPS-QES-86C.01_1992",
                   "Q11-4_1992",
                   "HRP1_1992",
                   "FAMSIZE_1992",
                   "MARSTAT-KEY_1992",
                   "DOI_EMPLOYED_1992",
                   "JOBSNUM_1992",
                   "NUMCH92_1992",
                   "CPS-QES-86A.01_1993",
                   "CPS-QES-86C.01_1993",
                   "Q11-4_1993",
                   "HRP1_1993",
                   "FAMSIZE_1993",
                   "MARSTAT-KEY_1993",
                   "DOI_EMPLOYED_1993",
                   "JOBSNUM_1993",
                   "NUMCH93_1993",
                   "QES-86A.01_1994",
                   "Q11-4_1994",
                   "HRP1_1994",
                   "FAMSIZE_1994",
                   "MARSTAT-KEY_1994",
                   "DOI_EMPLOYED_1994",
                   "JOBSNUM_1994",
                   "NUMCH94_1994",
                   "HRP1_1996",
                   "FAMSIZE_1996",
                   "MARSTAT-KEY_1996",
                   "DOI_EMPLOYED_1996",
                   "JOBSNUM_1996",
                   "NUMCH96_1996",
                   "QES-86A.01_1996",
                   "Q11-4_1996",
                   "QES-86A.01_1998",
                   "Q11-4_1998",
                   "HRP1_1998",
                   "FAMSIZE_1998",
                   "MARSTAT-KEY_1998",
                   "DOI_EMPLOYED_1998",
                   "JOBSNUM_1998",
                   "NUMCH98_1998",
                   "QES-86A.01_2000",
                   "Q11-4_2000",
                   "HRP1_2000",
                   "FAMSIZE_2000",
                   "MARSTAT-KEY_2000",
                   "DOI_EMPLOYED_2000",
                   "JOBSNUM_2000",
                   "NUMCH00_2000",
                   "QES-86A_0.01_2002",
                   "QES-86A.01_2002",
                   "Q11-4_2002",
                   "HRP1_2002",
                   "FAMSIZE_2002",
                   "MARSTAT-KEY_2002",
                   "DOI_EMPLOYED_2002",
                   "JOBSNUM_2002",
                   "NUMCH02_2002",
                   "QES-86A_0.01_2004",
                   "QES-86A.01_2004",
                   "Q11-4_2004",
                   "HRP1_2004",
                   "FAMSIZE_2004",
                   "MARSTAT-KEY_2004",
                   "DOI_EMPLOYED_2004",
                   "JOBSNUM_2004",
                   "NUMCH04_2004",
                   "QES-86A_0.01_2006",
                   "QES-86A.01_2006",
                   "Q11-4_2006",
                   "HRP1_2006",
                   "FAMSIZE_2006",
                   "MARSTAT-KEY_2006",
                   "DOI_EMPLOYED_2006",
                   "JOBSNUM_2006",
                   "NUMCH06_2006",
                   "QES-86A_0.01_2008",
                   "QES-86A.01_2008",
                   "Q11-4_2008",
                   "HRP1_2008",
                   "FAMSIZE_2008",
                   "MARSTAT-KEY_2008",
                   "DOI_EMPLOYED_2008",
                   "JOBSNUM_2008",
                   "NUMCH08_2008",
                   "QES-86A_0.01_2010",
                   "QES-86A.01_2010",
                   "Q11-4_2010",
                   "HRP1_2010",
                   "FAMSIZE_2010",
                   "MARSTAT-KEY_2010",
                   "DOI_EMPLOYED_2010",
                   "JOBSNUM_2010",
                   "NUMCH10_2010",
                   "QES-86A_0.01_2012",
                   "QES-86A.01_2012",
                   "Q11-4_2012",
                   "HRP1_2012",
                   "FAMSIZE_2012",
                   "MARSTAT-KEY_2012",
                   "DOI_EMPLOYED_2012",
                   "JOBSNUM_2012",
                   "NUMCH12_2012",
                   "QES-86A_0.01_2014",
                   "QES-86A.01_2014",
                   "Q11-4_2014",
                   "HRP1_2014",
                   "FAMSIZE_2014",
                   "MARSTAT-KEY_2014",
                   "DOI_EMPLOYED_2014",
                   "JOBSNUM_2014",
                   "NUMCH14_2014",
                   "QES-86A_0.01_2016",
                   "QES-86A.01_2016",
                   "Q11-4_2016",
                   "HRP1_2016",
                   "FAMSIZE_2016",
                   "MARSTAT-KEY_2016",
                   "DOI_EMPLOYED_2016",
                   "JOBSNUM_2016",
                   "NUMCH16_2016",
                   "QES-86A_0.01_2018",
                   "QES-86A.01_2018",
                   "Q11-4_2018",
                   "HRP1_2018",
                   "FAMSIZE_2018",
                   "MARSTAT-KEY_2018",
                   "DOI_EMPLOYED_2018",
                   "JOBSNUM_2018",
                   "NUMCH18_2018",
                   "QES-86A_0.01_2020",
                   "QES-86A.01_2020",
                   "Q11-4_2020",
                   "HRP1_2020",
                   "FAMSIZE_2020",
                   "MARSTAT-KEY_2020",
                   "DOI_EMPLOYED_2020",
                   "JOBSNUM_2020",
                   "NUMCH20_2020",
                   "QES-86A_0.01_2022",
                   "QES-86A.01_2022",
                   "Q11-4_2022",
                   "FAMSIZE_2022",
                   "MARSTAT-KEY_2022",
                   "DOI_EMPLOYED_2022",
                   "JOBSNUM_2022",
                   "NUMCH22_2022",
                   "HGC_EVER_XRND")
  return(data)
}




# Assign to data
categories <- vallabels(new_data)
data <- qnames(new_data)
